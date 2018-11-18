use 5.010;
use strict;
use warnings;
use English qw( -no_match_vars );

# This code uses as its grammar reference the code in
# the arvo repo: https://github.com/urbit/arvo
# File sys/hoon.hoon: https://github.com/urbit/arvo/blob/master/sys/hoon.hoon
# as of commit 7dc3eb1cfacaaafd917697a544bdcf7f22e09eeb 

# Organization is by hoon.hoon (and Hoon Library) sections: 4a, 5d, etc.;
# and within that alphabetically by "face" name

# === DSL Piece: "Do not edit" warning ===
my $dsl_warning = <<END_OF_WARNING;
# !!! DO NOT EDIT !!!
# This code automatically written by $PROGRAM_NAME

END_OF_WARNING

# === DSL Piece: the main body ===

# The "FIXED:" comments lines are descriptons of the fixed length runes
# (1-fixed, 2-fixed, 3-fixed and 4-fixed) for auto-generation
# of Marpa rules for the various regular formats, both
# tall and wide.
#
# The format is
#
#   rune type1 type2 ...

my $dsl_main = <<'END_OF_DSL_MAIN';
use 5.010;
use strict;
use warnings;

use Data::Dumper;
use Marpa::R2 4.000;

package MarpaX::YAHC;

use English qw( -no_match_vars );

my $dsl = do { $RS = undef; <DATA> };

my $grammar = Marpa::R2::Scanless::G->new( { source => \$dsl } );

sub divergence {
    die join '', 'Unrecoverable internal error: ', @_;
}

# Given a target, an input and an offset into that input,
# it reads using that recognizer.  The return values
# are the parse value and a new offset in the input.
# Errors are thrown.

sub getValue {
    my ( $target, $input, $offset ) = @_;
    my $input_length = length ${$input};
    my $resume_pos;
    my $this_pos;

    my $nextNL = index ${$input}, "\n", $offset;
    if ($nextNL < 0) {
      die join '', 'Newline missing after triple quotes: "', ${$input}, '"'
    }
    my $initiator = substr ${$input}, $offset, $nextNL-$offset;
    if ($initiator ne "'''" and $initiator !~ m/^''' *::/) {
      die join '', 'Disallowed characters after initial triple quotes: "', $initiator, '"'
    }

    pos ${$input} = $offset;
    my ($indent) = ${$input} =~ /\G( *)[^ ]/g;
    my $terminator = $indent . "'''";

    my $terminatorPos = index ${$input}, $terminator, $offset;
    my $value = substr ${$input}, $nextNL+1, ($terminatorPos - $nextNL - 1);

    say STDERR "Left main READ loop" if $MarpaX::YAHC::DEBUG;

    # Return ref to value and new offset
    return \$value, $terminatorPos + length $terminator;
}

sub parse {
    my ($input) = @_;
    my $debug = $MarpaX::YAHC::DEBUG;
    my $recce = Marpa::R2::Scanless::R->new(
        {
            grammar         => $grammar,
            ranking_method  => 'high_rule_only',
            trace_lexers    => ( $debug ? 1 : 0 ),
            trace_terminals => ( $debug ? 1 : 0 ),
        }
    );

    my $input_length = length ${$input};
    my $this_pos;
    my $ok = eval { $this_pos = $recce->read( $input ) ; 1; };
    if (not $ok) {
       say STDERR $recce->show_progress(0, -1) if $debug;
       die $EVAL_ERROR;
    }

    # The main read loop.  Read starting at $offset.
    # If interrupted execute the handler logic,
    # and, possibly, resume.
    say STDERR "this_pos=$this_pos ; input_length=$input_length" if $debug;

  READ:
    while ( $this_pos < $input_length ) {

	my $resume_pos;

        # Only one event at a time is expected -- more
        # than one is an error.  No event means parsing
        # is exhausted.

        my $events      = $recce->events();
        my $event_count = scalar @{$events};
        if ( $event_count < 0 ) {
            last READ;
        }
        if ( $event_count != 1 ) {
            divergence("One event expected, instead got $event_count");
        }

        # Find the event name

        my $event = $events->[0];
        my $name  = $event->[0];

        if ( $name eq 'tripleQuote' ) {
            say STDERR "$name event" if $MarpaX::YAHC::DEBUG;
            my $value_ref;
            ( $value_ref, $resume_pos ) = getValue( $name, $input, $this_pos );
            my $result = $recce->lexeme_read(
                'TRIPLE QUOTE STRING',
                $this_pos,
                ( length ${$value_ref} ),
                [ ${$value_ref} ]
            );
            say STDERR "lexeme_read('TRIPLE QUOTE STRING',...) returned ",
              Data::Dumper::Dumper( \$result )
              if $MarpaX::YAHC::DEBUG;
        }

	if (not $resume_pos) {
	  die "read() ended prematurely\n",
	    "  input length = $input_length\n",
	    "  length read = $this_pos\n",
	    "  the cause may be an unexpected event";
	}

	say STDERR "this_pos=$this_pos ; input_length=$input_length" if $debug;

        my $ok = eval { $this_pos = $recce->resume($resume_pos); 1; };
        if ( not $ok ) {
            say STDERR $recce->show_progress( 0, -1 ) if $debug;
            die $EVAL_ERROR;
        }

    }

    if ( 0 ) {
    # if ( $recce->ambiguity_metric() > 1 ) {

        # The calls in this section are experimental as of Marpa::R2 2.090
        my $asf = Marpa::R2::ASF->new( { slr => $recce } );
        say STDERR 'No ASF' if not defined $asf;
        my $ambiguities = Marpa::R2::Internal::ASF::ambiguities($asf);
        my @ambiguities = grep { defined } @{$ambiguities}[ 0 .. 1 ];
        die
          "Parse of BNF/Scanless source is ambiguous\n",
          Marpa::R2::Internal::ASF::ambiguities_show( $asf, \@ambiguities );
    } ## end if ( $recce->ambiguity_metric() > 1 )

    my $valueRef = $recce->value();
    if ( !$valueRef ) {
        die "input read, but there was no parse";
    }

    return $valueRef;
}

# Takes one argument and returns a ref to an array of acceptable
# nodes.  The array may be empty.  All scalars are acceptable
# leaf nodes.  Acceptable interior nodes have length at least 1
# and contain a Haskell Standard symbol name, followed by zero or
# more acceptable nodes.
sub prune {
    my ($v) = @_;

    state $deleteIfEmpty = {
        optKets => 1,
    };

    state $nonSemantic = {
        doubleStringElements => 1,
        wedeFirst            => 1,
        wideHoon             => 1,
        wideHoonJogging      => 1,
        wideHoonJogs         => 1,
        wideHoonJog          => 1,
        wideHoonSeq          => 1,
        wideMold             => 1,
        wideMoldSeq          => 1,
        hoon                 => 1,
        hoonExpression       => 1,
        hoonFile             => 1,
        hoonJog              => 1,
        hoonJogging          => 1,
        hoonJogs             => 1,
        hoonJog              => 1,
        hoonPrimary          => 1,
        hoonSeq              => 1,
        hoonUnary            => 1,
	long5dWide => 1,
        mold                 => 1,
        moldSeq              => 1,
        norm5d               => 1,
        norm5dWide          => 1,
        rope5d               => 1,
        scat5d               => 1,
    };

    return [] if not defined $v;
    my $reftype = ref $v;
    return [$v] if not $reftype; # An acceptable leaf node
    return prune($$v) if $reftype eq 'REF';
    divergence("Tree node has reftype $reftype") if $reftype ne 'ARRAY';
    my @source = grep { defined } @{$v};
    my $element_count = scalar @source;
    return [] if $element_count <= 0; # must have at least one element
    my $name = shift @source;
    my $nameReftype = ref $name;
    # divergence("Tree node name has reftype $nameReftype") if $nameReftype;
    if ($nameReftype) {
      my @result = ();
      ELEMENT:for my $element ($name, @source) {
	if (ref $element eq 'ARRAY') {
	  push @result, grep { defined }
		  map { @{$_}; }
		  map { prune($_); }
		  @{$element}
		;
	  next ELEMENT;
	}
	push @result, $_;
      }
      return [@result];
    }
    if (defined $deleteIfEmpty->{$name} and $element_count == 1) {
      return [];
    }
    if (defined $nonSemantic->{$name}) {
      # Not an acceptable branch node, but (hopefully)
      # its children are acceptable
      return [ grep { defined }
	      map { @{$_}; }
	      map { prune($_); }
	      @source
	    ];
    }

    # An acceptable branch node
    my @result = ($name);
    push @result, grep { defined }
	    map { @{$_}; }
	    map { prune($_); }
	    @source;
    return [\@result];
}

1;

__DATA__
# start and length will be needed for production
# :default ::= action => [name,start,length,values]
:default ::= action => [name,values]
lexeme default = latm => 1

# === CHARACTER SET ===

# Unicorn is a non-existence character used for various
# tricks: error rules, TODO rules, inaccessbile symbols,
# etc.
UNICORN ~ unicorn
unicorn ~ [^\d\D]

# === Hoon library: 4i ===

dog4i ~ dot4h gay4i

gay4i ~ # empty
gay4i ~ gap4k

# vul4i ~ '::' optNonNLs nl

# === Hoon library: 4j ===

# Two hex numbers
bix4j ~ six4j six4j

MOT4J ~ mot4j
mot4j ~ [12] sid4j
mot4j ~ sed4j

dum4j ~ sid4j+

QIX4J ~ qix4j
qix4j ~ six4j six4j six4j six4j

DIM4J ~ dim4j # a natural number
dim4j ~ '0'
dim4j ~ dip4j

DIP4J ~ dip4j
dip4j ~ [1-9] dip4jRest
dip4jRest ~ [0-9]*

VUM4J ~ vum4j
vum4j ~ siv4j+

sed4j ~ [1-9]
sid4j ~ [0-9]
# hexadecimal digit
six4j ~ [0-9a-f]
siv4j ~ [0-9a-v]

# === Hoon library: 4k ===

SYM4K ~ sym4k
CEN_SYM4K ~ cen4h sym4k
sym4k ~ low4k sym4kRest
low4k ~ [a-z]
hig4k ~ [A-Z]
nud4k ~ [0-9]
sym4kRest ~ # empty
sym4kRest ~ sym4kRestChars
sym4kRestChars ~ sym4kRestChar+
sym4kRestChar ~ low4k | nud4k | hep4h

VEN4K ~ ven4k
ven4k ~ carCdr
ven4k ~ carCdrPairs
ven4k ~ carCdrPairs carCdr
carCdrPairs ~ carCdrPair+
carCdrPair ~ [-+][<>]
carCdr ~ [-+]

qit4k ::= <UNESCAPED SINGLE QUOTE CHARS>
qit4k ::= EscapedSingleQuoteChar
<UNESCAPED SINGLE QUOTE CHARS> ~ unescapedSingleQuoteChar+

# LATER Single string element also allow escapes
# LATER: Add \xx hex escapes, and more backslash escapes
qut4k ::= <single quote string>
<single quote string> ::= ([']) <single quote cord> (['])
<single quote cord> ::= qit4k* separator=>gon4k proper=>1

# <TRIPLE QUOTE START> triggers an event -- the quoted
# string is actually supplies as <TRIPLE QUOTE STRING>.
qut4k ::= <TRIPLE QUOTE START>
qut4k ::= <TRIPLE QUOTE STRING>
:lexeme ~ <TRIPLE QUOTE START> event=>tripleQuote pause=>before
<TRIPLE QUOTE START> ~ ['] ['] [']
<TRIPLE QUOTE STRING> ~ unicorn # implemented with a combinator

# All the printable (non-control) characters except
# bas and soq
unescapedSingleQuoteChar ~ [\x20-\x26\x28-\x5b\x5d-\x7e\x80-\xff]
EscapedSingleQuoteChar ::= (BAS4H) BAS4H | (BAS4H) SOQ4H
EscapedSingleQuoteChar ::= AsciiHexChar
AsciiHexChar ::= (BAS4H) MES4K

dem4k ::= DIT4K_SEQ+ separator=>gon4k proper=>1

DIT4K_SEQ ~ dit4kSeq
dit4kSeq ~ dit4k+
dit4k ~ [0-9]

MES4K ~ mes4k
mes4k ~ hit4k hit4k

# HIT4K ~ hit4k
hit4k ~ dit4k
hit4k ~ [a-fA-F]

gon4k ~ bas4h gay4i fas4h

# === Hoon library: 4l ===

# TODO: crub(4l) is incomplete

crub4l ::= date
crub4l ::= timePeriod

date ::= date_part1
date ::= date_part1 DOT DOT date_part2
date ::= date_part1 DOT DOT date_part2 DOT DOT date_part3
date_part1 ::= DIM4J optHep DOT MOT4J DOT DIP4J
optHep ::= # empty
optHep ::= HEP
date_part2 ::= dum4j DOT dum4j DOT dum4j
date_part3 ::= qix4jSeq

qix4jSeq ::= QIX4J+ separator=>DOT proper=>1

timePeriod ::= timePeriodKernel timePeriodFraction
timePeriod ::= timePeriodKernel
timePeriodKernel ::= timePeriodByUnit+ separator=>DOT proper=>1
timePeriodByUnit ::= timePeriodDays
timePeriodByUnit ::= timePeriodHours
timePeriodByUnit ::= timePeriodMinutes
timePeriodByUnit ::= timePeriodSeconds
timePeriodDays ::= LAPSE_DAYS
LAPSE_DAYS ~ 'd' dim4j
timePeriodHours ::= LAPSE_HOURS
LAPSE_HOURS ~ 'h' dim4j
timePeriodMinutes ::= LAPSE_MINUTES
LAPSE_MINUTES ~ 'm' dim4j
timePeriodSeconds ::= LAPSE_SECONDS
LAPSE_SECONDS ~ 's' dim4j
timePeriodFraction ::= (DOT DOT) qix4jSeq

# nuck(4l) is the coin parser
# TODO: Finish nuck4l
nuck4l ::= SYM4K

# tash(4l) is the signed dime parser
nuck4l ::= TODO_tash4l
TODO_tash4l ::= UNICORN

# perd(4l) parses dimes or tuples without their standard prefixes
nuck4l ::= DOT4H perd4l

# Can be either '$~' or '%~'
# -- both seem to have the same semantics
nuck4l ::= moldNullSig
moldNullSig ::= '~'

# perd(4l) parses sig-prefixed coins after the sig prefix
nuck4l ::= SIG twid4l

# TODO: Finish perd4l
perd4l ::= zust4l

# TODO: Finish zust4l
zust4l ::= 'y'
zust4l ::= 'n'

twid4l ::= '0' VUM4J
twid4l ::= crub4l

nuck4l ::= bisk4l
# bisk(4l) parses unsigned dimes of any base
bisk4l ::= NUMBER

#     :~  :-  ['a' 'z']  (cook |=(a/@ta [%$ %tas a]) sym)
#         :-  ['0' '9']  (stag %$ bisk)
#         :-  '-'        (stag %$ tash)
#         :-  '.'        ;~(pfix dot perd)
#         :-  '~'        ;~(pfix sig ;~(pose twid (easy [%$ %n 0])))

# === Hoon library: 5d ===

bonk5d ::= CEN4H SYM4K COL4H SYM4K DOT4H DOT4H dem4k
bonk5d ::= CEN4H SYM4K COL4H SYM4K DOT4H dem4k
bonk5d ::= CEN4H SYM4K DOT4H dem4k
bonk5d ::= CEN4H SYM4K

bont5d ::= CEN4H SYM4K ([.] GAP) hoon
bont5d ::= wideBont5d
wideBont5d ::= CEN4H SYM4K ([.]) wideHoon
wideBont5d ::= CEN4H SYM4K ([.] ACE) wideHoon

# Lexemes cannot be empty so empty
# aura name must be special cased.
auraName ::= # empty
auraName ::= AURA_NAME
AURA_NAME ~ optLow4kSeq optHig4kSeq
optLow4kSeq ~ low4k*
optHig4kSeq ~ hig4k*

# === Hoon library: 5d, molds ===

mold ::= wideMold
moldSeq ::= mold+ separator=>GAP proper=>1
wideMoldSeq ::= wideMold+ separator=>ACE proper=>1

wede5d ::= (FAS) wideHoon
wede5d ::= (LUS) wideHoon

# Implementing long(5d)

long5dWide ::= infixTis
long5dWide ::= infixCol
long5dWide ::= infixKet
long5dWide ::= infixFas
long5dWide ::= circumScatParen
long5dWide ::= scat5d

infixTis ::= scat5d (TIS) wideHoon rank=>60
infixCol ::= scat5d (COL) wideHoon rank=>50
infixKet ::= scat5d (KET) wideHoon rank=>40
infixFas ::= scat5d (FAS) wideHoon rank=>30
circumScatParen ::= scat5d (PEL) lobo5d (PER) rank=>20

lobo5d ::= wideHoonJogs
wideHoonJogs ::= wideHoonJog+ separator=>wideHoonJoggingSeparator proper=>1
wideHoonJog ::= rope5d (ACE) wideHoon
wideHoonJoggingSeparator ::= COM ACE

# Implementing rump(5d)

rump5d ::= rope5d
rump5d ::= rope5d wede5d

# Implementing rupl(5d)
# rupl(5d) seems to implement the hoon '[...]', ~[...], and [...]~
# syntaxes.

# TODO: Finish rupl(5d)
rupl5d ::= circumBracket
rupl5d ::= sigCircumBracket
rupl5d ::= circumBracketSig
rupl5d ::= sigCircumBracketSig
# TODO: is the whitespace really what was intended?
circumBracket ::= ('[' ACE) hoonSeq (GAP ']')
circumBracket ::= ('[') wideHoonSeq (']')
sigCircumBracket ::= (SIG '[' ACE) hoonSeq (GAP ']')
sigCircumBracket ::= (SIG '[') wideHoonSeq (']')
circumBracketSig ::= ('[' ACE) hoonSeq (GAP ']' SIG)
circumBracketSig ::= ('[') wideHoonSeq (']' SIG)
sigCircumBracketSig ::= (SIG '[' ACE) hoonSeq (GAP ']' SIG)
sigCircumBracketSig ::= (SIG '[') wideHoonSeq (']' SIG)

# Implementing scad(5d)

# scad(5d) implements the irregular mold syntaxes

# Cases given in hoon.hoon order.  Unfortunately
# not the same as the order in scat(5d).

# '_'
# Same as scat(5d)
wideMold ::= moldPrefixCab
moldPrefixCab ::= ('_') wideHoon

# ','
# Differs from scat(5d)
wideMold ::= moldPrefixCom
moldPrefixCom ::= (',') wideHoon

# '$'
# Differs from scat(5d)
wideMold ::= moldBucbuc
moldBucbuc ::= '$$'

wideMold ::= moldBucpam
moldBucpam ::= '$&'

wideMold ::= moldBucbar
moldBucbar ::= '$|'

wideMold ::= moldBucSingleString
moldBucSingleString ::= '$' qut4k

wideMold ::= moldBucNuck4l
moldBucNuck4l ::= '$' nuck4l

wideMold ::= rump5d 

# '%'
# Differs from scat(5d)
wideMold ::= moldCenbuc
moldCenbuc ::= '%$'

wideMold ::= moldCenpam
moldCenpam ::= '%&'

wideMold ::= moldCenbar
moldCenbar ::= '%|'

wideMold ::= moldCenSingleString
moldCenSingleString ::= ('%') qut4k

wideMold ::= moldCenNuck4l
moldCenNuck4l ::= '%' nuck4l

# '('
# Differs from scat(5d)
wideMold ::= moldCircumParen
moldCircumParen ::= ('(') wideHoon (ACE) wideMoldSeq (')')
moldCircumParen ::= ('(') wideHoon (')')

# '{'
# Same as scat(5d)
wideMold ::= moldCircumBrace
moldCircumBrace ::= ('{') wideMoldSeq ('}')

# '['
# Differs from scat(5d)
wideMold ::= moldCircumBracket
moldCircumBracket ::= ('[') wideMoldSeq (']')

# '*'
# Subset of scat(5d)
wideMold ::= moldTar
moldTar ::= '*'

# '@'
# Same as scat(5d)
wideMold ::= moldAura
moldAura ::= '@' auraName

# '?'
# Same as scat(5d)
wideMold ::= moldPrefixWut
moldPrefixWut ::= ('?(') wideMoldSeq (')')

wideMold ::= moldWut
moldWut ::= '?'

# '~'
# Differs from scat(5d)
wideMold ::= moldSig
moldSig ::= '~'

# '^'
# Differs from scat(5d)
wideMold ::= moldKet
moldKet ~ '^'

# <moldInfixCol> can start with either KET (^) or lowercase char
# This is scab(5d)
# TODO: Delete after development
wideMold ::= moldInfixCol
moldInfixCol ::= rope5d [:] moldInfixCol2
moldInfixCol2 ::= rope5d+ separator=>[:] proper=>1

# '='
# Differs from scat(5d)
wideMold ::= moldPrefixTis
# TODO: Is this right?
moldPrefixTis ::= ('=') wideMold (')')

# ['a' 'z']
# Differs from scat(5d)
# for scab(5d), see the KET subcase
wideMold ::= moldInfixFas
wideMold ::= moldInfixTis
moldInfixFas ::= SYM4K FAS wideMold rank=>1
moldInfixTis ::= SYM4K TIS wideMold rank=>1

# End of scad(5d)

# === Implementing scat(5d) ===

# scat(5d) implements the irregular hoon syntaxes

# For convenience in referring back
# to hoon.hoon, I use scat(5d)'s order, as is.
# Unfortunately this is not in the same
# order as in scad.

# ','
# Differs from scad(5)
# TODO: Finish
wideBuccol ::= (',[') wideMoldSeq (']')

# '!'
# Not in scad(5)
# TODO: Finish
hoonUnary ::= prefixZap
prefixZap ::= (ZAP) wideHoon
scat5d ::= wideZapzap
wideZapzap ~ zap4h zap4h

# '_'
# Same as scad(5)
hoonUnary ::= prefixCab
prefixCab ::= (CAB) wideHoon

# '$'
# For rump, see subcase ['a' 'z']
# Differs from scad(5)
scat5d ::= bucBuc
scat5d ::= bucPam
scat5d ::= bucBar
scat5d ::= dollarTerm
bucBuc ::= BUC4H BUC4H
bucPam ::= BUC4H PAM4H
bucBar ::= BUC4H BAR4H
dollarTerm ::= BUC4H qut4k
dollarTerm ::= BUC4H nuck4l

# '%'
# Differs from scad(5)
scat5d ::= cenPath
scat5d ::= cenBuc
scat5d ::= cenPam
scat5d ::= cenBar
scat5d ::= cenTerm
scat5d ::= cenDirectories
cenPath ::= CEN4H porc5d
cenBuc ::= CEN4H BUC4H
cenPam ::= CEN4H PAM4H
cenBar ::= CEN4H BAR4H
cenTerm ::= CEN4H qut4k
cenTerm ::= CEN4H nuck4l
cenDirectories ::= CEN4H+

# '&'
# Not in scad(5)
# TODO: Finish
# For rope(5d), see subcase ['a' 'z'] and rump(5d)
scat5d ::= prefixPam
scat5d ::= pamPlusPrefix
scat5d ::= soloPam
prefixPam ::= (PAM4H '(') wideHoonSeq (')')
pamPlusPrefix ::= (PAM4H) wede5d
soloPam ::= PAM4H

# '\''
# Not in scad(5)
scat5d ::= singleQuoteString
singleQuoteString ::= qut4k

# '('
# Differs from scad(5)
# See https://raw.githubusercontent.com/urbit/old-urbit.org/master/doc/hoon/lan/irregular.markdown
# and cenhep in https://urbit.org/docs/hoon/irregular/
scat5d ::= circumParen1
scat5d ::= circumParen2
circumParen1 ::= ('(') wideHoon (')')
circumParen2 ::= ('(') wideHoon (ACE) wideHoonSeq (')')

# '{'
# Same as scad(5)
scat5d ::= circumBraces
circumBraces ::= ('{') wideMoldSeq ('}')

# '*'
# Superset of scad(5)
scat5d ::= prefixTar
scat5d ::= soloTar
prefixTar ::= TAR wideMold
soloTar ::= TAR

# '@'
# TODO: NYI
# Same as scad(5)
# '@'
# Same as scat(5d)
scat5d ::= aura
aura ::= '@' auraName

# '+'
# Not in scad(5)
# TODO: Finish
scat5d ::= irrDotlus
irrDotlus ::= ('+(') wideHoon (')')

# '-'
# TODO: NYI
# Not in scad(5)

# '.'
# TODO: NYI
# Not in scad(5)

# ['0' '9']
# Not in scad(5)
# This subcase handles infix expressions
# starting with a digit.
scat5d ::= bisk4l
scat5d ::= bisk4l wede5d

# ':'
# TODO: NYI
# Not in scad(5)

# '='
# Differs from scad(5)
tallDottis ::= (TIS GAP) hoon
scat5d ::= irrDottis
irrDottis ::= ('=(') wideHoon (ACE) wideHoon (')')
irrDottis ::= ('=(') wideHoon (')')

# '?'
# Same as scad(5)
# TODO: Finish
scat5d ::= circumWutParen
scat5d ::= soloWut
circumWutParen ::= (WUT PEL) wideMoldSeq (PER)
soloWut ::= WUT

# '['
# Differs from scad(5)
scat5d ::= rupl5d

# '^'
# Differs from scad(5)
# For rope(5d) see ['a' 'z'] subcase and rump(5d)
scat5d ::= soloKet
soloKet ::= KET

# '`'
# Not in scad(5)
# TODO: Finish
scat5d ::= irrKethep
irrKethep ::= ('`') mold ('`') hoon
scat5d ::= prefixKet
prefixKet ::= ('`') wideHoon

# '"'
# Not in scad(5)
scat5d ::= infixDot
infixDot ::= soil5d+ separator=>dog4i proper=>1

# ['a' 'z']
# Differs from scad(5)
scat5d ::= rump5d

# '|'
# Not in scad(5)
# TODO: Finish
scat5d ::= prefixBar
scat5d ::= circumBarParen
scat5d ::= soloBar
prefixBar ::= (BAR4H) wede5d rank=>1
circumBarParen ::= (BAR4H PEL) wideHoonSeq (PER) rank=>1
soloBar ::= BAR4H

# '~'
# Differs from scad(5)
# See also rupl(5d) in the '[' subcase
scat5d ::= circumSigParen
scat5d ::= circumSigBracket
scat5d ::= (SIG) twid4l
scat5d ::= (SIG) wede5d
scat5d ::= soloSig
circumSigBracket ::= (SIG SEL) wideHoonSeq (SER)
circumSigParen ::= (SIG PEL) rope5d (ACE) wideHoon (ACE) wideHoonSeq (PER)
soloSig ::= SIG

# '/'
# Not in scad(5)
scat5d ::= rood5d

# '<'
# Not in scad(5)
scat5d ::= circumGalgar
circumGalgar ::= ('<') wideHoon ('>')

# '>'
# Not in scad(5)
scat5d ::= circumGargal
circumGargal ::= ('>') wideHoon ('<')

# Molds from norm(5d)

# Running syntax
mold ::= tallBuccenMold
wideMold ::= wideBuccenMold
tallBuccenMold ::= (BUC CEN GAP) moldSeq (GAP '==')
wideBuccenMold ::= (BUC CEN '(') wideMoldSeq (')')

# [':' (rune col %bccl exqs)]
# ++  exqs  |.((butt hunk))                           ::  closed gapped roots
# Running syntax
mold ::= tallBuccolMold
wideMold ::= wideBuccolMold
tallBuccolMold ::= (BUC COL GAP) moldSeq (GAP '==')
wideBuccolMold ::= (BUC COL '(') wideMoldSeq (')')

# BUCHEP mold mold
mold ::= tallBuchepMold
wideMold ::= wideBuchepMold
tallBuchepMold ::= (BUC HEP GAP) mold (GAP)  mold
wideBuchepMold ::= (BUC HEP) [(] wideMold (ACE) wideMold [)]

# BUCPAT mold mold
mold ::= tallBucpatMold
wideMold ::= wideBucpatMold
tallBucpatMold ::= (BUC PAT GAP) mold (GAP)  mold
wideBucpatMold ::= (BUC PAT) [(] wideMold (ACE) wideMold [)]

# Running syntax
mold ::= tallBucwutMold
wideMold ::= wideBucwutMold
tallBucwutMold ::= (BUC WUT GAP) moldSeq (GAP '==')
wideBucwutMold ::= (BUC WUT '(') wideMoldSeq (')')

# TODO: Finish adding molds from norm

# Implementing soil(5d)

# TODO -- Finish soil(5d) -- add triple double strings

soil5d ::= doubleQuoteString

doubleQuoteString ::= (["]) <double quote cord> (["])
<double quote cord> ::= <double quote element>*
<double quote element> ::= <UNESCAPED DOUBLE QUOTE CHARS>
<double quote element> ::= <ESCAPED DOUBLE QUOTE CHAR>
<double quote element> ::= sump5d

# All the printable (non-control) characters except
# bas (x5c) kel (x7b) and doq (x22)
<UNESCAPED DOUBLE QUOTE CHARS> ~ unescapedDoubleQuoteChar+
unescapedDoubleQuoteChar ~ [\x20-\x21\x23-\x5b\x5d-\x7a\x7c-\x7e\x80-\xff]
<ESCAPED DOUBLE QUOTE CHAR> ~ bas4h bas4h | bas4h doq4h | bas4h kel4h | bas4h bix4j

sump5d ::= KEL wideHoonSeq KER

# === HOON FILE ===
:start ::= hoonFile
# LATER: This is a simplication, which does not
# catch all the subtleties of "ford" files
hoonFile ::= (leader) hoonSeq (trailer)

trailer ::= WS
trailer ::=
leader  ::= WS
leader  ::=

# A hack to allow inaccessible symbols
hoonFile ::= UNICORN inaccessible_ok

# === WHITESPACE ===

WS ~ whitespace
whitespace ~ ace
whitespace ~ gap4k
optHorizontalWhitespace ~ horizontalWhitespaceElement*
horizontalWhitespaceElements ~ horizontalWhitespaceElement+
horizontalWhitespaceElement ~ ace
# horizontalWhitespaceElement ~ horizontalGapElement
# horizontalGapElement ~ '+=' # documentation decoration

GAP ~ gap4k

gap4k ~ ace horizontalWhitespaceElements # a "wide" gap
gap4k ~ tallGapPrefix optGapLines optHorizontalWhitespace
# The prefix must contain an <NL> to ensure that this *is* a tall gap
tallGapPrefix ~ optHorizontalWhitespace nl
tallGapPrefix ~ optHorizontalWhitespace comment
optGapLines ~ gapLine*
gapLine ~ optHorizontalWhitespace comment
gapLine ~ optHorizontalWhitespace nl

ACE ~ ace
ace ~ ' '
comment ~ '::' optNonNLs nl

# TODO: Is this treatment of these fas runes OK?
# No, it is not OK, need to look at ford rune
comment ~ '/?' optNonNLs nl
comment ~ '/+' optNonNLs nl
comment ~ '/-' optNonNLs nl

# Documentation decorations treated as comments
comment ~ ':>' optNonNLs nl
comment ~ ':<' optNonNLs nl
comment ~ '+|' optNonNLs nl

# NL ~ nl
nl ~ [\n]
optNonNLs ~ nonNL*
nonNL ~ [^\n]

wsChars ~ wsChar*
wsChar ~ [ \n]

# @ub   unsigned binary          0b10          (2)
NUMBER ~ binaryNumber
# syn match       hoonNumber        "0b[01]\{1,4\}\%(\.\_s*[01]\{4\}\)*"
binaryNumber ~ '0b' binaryPrefix binaryGroups
binaryPrefix ~ binaryDigit
binaryPrefix ~ binaryDigit binaryDigit
binaryPrefix ~ binaryDigit binaryDigit binaryDigit
binaryPrefix ~ binaryDigit binaryDigit binaryDigit binaryDigit
binaryDigit ~ [01]
binaryGroups ~ binaryGroup*
binaryGroup ~ [.] wsChars binaryDigit binaryDigit binaryDigit binaryDigit

# @uc   bitcoin address          0c1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa
# @ud   unsigned decimal         42            (42)
#                                1.420         (1420)
NUMBER ~ decimalNumber
# syn match       hoonNumber        "\d\{1,3\}\%(\.\_s\?\d\{3\}\)*"
decimalNumber ~ decimalPrefix decimalGroups
decimalPrefix ~ decimalDigit
decimalPrefix ~ decimalDigit decimalDigit
decimalPrefix ~ decimalDigit decimalDigit decimalDigit
decimalDigit ~ [0-9]
decimalGroups ~ decimalGroup*
decimalGroup ~ [.] wsChars decimalDigit decimalDigit decimalDigit

# @uv   unsigned base32          0v3ic5h.6urr6
NUMBER ~ vNumber
# syn match       hoonNumber        "0v[0-9a-v]\{1,5\}\%(\.\_s*[0-9a-v]\{5\}\)*"
vNumber ~ '0v' vNumPrefix vNumGroups
vNumPrefix ~ vNumDigit
vNumPrefix ~ vNumDigit vNumDigit
vNumPrefix ~ vNumDigit vNumDigit vNumDigit
vNumPrefix ~ vNumDigit vNumDigit vNumDigit vNumDigit
vNumPrefix ~ vNumDigit vNumDigit vNumDigit vNumDigit vNumDigit
vNumDigit ~ [0-9a-v]
vNumGroups ~ vNumGroup*
vNumGroup ~ [.] wsChars vNumDigit vNumDigit vNumDigit vNumDigit vNumDigit

# @uw   unsigned base64          0wsC5.yrSZC
NUMBER ~ wNumber
# syn match       hoonNumber        "0w[-~0-9a-zA-Z]\{1,5\}\%(\.\_s*[-~0-9a-zA-Z]\{5\}\)*"
wNumber ~ '0w' wNumPrefix wNumGroups
wNumPrefix ~ wNumDigit
wNumPrefix ~ wNumDigit wNumDigit
wNumPrefix ~ wNumDigit wNumDigit wNumDigit
wNumPrefix ~ wNumDigit wNumDigit wNumDigit wNumDigit
wNumPrefix ~ wNumDigit wNumDigit wNumDigit wNumDigit wNumDigit
wNumDigit ~ [-~0-9a-zA-Z]
wNumGroups ~ wNumGroup*
wNumGroup ~ [.] wsChars wNumDigit wNumDigit wNumDigit wNumDigit wNumDigit

# @ux   unsigned hexadecimal     0xcafe.babe
NUMBER ~ hexNumber
# syn match       hoonNumber        "0x\x\{1,4\}\%(\.\_s*\x\{4\}\)*"
hexNumber ~ '0x' hexPrefix hexGroups
hexPrefix ~ hexDigit
hexPrefix ~ hexDigit hexDigit
hexPrefix ~ hexDigit hexDigit hexDigit
hexPrefix ~ hexDigit hexDigit hexDigit hexDigit
hexDigit ~ [0-9a-fA-F]
hexGroups ~ hexGroup*
hexGroup ~ [.] wsChars hexDigit hexDigit hexDigit hexDigit

# === PATHS ==

# rood is the path parser

rood5d ::= [/] poor5d
poor5d ::= gash5d
poor5d ::= gash5d CEN4H porc5d
porc5d ::= optCen4hSeq FAS gash5d
optCen4hSeq ::= # empty
optCen4hSeq ::= CEN4H_SEQ
CEN4H_SEQ ~ cen4h+
gash5d ::= limp5d* separator=>[/]
limp5d ::= (optFasSeq) gasp5d
optFasSeq ::= # empty
optFasSeq ::= FAS_SEQ
FAS_SEQ ~ fas4h+
gasp5d ::= tisSeq
tisSeq ~ tis4h+
optTisSeq ::= # empty
optTisSeq ::= TIS_SEQ
TIS_SEQ ~ tis4h+
gasp5d ::= (optTisSeq) hasp5d (optTisSeq)
hasp5d ::= (SEL) wideHoon (SER)
hasp5d ::= (PEL) wideHoonSeq (PER)
hasp5d ::= BUC4H
hasp5d ::= qut4k
hasp5d ::= nuck4l

# === CELLS BY TYPE ==

hoonSeq ::= hoon+ separator=>GAP proper=>1
hoon ::= wideHoon
wideHoon ::= hoonUnary
hoonUnary ::= hoonPrimary
hoonPrimary ::= norm5dWide rank=>10
hoonPrimary ::= long5dWide rank=>8

wideHoonSeq ::= wideHoon+ separator=>ACE proper=>1

hoonJogging ::= hoonJogs
hoonJogs ::= hoonJog+ separator=>GAP proper=>1
hoonJog ::= hoon (GAP) hoon

battery ::= batteryElement* separator=>GAP proper=>1
batteryElement ::= hoonBatteryElement
batteryElement ::= moldBatteryElement
# TODO: What is the meaning of these various types of battery element?
hoonBatteryElement ::= ('++' GAP) SYM4K (GAP) hoon
moldBatteryElement ::= ('+=' GAP) SYM4K (GAP) mold
hoonBatteryElement ::= ('+-' GAP) SYM4K (GAP) hoon

# Implementing norm(5d) for hoons
 
# ['_' (runo cab %brcb [~ ~] exqr)]
hoon ::= tallBarcab
tallBarcab ::= (BAR CAB GAP) hoon (GAP) battery (GAP '--')

# ['%' (runo cen %brcn [~ ~] expe)]
# ++  expe  |.(wisp)                                  ::  core tail
hoon ::= tallBarcen
tallBarcen ::= (BAR CEN GAP) battery (GAP '--')

# [':' (runo col %brcl [~ ~] expb)]
# FIXED: barcol hoon hoon

# ['.' (runo dot %brdt [~ ~] expa)]
# FIXED: bardot hoon

# ['-' (runo hep %brhp [~ ~] expa)]
# FIXED: barhep hoon

# ['^' (runo ket %brkt [~ ~] expx)]
# ++  expx  |.  ;~  gunk  loaf [...] wisp ::  hoon and core tail
hoon ::= tallBarket
tallBarket ::= (BAR KET GAP) hoon (GAP) battery (GAP '--')

# ['~' (runo sig %brsg [~ ~] exqc)]
#  ++  exqc  |.(;~(gunk loan loaf))                    ::  root then hoon
# FIXED: barsig mold hoon

# ['*' (runo tar %brtr [~ ~] exqc)]
#  ++  exqc  |.(;~(gunk loan loaf))                    ::  root then hoon
# FIXED: bartar mold hoon

# ['=' (runo tis %brts [~ ~] exqc)]
# ++  exqc  |.(;~(gunk loan loaf))                    ::  root then hoon
hoon ::= tallBartis
norm5dWide ::= wideBartis
tallBartis ::= (BAR4H TIS GAP) mold (GAP) hoon
wideBartis ::= (BAR4H TIS) [(] wideMold (ACE) wideHoon [)]

# ['?' (runo wut %brwt [~ ~] expa)]
# FIXED: barwut hoon

# FIXED: buccab hoon

# Running syntax
hoon ::= tallBuccen
tallBuccen ::= (BUC CEN GAP) moldSeq (GAP '==')
norm5dWide ::= wideBuccen
wideBuccen ::= (BUC CEN '(') wideMoldSeq (')')

# [':' (rune col %bccl exqs)]
# ++  exqs  |.((butt hunk))                           ::  closed gapped roots
hoon ::= tallBuccol
tallBuccol ::= (BUC COL GAP) moldSeq (GAP '==')
norm5dWide ::= wideBuccol
wideBuccol ::= (BUC COL '(') wideMoldSeq (')')

# FIXED: buchep mold mold
# FIXED: bucket hoon hoon
# FIXED: bucpat hoon hoon

# Undocumented runes
# $*  ::  bunt (irregular form is *)
# FIXED: buctar hoon

# [';' (rune sem %bcsm exqa)]
# ++  exqa  |.(loan)                                  ::  one hoon
# Typo in hoon.hoon -- actually "loan" is a mold
hoon ::= tallBucsem
tallBucsem ::= (BUC SEM GAP) mold
norm5dWide ::= wideBucsem
wideBucsem ::= (BUC SEM '(') mold (')')

# ['=' (rune tis %bcts exqg)]
# ++  exqg  |.(;~(gunk sym loan))                     ::  term and root
# FIXED: buctis SYM4K mold

hoon ::= tallBucwut
tallBucwut ::= (BUC WUT GAP) hoonSeq (GAP '==')
norm5dWide ::= wideBucwut
wideBucwut ::= (BUC WUT '(') wideHoonSeq (')')

# FIXED: cendot hoon hoon

# FIXED: cenhep hoon hoon

# FIXED: cenket hoon hoon hoon hoon
# FIXED: cenlus hoon hoon hoon

# FIXED: censig rope5d hoon hoon

hoon ::= tallCentis
CENTIS ~ [%] [=]
tallCentis ::= CENTIS (GAP) rope5d (GAP) hoonJogging (GAP '==')

# FIXED: colcab hoon hoon

# FIXED: colhep hoon hoon

# FIXED: collus hoon hoon hoon
# FIXED: colket hoon hoon hoon hoon

# Running syntax
COLSIG ~ [:] [~]
hoon ::= tallColsig
tallColsig ::= (COLSIG GAP) hoonSeq (GAP '==')
norm5dWide ::= wideColsig
wideColsig ::= (COLSIG '(') wideHoonSeq (')')

# Running syntax
COLTAR ~ [:] [*]
hoon ::= tallColtar
tallColtar ::= (COLTAR GAP) hoonSeq (GAP '==')
norm5dWide ::= wideColtar
wideColtar ::= (COLTAR '(') wideHoonSeq (')')

# DOTKET hoon hoon
DOTKET ~ [.] [\^]
hoon ::= tallDotket
norm5dWide ::= wideDotket
tallDotket ::= (DOTKET GAP)hoon (GAP) hoonSeq
wideDotket ::= (DOTKET) [(] wideHoon (ACE) wideHoonSeq [)]

# FIXED: dottis hoon hoon

# :~  ['+' (rune lus %dtls expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: dotlus hoon

# FIXED: dottar hoon hoon
# FIXED: dotwut hoon

# FAS group are (usually?) ford runes:

# FIXED: fassem hoon hoon

# TODO: Where to classify this?
FASTIS ~ [\/] [=]
hoon ::= tallFastis
tallFastis ::= (FASTIS GAP) SYM4K (GAP) hoon
norm5dWide ::= wideFastis
wideFastis ::= (FASTIS) SYM4K '=' hoon


# ['|' (rune bar %ktbr expa)]
# FIXED: ketbar hoon

# ['.' (rune dot %ktdt expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: ketdot hoon hoon

# ['-' (rune hep %kthp exqc)]
# FIXED: kethep hoon hoon

# ['+' (rune lus %ktls expb)]
# FIXED: ketlus hoon hoon

# ['&' (rune pam %ktpm expa)]

# ['~' (rune sig %ktsg expa)]
# FIXED: ketsig hoon

# ['=' (rune tis %ktts expg)]
# ++  expg  |.(;~(gunk sym loaf))                     ::  term and hoon
# FIXED: kettis SYM4K hoon

# ['?' (rune wut %ktwt expa)]
# FIXED: ketwut hoon

# ['%' (rune cen %ktcn expa)]

# :~  ['|' (rune bar %sgbr expb)]
# FIXED: sigbar hoon hoon

# ['$' (rune buc %sgbc expf)]
# ++  expf  |.(;~(gunk ;~(pfix cen sym) loaf))        ::  %term and hoon
# FIXED: sigbuc CEN_SYM4K hoon

# ['_' (rune cab %sgcb expb)]

# ['%' (rune cen %sgcn hind)]
# TODO implement bonz(5d)
# ++  hind  |.(;~(gunk bonk loaf bonz loaf))          ::  jet hoon "bon"s hoon
# FIXED: sigcen bonk5d rope5d hoon hoon

# ['/' (rune fas %sgfs hine)]
# ++  hine  |.(;~(gunk bonk loaf))                    ::  jet-hint and hoon
# FIXED: sigfas bonk5d hoon

# ['<' (rune gal %sggl hinb)]
# FIXED: siggal hoon hoon

# ['>' (rune gar %sggr hinb)]
# FIXED: siggar bont5d hoon

# ['+' (rune lus %sgls hinc)]
# FIXED: siglus hoon

# ['&' (rune pam %sgpm hinf)]
# FIXED: sigpam hoon hoon

# ['=' (rune tis %sgts expb)]
# ['?' (rune wut %sgwt hing)]
# ['!' (rune zap %sgzp expb)]

# 1-fixed, then running syntax
SEMCOL ~ [;] [:]
hoon ::= tallSemcol
tallSemcol ::= (SEMCOL GAP) hoon (GAP) hoonSeq (GAP '==')
norm5dWide ::= wideSemcol
wideSemcol ::= (SEMCOL '(') hoon (ACE) wideHoonSeq (')')

# #semsem hoon value
# FIXED: semsem hoon hoon

# 1-fixed, then running syntax
SEMSIG ~ [;] [~]
hoon ::= tallSemsig
tallSemsig ::= (SEMSIG GAP) hoon (GAP) hoonSeq (GAP '==')
norm5dWide ::= wideSemsig
wideSemsig ::= (SEMSIG '(') hoon (ACE) wideHoonSeq (')')

# FIXED: sigcab hoon hoon
# FIXED: sigwut hoon hoon hoon
# FIXED: sigzap hoon hoon

# ['|' (rune bar %tsbr exqc)]
# ++  exqc  |.(;~(gunk loan loaf))                    ::  root then hoon
# FIXED: tisbar mold hoon

# FIXED: tiscom hoon hoon
# FIXED: tisdot rope5d hoon hoon
# FIXED: tishep hoon hoon

# tisfas taco hoon hoon
# FIXED: tisfas hoon hoon hoon

# FIXED: tisgal hoon hoon

# FIXED: tisgar hoon hoon
# FIXED: tisket hoon rope5d hoon hoon

#  ['+' (rune lus %tsls expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: tislus hoon hoon

# tissem taco hoon hoon
# FIXED: tissem hoon hoon hoon

# FIXED: tistar SYM4K hoon hoon
# FIXED: tiswut rope5d hoon hoon hoon

WUTBAR ~ [?] [|]
hoon ::= tallWutbar
tallWutbar ::= (WUTBAR GAP) hoonSeq (GAP '==')
tallWutbar ::= (BAR4H GAP) hoon
norm5dWide ::= wideWutbar
wideWutbar ::= (WUTBAR '(') wideHoonSeq (')')

# FIXED: wutcol hoon hoon hoon
# FIXED: wutdot hoon hoon hoon
# FIXED: wutgal hoon hoon
# FIXED: wutgar hoon hoon
# FIXED: wutzap hoon
# FIXED: wutket rope5d hoon hoon

WUTPAM ~ [?] [&]
hoon ::= tallWutpam
tallWutpam ::= (WUTPAM GAP) hoonSeq (GAP '==')
norm5dWide ::= wideWutpam
wideWutpam ::= (WUTPAM '(') wideHoonSeq (')')

# FIXED: wutpat rope5d hoon hoon
# FIXED: wutsig rope5d hoon hoon
# FIXED: wuttis hoon rope5d

WUTHEP ~ [?] [-]
hoon ::= tallWuthep
tallWuthep ::= WUTHEP (GAP) rope5d (GAP) hoonJogging (GAP '==')

# ['+' ;~(pfix lus (toad tkls))]
# ++  tkls  |.  %+  cook  |=  {a/tiki b/hoon c/(list (pair root hoon))}
# 			(~(wtls ah a) b c)
#	      (butt ;~(gunk teak loaf ruck))
hoon ::= tallWutlus
tallWutlus ::= (WUT LUS GAP) teak (GAP) hoon (GAP) ruck5d (GAP '==')
norm5dWide ::= wideWutlus
wideWutlus ::= (WUT LUS PEL) teak (ACE) hoon (ACE) wideRuck5d (PAR)

# Undocumented runes
# !:  ::  turn on debugging printfs
ZAPCOL ~ [!] [:]
hoon ::= tallZapcol
tallZapcol ::= ZAPCOL

# Undocumented runes
# !.  ::  turn off debugging printfs
ZAPDOT ~ [!] [.]
hoon ::= tallZapdot
tallZapdot ::= ZAPDOT

# FIXED: zapgar hoon

# ZAPTIS hoon
# Cannot use ZAPTIS because ZAP TIS must also
# be accepted
hoon ::= tallZaptis
norm5dWide ::= wideZaptis
tallZaptis ::= (ZAP TIS GAP)hoon
wideZaptis ::= (ZAP TIS) [(] wideHoon [)]

# ['?' (rune wut %zpwt hinh)]
# ++  hinh  |.                                        ::  1/2 numbers, hoon
#         ;~  gunk
#           ;~  pose
#             dem
#             (ifix [sel ser] ;~(plug dem ;~(pfix ace dem)))
#           ==
#           loaf
#         ==
hoon ::= tallZapWut
norm5dWide ::= wideZapWut
tallZapWut ::= (ZAP WUT GAP) dem4k (GAP) hoon
tallZapWut ::= (ZAP WUT GAP SEL) dem4k (ACE) dem4k (SER GAP) hoon
wideZapWut ::= (ZAP WUT ACE) dem4k (ACE) hoon
wideZapWut ::= (ZAP WUT ACE SEL) dem4k (ACE) dem4k (SER ACE) hoon

# zapzap (= crash) is implemented in scat5d

# Implementing rope(5d)

# the wing type is parsed by the rope(5d)
rope5d ::= limb+ separator=>[.] proper=>1
limb ::= ','
limb ::= optKets '$'
limb ::= optKets SYM4K
optKets ::= KET*
limb ::= BAR4H DIM4J
limb ::= LUS DIM4J
limb ::= PAM4H DIM4J
limb ::= VEN4K
limb ::= '.'

# Implementing ruck(5d)

ruck5d ::= ruck5dJog+ separator=>GAP proper=>1
ruck5dJog ::= mold (GAP) hoon

wideRuck5d ::= wideRuck5dJog+ separator=>GAP proper=>1
wideRuck5dJog ::= mold (COM ACE) hoon

# Implementing teak(5d)

# teak is
#
# 1) a mold, if possible, hoon otherwise,
#
# 2) an assignment to <SYM4K>, which is again, of a mold,
# if possible, of a hoon otherwise

teak ::= teakChoice
teakChoice ::= (KET TIS GAP) SYM4K (GAP) rope5d rank=>2
teakChoice ::= (KET TIS GAP) SYM4K (GAP) hoon rank=>1
teakChoice ::= hoon rank=>1
teakChoice ::= wideTeak rank=>0
wideTeak ::= wideTeakChoice
wideTeakChoice ::= SYM4K (TIS) rope5d rank=>2
wideTeakChoice ::= rope5d rank=>2
wideTeakChoice ::= SYM4K (TIS) wideHoon rank=>1
wideTeakChoice ::= wideHoon rank=>1

END_OF_DSL_MAIN

# Here is meta-programming to write piece 2

# ace and gap are not really char names,
# and are omitted
my %glyphs = (
    bar => '|',
    bas => '\x5c', # '\'
    buc => '$',
    cab => '_',
    cen => '%',
    col => ':',
    com => ',',
    doq => '"',
    dot => '.',
    fas => '/',
    gal => '<',
    gar => '>',
    hax => '#',
    hep => '-',
    kel => '{',
    ker => '}',
    ket => '\\^',
    lus => '+',
    pal => '(',
    pam => '&',
    par => ')',
    pat => '@',
    pel => '(',
    per => ')',
    sel => '\x5b', # '['
    sem => ';',
    ser => '\x5d', # ']'
    sig => '~',
    soq => '\'',
    tar => '*',
    tec => '`',
    tis => '=',
    wut => '?',
    zap => '!',
);

# takes LC alphanumeric rune name and samples
# for N-fixed rune and returns the Marpa rules
# for the tall and the 2 regular wide forms.
sub doFixedRune {
    my ($runeName, @samples) = @_;
    my @result = (join ' ', '#', (uc $runeName), @samples);
    my $glyphName1 = substr($runeName, 0, 3);
    my $glyphName2 = substr($runeName, 3, 3);
    my $glyph1 = $glyphs{$glyphName1} or die "no glyph for $glyphName1";
    my $glyph2 = $glyphs{$glyphName2};
    my $glyphLexeme1 = (uc $glyphName1) . '4H';
    my $glyphLexeme2 = (uc $glyphName2) . '4H';
    my $tallLHS = 'tall' . ucfirst $runeName;
    my $wideLHS = 'wide' . ucfirst $runeName;

    # hoon ::= tallBarhep
    push @result, 'hoon ::= ' . $tallLHS;

    # norm5dWide ::= wideBarhep
    push @result, 'norm5dWide ::= ' . $wideLHS;

    # tallBarhep ::= (BAR4H HEP4H GAP) hoon (GAP) hoon
    push @result, $tallLHS . ' ::= ('
      . $glyphLexeme1
      . ' ' . $glyphLexeme2
      . ' GAP)' . (join ' (GAP) ', @samples);
    state $wideEquiv = {
        hoon => 'wideHoon',
        teak => 'wideTeak',
        bont5d => 'wideBont5d',
    };
    my @wideSamples = map { $wideEquiv->{$_} // $_; } @samples;

    # wideBarhep ::= ([|] [-]) (ACE) wideHoon (ACE) wideHoon
    push @result, $wideLHS . ' ::= ('
    . $glyphLexeme1
    . ' ' . $glyphLexeme2
    . ') [(] ' . (join ' (ACE) ', @wideSamples) . q{ [)]};
    return join "\n", @result, '';
}

# === DSL Piece: Automatically generated Marpa rules ===

my @glyphRules = ();
for my $glyphName (sort keys %glyphs) {
    my $glyph = $glyphs{$glyphName};
    my $ucGlyphName = uc $glyphName;
    my $uc4hGlyphName = $ucGlyphName . '4H';
    my $lcGlyphName = $glyphName . '4h';
    push @glyphRules, "$ucGlyphName ~ $lcGlyphName";
    push @glyphRules, "$uc4hGlyphName ~ $lcGlyphName";
    push @glyphRules, "$lcGlyphName ~ [" . $glyph . q{]};
    push @glyphRules, "inaccessible_ok ::= $ucGlyphName";
    push @glyphRules, "inaccessible_ok ::= $uc4hGlyphName";
}
my $glyphAutoRules = join "\n", @glyphRules; 

my @dslAutoRules = ();
DESC: for my $desc (split "\n", $dsl_main) {
    my $originalDesc = $desc;
    chomp $desc; # remove newline
    next DESC if not $desc =~ s/^[#] FIXED: //;
    $desc =~ s/^\s+//; # eliminate leading spaces
    $desc =~ s/\s+$//; # eliminate trailing spaces
    my ($rune, @samples) = split /\s+/, $desc;
    die $originalDesc if not $rune;
    push @dslAutoRules, doFixedRune( $rune, @samples );
}
my $dslAutoRules = join "\n", @dslAutoRules;

# Print the two pieces
say join "\n", $dsl_warning, $dsl_main, $glyphAutoRules, $dslAutoRules;

