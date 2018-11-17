# !!! DO NOT EDIT !!!
# This code automatically written by yahc.PM


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
	wedeFirst => 1,
        wideHoon             => 1,
        wideHoonJogging      => 1,
        wideHoonJogs         => 1,
        wideHoonJog          => 1,
        wideHoonSeq          => 1,
        wideMold          => 1,
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
        hoonUnary              => 1,
        mold                 => 1,
        moldSeq              => 1,
        togaElements         => 1,
        rope5d                 => 1,
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
qit4k ::= <ESCAPED SINGLE QUOTE CHAR>
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
<ESCAPED SINGLE QUOTE CHAR> ~ bas4h bas4h | bas4h soq4h | bas4h mes4k

dem4k ::= DIT4K_SEQ+ separator=>gon4k proper=>1

DIT4K_SEQ ~ dit4kSeq
dit4kSeq ~ dit4k+
dit4k ~ [0-9]

hit4k ~ dit4k
hit4k ~ [a-f][A-F]

mes4k ~ hit4k hit4k

gon4k ~ bas4h gay4i fas4h

# === Hoon library: 4l ===

# TODO: crub(4l) is incomplete

crub4l ::= crub4l_part1
crub4l ::= crub4l_part1 DOT4H DOT4H crub4l_part2
crub4l ::= crub4l_part1 DOT4H DOT4H crub4l_part2 DOT4H DOT4H crub4l_part3
crub4l_part1 ::= DIM4J optHep DOT4H MOT4J DOT4H DIP4J
optHep ::= # empty
optHep ::= HEP
crub4l_part2 ::= dum4j DOT4H dum4j DOT4H dum4j
crub4l_part3 ::= crub4l_part3_elements
crub4l_part3_elements ::= crub4l_part3_element+ separator=>DOT4H proper=>1
crub4l_part3_element ::= qix4j

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

# TODO: Should all unary expression be <hoonPrimary>?

# TODO TO JK: Census circum irregular forms for those which should be broken out by
# n-ary, for n==1, n==2, n>=3.

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

# '_'
# Same as scad(5)
hoonUnary ::= prefixCab
prefixCab ::= (CAB) wideHoon

# '$'
# For rump, see subcase ['a' 'z']
# Differs from scad(5)
hoonPrimary ::= bucBuc
hoonPrimary ::= bucPam
hoonPrimary ::= bucBar
hoonPrimary ::= dollarTerm
bucBuc ::= BUC4H BUC4H
bucPam ::= BUC4H PAM4H
bucBar ::= BUC4H BAR4H
dollarTerm ::= BUC4H qut4k
dollarTerm ::= BUC4H nuck4l

# '%'
# Differs from scad(5)
hoonPrimary ::= cenPath
hoonPrimary ::= cenBuc
hoonPrimary ::= cenPam
hoonPrimary ::= cenBar
hoonPrimary ::= cenTerm
hoonPrimary ::= cenDirectories
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
hoonPrimary ::= prefixPam
hoonPrimary ::= pamPlusPrefix
hoonPrimary ::= soloPam
prefixPam ::= (PAM4H '(') wideHoonSeq (')')
pamPlusPrefix ::= (PAM4H) wede5d
soloPam ::= PAM4H

# '\''
# Not in scad(5)
hoonPrimary ::= singleQuoteString
singleQuoteString ::= qut4k

# '('
# Differs from scad(5)
# See https://raw.githubusercontent.com/urbit/old-urbit.org/master/doc/hoon/lan/irregular.markdown
# and cenhep in https://urbit.org/docs/hoon/irregular/
hoonPrimary ::= circumParen1
hoonPrimary ::= circumParen2
circumParen1 ::= ('(') wideHoon (')')
circumParen2 ::= ('(') wideHoon (ACE) wideHoonSeq (')')

# '{'
# Same as scad(5)
wideBuccol ::= ('{') wideMoldSeq ('}')

# '*'
# Superset of scad(5)
hoonPrimary ::= prefixTar
hoonPrimary ::= soloTar
prefixTar ::= TAR wideMold
soloTar ::= TAR

# '@'
# TODO: NYI
# Same as scad(5)
# '@'
# Same as scat(5d)
hoonPrimary ::= aura
aura ::= '@' auraName

# '+'
# Not in scad(5)
# TODO: Finish
hoonPrimary ::= irrDotlus
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
hoonPrimary ::= bisk4l
hoonPrimary ::= bisk4l wede5d

# ':'
# TODO: NYI
# Not in scad(5)

# '='
# Differs from scad(5)
tallDottis ::= (TIS GAP) hoon
hoonPrimary ::= irrDottis
irrDottis ::= ('=(') wideHoon (ACE) wideHoon (')')
irrDottis ::= ('=(') wideHoon (')')

# '?'
# Same as scad(5)
# TODO: Finish
hoonPrimary ::= circumWutParen
hoonPrimary ::= soloWut
circumWutParen ::= (WUT PEL) wideMoldSeq (PER)
soloWut ::= WUT

# '['
# Differs from scad(5)
hoonPrimary ::= rupl5d

# '^'
# Differs from scad(5)
# For rope(5d) see ['a' 'z'] subcase and rump(5d)
hoonPrimary ::= soloKet
soloKet ::= KET

# '`'
# Not in scad(5)
# TODO: Finish
hoonPrimary ::= irrKethep
irrKethep ::= ('`') mold ('`') hoon
hoonPrimary ::= prefixKet
prefixKet ::= ('`') wideHoon

# '"'
# Not in scad(5)
hoonPrimary ::= infixDot
infixDot ::= soil5d+ separator=>dog4i proper=>1

# ['a' 'z']
# Differs from scad(5)
hoonPrimary ::= rump5d

# '|'
# Not in scad(5)
# TODO: Finish
hoonPrimary ::= prefixBar
hoonPrimary ::= circumBarParen
hoonPrimary ::= soloBar
prefixBar ::= (BAR4H) wede5d rank=>1
circumBarParen ::= (BAR4H PEL) wideHoonSeq (PER) rank=>1
soloBar ::= BAR4H

# '~'
# Differs from scad(5)
# See also rupl(5d) in the '[' subcase
hoonPrimary ::= circumSigParen
hoonPrimary ::= circumSigBracket
hoonPrimary ::= (SIG) twid4l
hoonPrimary ::= (SIG) wede5d
hoonPrimary ::= soloSig
circumSigBracket ::= (SIG SEL) wideHoonSeq (SER)
circumSigParen ::= (SIG PEL) rope5d (ACE) wideHoon (ACE) wideHoonSeq (PER)
soloSig ::= SIG

# '/'
# Not in scad(5)
hoonPrimary ::= rood5d

# '<'
# Not in scad(5)
hoonPrimary ::= circumGalgar
circumGalgar ::= ('<') wideHoon ('>')

# '>'
# Not in scad(5)
hoonPrimary ::= circumGargal
circumGargal ::= ('>') wideHoon ('<')

# TODO: Finish adding rules from scat(5d)

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

# === NAMES ==

NAME ~ name
name ~ nameFirstChar nameLaterChars
name ~ '$'

nameFirstChar ~ [a-z]
nameLaterChars ~ nameLaterChar*
nameLaterChar ~ [a-z0-9-]

# === STRINGS ==

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
hoonUnary ::= hoonExpression
hoonExpression ::= infixColon
hoonExpression ::= infixKet
hoonExpression ::= infixEqual
hoonExpression ::= hoonPrimary
infixColon ::= hoonPrimary (':') wideHoon
infixKet ::= hoonPrimary ('^') wideHoon
infixEqual ::= toga ('=') hoonExpression

toga ::= NAME
toga ::= togaSeq
togaSeq ::= ('[') togaElements (']')
togaElements ::= togaElement+ separator=>ACE proper=>1
togaElement ::= toga
togaElement ::= SIG

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

wideHoonSeq ::= wideHoon+ separator=>ACE proper=>1

hoonJogging ::= hoonJogs
hoonJogs ::= hoonJog+ separator=>GAP proper=>1
hoonJog ::= hoon (GAP) hoon
wideHoonJogging ::= wideHoonJogs
wideHoonJogs ::= wideHoonJog+ separator=>wideHoonJoggingSeparator proper=>1
wideHoonJog ::= wideHoon (ACE) wideHoon
wideHoonJoggingSeparator ::= ',' ACE

battery ::= batteryElement* separator=>GAP proper=>1
batteryElement ::= hoonBatteryElement
batteryElement ::= moldBatteryElement
# TODO: What is the meaning of these various types of battery element?
hoonBatteryElement ::= ('++' GAP) SYM4K (GAP) hoon
moldBatteryElement ::= ('+=' GAP) SYM4K (GAP) mold
hoonBatteryElement ::= ('+-' GAP) SYM4K (GAP) hoon

# === CELLS BY RUNE ==

BARCAB ~ [|] [_]
hoon ::= tallBarcab
tallBarcab ::= (BARCAB GAP) hoon (GAP) battery (GAP '--')

# ['%' (runo cen %brcn [~ ~] expe)]
#   ++  expe  |.(wisp)                                  ::  core tail
BARCEN ~ [|] [%]
hoon ::= tallBarcen
tallBarcen ::= (BARCEN GAP) battery (GAP '--')

# FIXED: barcol hoon hoon
# FIXED: bardot hoon
# FIXED: barhep hoon

BARKET ~ [|] '^'
hoon ::= tallBarket
tallBarket ::= (BARKET GAP) hoon (GAP) battery (GAP '--')

# FIXED: barsig hoon hoon


# FIXED: bartar mold hoon
# ['*' (runo tar %brtr [~ ~] exqc)]
#  ++  exqc  |.(;~(gunk loan loaf))                    ::  root then hoon

# Cannot use BARTIS because BAR4H TIS must also be accepted
# BARTIS mold hoon
hoon ::= tallBartis
hoonPrimary ::= wideBartis
tallBartis ::= (BAR4H TIS GAP) mold (GAP) hoon
wideBartis ::= (BAR4H TIS) [(] wideMold (ACE) wideHoon [)]

# FIXED: barwut hoon

# FIXED: buccab hoon

# Running syntax
BUCCEN ~ [$] [%]
hoon ::= tallBuccen
tallBuccen ::= (BUCCEN GAP) moldSeq (GAP '==')
hoonPrimary ::= wideBuccen
wideBuccen ::= (BUCCEN '(') wideMoldSeq (')')

# [':' (rune col %bccl exqs)]
# ++  exqs  |.((butt hunk))                           ::  closed gapped roots
BUCCOL ~ [$] [:]
hoon ::= tallBuccol
tallBuccol ::= (BUCCOL GAP) moldSeq (GAP '==')
hoonPrimary ::= wideBuccol
wideBuccol ::= (BUCCOL '(') wideMoldSeq (')')

# FIXED: buchep mold mold
# FIXED: bucket hoon hoon
# FIXED: bucpat hoon hoon

# Undocumented runes
# $*  ::  bunt (irregular form is *)
# FIXED: buctar hoon

BUCSEM ~ [$] [;]
mold ::= moldBucsem
moldBucsem ::= (BUCSEM GAP) rope5d (GAP) wede5d (GAP '==')
moldBucsem ::= (BUCSEM GAP) rope5d (GAP '==')
wideMold ::= wideMoldBucsem
wideMoldBucsem ::= (BUCSEM '(') rope5d (ACE) wede5d (')')
wideMoldBucsem ::= (BUCSEM '(') rope5d (')')

# ['=' (rune tis %bcts exqg)]
# ++  exqg  |.(;~(gunk sym loan))                     ::  term and root
# FIXED: buctis SYM4K mold

BUCWUT ~ [$] [?]
hoon ::= tallBucwut
tallBucwut ::= (BUCWUT GAP) hoonSeq (GAP '==')
hoonPrimary ::= wideBucwut
wideBucwut ::= (BUCWUT '(') wideHoonSeq (')')

# FIXED: cendot hoon hoon

# FIXED: cenhep hoon hoon

# FIXED: cenket hoon hoon hoon hoon
# FIXED: cenlus hoon hoon hoon

# FIXED: censig rope5d hoon hoon

hoonPrimary ::= irrCentis
hoon ::= tallCentis
CENTIS ~ [%] [=]
tallCentis ::= CENTIS (GAP) rope5d (GAP) hoonJogging (GAP '==')
# TODO: Where to move?  Is this a value form?  model?  both?
irrCentis ::= rope5d ('(') wideHoonJogging (')')

# FIXED: colcab hoon hoon

# FIXED: colhep hoon hoon

# FIXED: collus hoon hoon hoon
# FIXED: colket hoon hoon hoon hoon

# Running syntax
COLSIG ~ [:] [~]
hoon ::= tallColsig
tallColsig ::= (COLSIG GAP) hoonSeq (GAP '==')
hoonPrimary ::= wideColsig
wideColsig ::= (COLSIG '(') wideHoonSeq (')')

# Running syntax
COLTAR ~ [:] [*]
hoon ::= tallColtar
tallColtar ::= (COLTAR GAP) hoonSeq (GAP '==')
hoonPrimary ::= wideColtar
wideColtar ::= (COLTAR '(') wideHoonSeq (')')

# DOTKET hoon hoon
DOTKET ~ [.] [\^]
hoon ::= tallDotket
hoonPrimary ::= wideDotket
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
tallFastis ::= (FASTIS GAP) NAME (GAP) hoon
hoonPrimary ::= wideFastis
wideFastis ::= (FASTIS) NAME '=' hoon

# FIXED: ketbar hoon

# FIXED: kethep hoon hoon

# FIXED: ketlus hoon hoon
# FIXED: ketsig hoon

# FIXED: kettis toga hoon

# FIXED: ketwut hoon

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
hoonPrimary ::= wideSemcol
wideSemcol ::= (SEMCOL '(') hoon (ACE) wideHoonSeq (')')

# #semsem hoon value
# FIXED: semsem hoon hoon

# 1-fixed, then running syntax
SEMSIG ~ [;] [~]
hoon ::= tallSemsig
tallSemsig ::= (SEMSIG GAP) hoon (GAP) hoonSeq (GAP '==')
hoonPrimary ::= wideSemsig
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
hoonPrimary ::= wideWutbar
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
hoonPrimary ::= wideWutpam
wideWutpam ::= (WUTPAM '(') wideHoonSeq (')')

# FIXED: wutpat rope5d hoon hoon
# FIXED: wutsig rope5d hoon hoon
# FIXED: wuttis hoon rope5d

WUTHEP ~ [?] [-]
hoon ::= tallWuthep
tallWuthep ::= WUTHEP (GAP) rope5d (GAP) hoonJogging (GAP '==')

# TODO: WUTLUS Should be teak hoon hoonJogging
WUTLUS ~ [?] [+]
hoon ::= tallWutlus
tallWutlus ::= WUTLUS (GAP) hoon (GAP) hoon (GAP) hoonJogging (GAP '==')

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
hoonPrimary ::= wideZaptis
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
wideHoon ::= tallZapWut
tallZapWut ::= (ZAP WUT GAP) dem4k (GAP) hoon
tallZapWut ::= (ZAP WUT GAP SEL) dem4k (ACE) dem4k (SER GAP) hoon
hoonPrimary ::= wideZapWut
wideZapWut ::= (ZAP WUT ACE) dem4k (ACE) hoon
wideZapWut ::= (ZAP WUT ACE SEL) dem4k (ACE) dem4k (SER ACE) hoon

# zapzap (= crash) is nullary
ZAPZAP ~ [!] [!]
wideHoon ::= ZAPZAP


BAR ~ bar4h
BAR4H ~ bar4h
bar4h ~ [|]
inaccessible_ok ::= BAR
inaccessible_ok ::= BAR4H
BAS ~ bas4h
BAS4H ~ bas4h
bas4h ~ [\\]
inaccessible_ok ::= BAS
inaccessible_ok ::= BAS4H
BUC ~ buc4h
BUC4H ~ buc4h
buc4h ~ [$]
inaccessible_ok ::= BUC
inaccessible_ok ::= BUC4H
CAB ~ cab4h
CAB4H ~ cab4h
cab4h ~ [_]
inaccessible_ok ::= CAB
inaccessible_ok ::= CAB4H
CEN ~ cen4h
CEN4H ~ cen4h
cen4h ~ [%]
inaccessible_ok ::= CEN
inaccessible_ok ::= CEN4H
COL ~ col4h
COL4H ~ col4h
col4h ~ [:]
inaccessible_ok ::= COL
inaccessible_ok ::= COL4H
COM ~ com4h
COM4H ~ com4h
com4h ~ [,]
inaccessible_ok ::= COM
inaccessible_ok ::= COM4H
DOQ ~ doq4h
DOQ4H ~ doq4h
doq4h ~ ["]
inaccessible_ok ::= DOQ
inaccessible_ok ::= DOQ4H
DOT ~ dot4h
DOT4H ~ dot4h
dot4h ~ [.]
inaccessible_ok ::= DOT
inaccessible_ok ::= DOT4H
FAS ~ fas4h
FAS4H ~ fas4h
fas4h ~ [/]
inaccessible_ok ::= FAS
inaccessible_ok ::= FAS4H
GAL ~ gal4h
GAL4H ~ gal4h
gal4h ~ [<]
inaccessible_ok ::= GAL
inaccessible_ok ::= GAL4H
GAR ~ gar4h
GAR4H ~ gar4h
gar4h ~ [>]
inaccessible_ok ::= GAR
inaccessible_ok ::= GAR4H
HAX ~ hax4h
HAX4H ~ hax4h
hax4h ~ [#]
inaccessible_ok ::= HAX
inaccessible_ok ::= HAX4H
HEP ~ hep4h
HEP4H ~ hep4h
hep4h ~ [-]
inaccessible_ok ::= HEP
inaccessible_ok ::= HEP4H
KEL ~ kel4h
KEL4H ~ kel4h
kel4h ~ [{]
inaccessible_ok ::= KEL
inaccessible_ok ::= KEL4H
KER ~ ker4h
KER4H ~ ker4h
ker4h ~ [}]
inaccessible_ok ::= KER
inaccessible_ok ::= KER4H
KET ~ ket4h
KET4H ~ ket4h
ket4h ~ [\^]
inaccessible_ok ::= KET
inaccessible_ok ::= KET4H
LUS ~ lus4h
LUS4H ~ lus4h
lus4h ~ [+]
inaccessible_ok ::= LUS
inaccessible_ok ::= LUS4H
PAL ~ pal4h
PAL4H ~ pal4h
pal4h ~ [(]
inaccessible_ok ::= PAL
inaccessible_ok ::= PAL4H
PAM ~ pam4h
PAM4H ~ pam4h
pam4h ~ [&]
inaccessible_ok ::= PAM
inaccessible_ok ::= PAM4H
PAR ~ par4h
PAR4H ~ par4h
par4h ~ [)]
inaccessible_ok ::= PAR
inaccessible_ok ::= PAR4H
PAT ~ pat4h
PAT4H ~ pat4h
pat4h ~ [@]
inaccessible_ok ::= PAT
inaccessible_ok ::= PAT4H
PEL ~ pel4h
PEL4H ~ pel4h
pel4h ~ [(]
inaccessible_ok ::= PEL
inaccessible_ok ::= PEL4H
PER ~ per4h
PER4H ~ per4h
per4h ~ [)]
inaccessible_ok ::= PER
inaccessible_ok ::= PER4H
SEL ~ sel4h
SEL4H ~ sel4h
sel4h ~ [\x5b]
inaccessible_ok ::= SEL
inaccessible_ok ::= SEL4H
SEM ~ sem4h
SEM4H ~ sem4h
sem4h ~ [;]
inaccessible_ok ::= SEM
inaccessible_ok ::= SEM4H
SER ~ ser4h
SER4H ~ ser4h
ser4h ~ [\x5d]
inaccessible_ok ::= SER
inaccessible_ok ::= SER4H
SIG ~ sig4h
SIG4H ~ sig4h
sig4h ~ [~]
inaccessible_ok ::= SIG
inaccessible_ok ::= SIG4H
SOQ ~ soq4h
SOQ4H ~ soq4h
soq4h ~ [']
inaccessible_ok ::= SOQ
inaccessible_ok ::= SOQ4H
TAR ~ tar4h
TAR4H ~ tar4h
tar4h ~ [*]
inaccessible_ok ::= TAR
inaccessible_ok ::= TAR4H
TEC ~ tec4h
TEC4H ~ tec4h
tec4h ~ [`]
inaccessible_ok ::= TEC
inaccessible_ok ::= TEC4H
TIS ~ tis4h
TIS4H ~ tis4h
tis4h ~ [=]
inaccessible_ok ::= TIS
inaccessible_ok ::= TIS4H
WUT ~ wut4h
WUT4H ~ wut4h
wut4h ~ [?]
inaccessible_ok ::= WUT
inaccessible_ok ::= WUT4H
ZAP ~ zap4h
ZAP4H ~ zap4h
zap4h ~ [!]
inaccessible_ok ::= ZAP
inaccessible_ok ::= ZAP4H
# BARCOL hoon hoon
hoon ::= tallBarcol
hoonPrimary ::= wideBarcol
tallBarcol ::= (BAR4H COL4H GAP)hoon (GAP) hoon
wideBarcol ::= (BAR4H COL4H) [(] wideHoon (ACE) wideHoon [)]

# BARDOT hoon
hoon ::= tallBardot
hoonPrimary ::= wideBardot
tallBardot ::= (BAR4H DOT4H GAP)hoon
wideBardot ::= (BAR4H DOT4H) [(] wideHoon [)]

# BARHEP hoon
hoon ::= tallBarhep
hoonPrimary ::= wideBarhep
tallBarhep ::= (BAR4H HEP4H GAP)hoon
wideBarhep ::= (BAR4H HEP4H) [(] wideHoon [)]

# BARSIG hoon hoon
hoon ::= tallBarsig
hoonPrimary ::= wideBarsig
tallBarsig ::= (BAR4H SIG4H GAP)hoon (GAP) hoon
wideBarsig ::= (BAR4H SIG4H) [(] wideHoon (ACE) wideHoon [)]

# BARTAR mold hoon
hoon ::= tallBartar
hoonPrimary ::= wideBartar
tallBartar ::= (BAR4H TAR4H GAP)mold (GAP) hoon
wideBartar ::= (BAR4H TAR4H) [(] mold (ACE) wideHoon [)]

# BARWUT hoon
hoon ::= tallBarwut
hoonPrimary ::= wideBarwut
tallBarwut ::= (BAR4H WUT4H GAP)hoon
wideBarwut ::= (BAR4H WUT4H) [(] wideHoon [)]

# BUCCAB hoon
hoon ::= tallBuccab
hoonPrimary ::= wideBuccab
tallBuccab ::= (BUC4H CAB4H GAP)hoon
wideBuccab ::= (BUC4H CAB4H) [(] wideHoon [)]

# BUCHEP mold mold
hoon ::= tallBuchep
hoonPrimary ::= wideBuchep
tallBuchep ::= (BUC4H HEP4H GAP)mold (GAP) mold
wideBuchep ::= (BUC4H HEP4H) [(] mold (ACE) mold [)]

# BUCKET hoon hoon
hoon ::= tallBucket
hoonPrimary ::= wideBucket
tallBucket ::= (BUC4H KET4H GAP)hoon (GAP) hoon
wideBucket ::= (BUC4H KET4H) [(] wideHoon (ACE) wideHoon [)]

# BUCPAT hoon hoon
hoon ::= tallBucpat
hoonPrimary ::= wideBucpat
tallBucpat ::= (BUC4H PAT4H GAP)hoon (GAP) hoon
wideBucpat ::= (BUC4H PAT4H) [(] wideHoon (ACE) wideHoon [)]

# BUCTAR hoon
hoon ::= tallBuctar
hoonPrimary ::= wideBuctar
tallBuctar ::= (BUC4H TAR4H GAP)hoon
wideBuctar ::= (BUC4H TAR4H) [(] wideHoon [)]

# BUCTIS SYM4K mold
hoon ::= tallBuctis
hoonPrimary ::= wideBuctis
tallBuctis ::= (BUC4H TIS4H GAP)SYM4K (GAP) mold
wideBuctis ::= (BUC4H TIS4H) [(] SYM4K (ACE) mold [)]

# CENDOT hoon hoon
hoon ::= tallCendot
hoonPrimary ::= wideCendot
tallCendot ::= (CEN4H DOT4H GAP)hoon (GAP) hoon
wideCendot ::= (CEN4H DOT4H) [(] wideHoon (ACE) wideHoon [)]

# CENHEP hoon hoon
hoon ::= tallCenhep
hoonPrimary ::= wideCenhep
tallCenhep ::= (CEN4H HEP4H GAP)hoon (GAP) hoon
wideCenhep ::= (CEN4H HEP4H) [(] wideHoon (ACE) wideHoon [)]

# CENKET hoon hoon hoon hoon
hoon ::= tallCenket
hoonPrimary ::= wideCenket
tallCenket ::= (CEN4H KET4H GAP)hoon (GAP) hoon (GAP) hoon (GAP) hoon
wideCenket ::= (CEN4H KET4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# CENLUS hoon hoon hoon
hoon ::= tallCenlus
hoonPrimary ::= wideCenlus
tallCenlus ::= (CEN4H LUS4H GAP)hoon (GAP) hoon (GAP) hoon
wideCenlus ::= (CEN4H LUS4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# CENSIG rope5d hoon hoon
hoon ::= tallCensig
hoonPrimary ::= wideCensig
tallCensig ::= (CEN4H SIG4H GAP)rope5d (GAP) hoon (GAP) hoon
wideCensig ::= (CEN4H SIG4H) [(] rope5d (ACE) wideHoon (ACE) wideHoon [)]

# COLCAB hoon hoon
hoon ::= tallColcab
hoonPrimary ::= wideColcab
tallColcab ::= (COL4H CAB4H GAP)hoon (GAP) hoon
wideColcab ::= (COL4H CAB4H) [(] wideHoon (ACE) wideHoon [)]

# COLHEP hoon hoon
hoon ::= tallColhep
hoonPrimary ::= wideColhep
tallColhep ::= (COL4H HEP4H GAP)hoon (GAP) hoon
wideColhep ::= (COL4H HEP4H) [(] wideHoon (ACE) wideHoon [)]

# COLLUS hoon hoon hoon
hoon ::= tallCollus
hoonPrimary ::= wideCollus
tallCollus ::= (COL4H LUS4H GAP)hoon (GAP) hoon (GAP) hoon
wideCollus ::= (COL4H LUS4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# COLKET hoon hoon hoon hoon
hoon ::= tallColket
hoonPrimary ::= wideColket
tallColket ::= (COL4H KET4H GAP)hoon (GAP) hoon (GAP) hoon (GAP) hoon
wideColket ::= (COL4H KET4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# DOTTIS hoon hoon
hoon ::= tallDottis
hoonPrimary ::= wideDottis
tallDottis ::= (DOT4H TIS4H GAP)hoon (GAP) hoon
wideDottis ::= (DOT4H TIS4H) [(] wideHoon (ACE) wideHoon [)]

# DOTLUS hoon
hoon ::= tallDotlus
hoonPrimary ::= wideDotlus
tallDotlus ::= (DOT4H LUS4H GAP)hoon
wideDotlus ::= (DOT4H LUS4H) [(] wideHoon [)]

# DOTTAR hoon hoon
hoon ::= tallDottar
hoonPrimary ::= wideDottar
tallDottar ::= (DOT4H TAR4H GAP)hoon (GAP) hoon
wideDottar ::= (DOT4H TAR4H) [(] wideHoon (ACE) wideHoon [)]

# DOTWUT hoon
hoon ::= tallDotwut
hoonPrimary ::= wideDotwut
tallDotwut ::= (DOT4H WUT4H GAP)hoon
wideDotwut ::= (DOT4H WUT4H) [(] wideHoon [)]

# FASSEM hoon hoon
hoon ::= tallFassem
hoonPrimary ::= wideFassem
tallFassem ::= (FAS4H SEM4H GAP)hoon (GAP) hoon
wideFassem ::= (FAS4H SEM4H) [(] wideHoon (ACE) wideHoon [)]

# KETBAR hoon
hoon ::= tallKetbar
hoonPrimary ::= wideKetbar
tallKetbar ::= (KET4H BAR4H GAP)hoon
wideKetbar ::= (KET4H BAR4H) [(] wideHoon [)]

# KETHEP hoon hoon
hoon ::= tallKethep
hoonPrimary ::= wideKethep
tallKethep ::= (KET4H HEP4H GAP)hoon (GAP) hoon
wideKethep ::= (KET4H HEP4H) [(] wideHoon (ACE) wideHoon [)]

# KETLUS hoon hoon
hoon ::= tallKetlus
hoonPrimary ::= wideKetlus
tallKetlus ::= (KET4H LUS4H GAP)hoon (GAP) hoon
wideKetlus ::= (KET4H LUS4H) [(] wideHoon (ACE) wideHoon [)]

# KETSIG hoon
hoon ::= tallKetsig
hoonPrimary ::= wideKetsig
tallKetsig ::= (KET4H SIG4H GAP)hoon
wideKetsig ::= (KET4H SIG4H) [(] wideHoon [)]

# KETTIS toga hoon
hoon ::= tallKettis
hoonPrimary ::= wideKettis
tallKettis ::= (KET4H TIS4H GAP)toga (GAP) hoon
wideKettis ::= (KET4H TIS4H) [(] toga (ACE) wideHoon [)]

# KETWUT hoon
hoon ::= tallKetwut
hoonPrimary ::= wideKetwut
tallKetwut ::= (KET4H WUT4H GAP)hoon
wideKetwut ::= (KET4H WUT4H) [(] wideHoon [)]

# SIGBAR hoon hoon
hoon ::= tallSigbar
hoonPrimary ::= wideSigbar
tallSigbar ::= (SIG4H BAR4H GAP)hoon (GAP) hoon
wideSigbar ::= (SIG4H BAR4H) [(] wideHoon (ACE) wideHoon [)]

# SIGBUC CEN_SYM4K hoon
hoon ::= tallSigbuc
hoonPrimary ::= wideSigbuc
tallSigbuc ::= (SIG4H BUC4H GAP)CEN_SYM4K (GAP) hoon
wideSigbuc ::= (SIG4H BUC4H) [(] CEN_SYM4K (ACE) wideHoon [)]

# SIGCEN bonk5d rope5d hoon hoon
hoon ::= tallSigcen
hoonPrimary ::= wideSigcen
tallSigcen ::= (SIG4H CEN4H GAP)bonk5d (GAP) rope5d (GAP) hoon (GAP) hoon
wideSigcen ::= (SIG4H CEN4H) [(] bonk5d (ACE) rope5d (ACE) wideHoon (ACE) wideHoon [)]

# SIGFAS bonk5d hoon
hoon ::= tallSigfas
hoonPrimary ::= wideSigfas
tallSigfas ::= (SIG4H FAS4H GAP)bonk5d (GAP) hoon
wideSigfas ::= (SIG4H FAS4H) [(] bonk5d (ACE) wideHoon [)]

# SIGGAL hoon hoon
hoon ::= tallSiggal
hoonPrimary ::= wideSiggal
tallSiggal ::= (SIG4H GAL4H GAP)hoon (GAP) hoon
wideSiggal ::= (SIG4H GAL4H) [(] wideHoon (ACE) wideHoon [)]

# SIGGAR bont5d hoon
hoon ::= tallSiggar
hoonPrimary ::= wideSiggar
tallSiggar ::= (SIG4H GAR4H GAP)bont5d (GAP) hoon
wideSiggar ::= (SIG4H GAR4H) [(] wideBont5d (ACE) wideHoon [)]

# SIGLUS hoon
hoon ::= tallSiglus
hoonPrimary ::= wideSiglus
tallSiglus ::= (SIG4H LUS4H GAP)hoon
wideSiglus ::= (SIG4H LUS4H) [(] wideHoon [)]

# SIGPAM hoon hoon
hoon ::= tallSigpam
hoonPrimary ::= wideSigpam
tallSigpam ::= (SIG4H PAM4H GAP)hoon (GAP) hoon
wideSigpam ::= (SIG4H PAM4H) [(] wideHoon (ACE) wideHoon [)]

# SEMSEM hoon hoon
hoon ::= tallSemsem
hoonPrimary ::= wideSemsem
tallSemsem ::= (SEM4H SEM4H GAP)hoon (GAP) hoon
wideSemsem ::= (SEM4H SEM4H) [(] wideHoon (ACE) wideHoon [)]

# SIGCAB hoon hoon
hoon ::= tallSigcab
hoonPrimary ::= wideSigcab
tallSigcab ::= (SIG4H CAB4H GAP)hoon (GAP) hoon
wideSigcab ::= (SIG4H CAB4H) [(] wideHoon (ACE) wideHoon [)]

# SIGWUT hoon hoon hoon
hoon ::= tallSigwut
hoonPrimary ::= wideSigwut
tallSigwut ::= (SIG4H WUT4H GAP)hoon (GAP) hoon (GAP) hoon
wideSigwut ::= (SIG4H WUT4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# SIGZAP hoon hoon
hoon ::= tallSigzap
hoonPrimary ::= wideSigzap
tallSigzap ::= (SIG4H ZAP4H GAP)hoon (GAP) hoon
wideSigzap ::= (SIG4H ZAP4H) [(] wideHoon (ACE) wideHoon [)]

# TISBAR mold hoon
hoon ::= tallTisbar
hoonPrimary ::= wideTisbar
tallTisbar ::= (TIS4H BAR4H GAP)mold (GAP) hoon
wideTisbar ::= (TIS4H BAR4H) [(] mold (ACE) wideHoon [)]

# TISCOM hoon hoon
hoon ::= tallTiscom
hoonPrimary ::= wideTiscom
tallTiscom ::= (TIS4H COM4H GAP)hoon (GAP) hoon
wideTiscom ::= (TIS4H COM4H) [(] wideHoon (ACE) wideHoon [)]

# TISDOT rope5d hoon hoon
hoon ::= tallTisdot
hoonPrimary ::= wideTisdot
tallTisdot ::= (TIS4H DOT4H GAP)rope5d (GAP) hoon (GAP) hoon
wideTisdot ::= (TIS4H DOT4H) [(] rope5d (ACE) wideHoon (ACE) wideHoon [)]

# TISHEP hoon hoon
hoon ::= tallTishep
hoonPrimary ::= wideTishep
tallTishep ::= (TIS4H HEP4H GAP)hoon (GAP) hoon
wideTishep ::= (TIS4H HEP4H) [(] wideHoon (ACE) wideHoon [)]

# TISFAS hoon hoon hoon
hoon ::= tallTisfas
hoonPrimary ::= wideTisfas
tallTisfas ::= (TIS4H FAS4H GAP)hoon (GAP) hoon (GAP) hoon
wideTisfas ::= (TIS4H FAS4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# TISGAL hoon hoon
hoon ::= tallTisgal
hoonPrimary ::= wideTisgal
tallTisgal ::= (TIS4H GAL4H GAP)hoon (GAP) hoon
wideTisgal ::= (TIS4H GAL4H) [(] wideHoon (ACE) wideHoon [)]

# TISGAR hoon hoon
hoon ::= tallTisgar
hoonPrimary ::= wideTisgar
tallTisgar ::= (TIS4H GAR4H GAP)hoon (GAP) hoon
wideTisgar ::= (TIS4H GAR4H) [(] wideHoon (ACE) wideHoon [)]

# TISKET hoon rope5d hoon hoon
hoon ::= tallTisket
hoonPrimary ::= wideTisket
tallTisket ::= (TIS4H KET4H GAP)hoon (GAP) rope5d (GAP) hoon (GAP) hoon
wideTisket ::= (TIS4H KET4H) [(] wideHoon (ACE) rope5d (ACE) wideHoon (ACE) wideHoon [)]

# TISLUS hoon hoon
hoon ::= tallTislus
hoonPrimary ::= wideTislus
tallTislus ::= (TIS4H LUS4H GAP)hoon (GAP) hoon
wideTislus ::= (TIS4H LUS4H) [(] wideHoon (ACE) wideHoon [)]

# TISSEM hoon hoon hoon
hoon ::= tallTissem
hoonPrimary ::= wideTissem
tallTissem ::= (TIS4H SEM4H GAP)hoon (GAP) hoon (GAP) hoon
wideTissem ::= (TIS4H SEM4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# TISTAR SYM4K hoon hoon
hoon ::= tallTistar
hoonPrimary ::= wideTistar
tallTistar ::= (TIS4H TAR4H GAP)SYM4K (GAP) hoon (GAP) hoon
wideTistar ::= (TIS4H TAR4H) [(] SYM4K (ACE) wideHoon (ACE) wideHoon [)]

# TISWUT rope5d hoon hoon hoon
hoon ::= tallTiswut
hoonPrimary ::= wideTiswut
tallTiswut ::= (TIS4H WUT4H GAP)rope5d (GAP) hoon (GAP) hoon (GAP) hoon
wideTiswut ::= (TIS4H WUT4H) [(] rope5d (ACE) wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# WUTCOL hoon hoon hoon
hoon ::= tallWutcol
hoonPrimary ::= wideWutcol
tallWutcol ::= (WUT4H COL4H GAP)hoon (GAP) hoon (GAP) hoon
wideWutcol ::= (WUT4H COL4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# WUTDOT hoon hoon hoon
hoon ::= tallWutdot
hoonPrimary ::= wideWutdot
tallWutdot ::= (WUT4H DOT4H GAP)hoon (GAP) hoon (GAP) hoon
wideWutdot ::= (WUT4H DOT4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# WUTGAL hoon hoon
hoon ::= tallWutgal
hoonPrimary ::= wideWutgal
tallWutgal ::= (WUT4H GAL4H GAP)hoon (GAP) hoon
wideWutgal ::= (WUT4H GAL4H) [(] wideHoon (ACE) wideHoon [)]

# WUTGAR hoon hoon
hoon ::= tallWutgar
hoonPrimary ::= wideWutgar
tallWutgar ::= (WUT4H GAR4H GAP)hoon (GAP) hoon
wideWutgar ::= (WUT4H GAR4H) [(] wideHoon (ACE) wideHoon [)]

# WUTZAP hoon
hoon ::= tallWutzap
hoonPrimary ::= wideWutzap
tallWutzap ::= (WUT4H ZAP4H GAP)hoon
wideWutzap ::= (WUT4H ZAP4H) [(] wideHoon [)]

# WUTKET rope5d hoon hoon
hoon ::= tallWutket
hoonPrimary ::= wideWutket
tallWutket ::= (WUT4H KET4H GAP)rope5d (GAP) hoon (GAP) hoon
wideWutket ::= (WUT4H KET4H) [(] rope5d (ACE) wideHoon (ACE) wideHoon [)]

# WUTPAT rope5d hoon hoon
hoon ::= tallWutpat
hoonPrimary ::= wideWutpat
tallWutpat ::= (WUT4H PAT4H GAP)rope5d (GAP) hoon (GAP) hoon
wideWutpat ::= (WUT4H PAT4H) [(] rope5d (ACE) wideHoon (ACE) wideHoon [)]

# WUTSIG rope5d hoon hoon
hoon ::= tallWutsig
hoonPrimary ::= wideWutsig
tallWutsig ::= (WUT4H SIG4H GAP)rope5d (GAP) hoon (GAP) hoon
wideWutsig ::= (WUT4H SIG4H) [(] rope5d (ACE) wideHoon (ACE) wideHoon [)]

# WUTTIS hoon rope5d
hoon ::= tallWuttis
hoonPrimary ::= wideWuttis
tallWuttis ::= (WUT4H TIS4H GAP)hoon (GAP) rope5d
wideWuttis ::= (WUT4H TIS4H) [(] wideHoon (ACE) rope5d [)]

# ZAPGAR hoon
hoon ::= tallZapgar
hoonPrimary ::= wideZapgar
tallZapgar ::= (ZAP4H GAR4H GAP)hoon
wideZapgar ::= (ZAP4H GAR4H) [(] wideHoon [)]

