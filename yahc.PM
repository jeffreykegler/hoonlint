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

# === Hoon library: 4h ===

BAR ~ bar4h
bar4h ~ [|]

BUC ~ buc4h
buc4h ~ [$]

# BACKSLASH ~ backslash
bas4h ~ backslash
backslash ~ [\x5c] # hex 5c is backslash

CAB ~ cab4h
cab4h ~ [_]

CEN ~ cen4h
cen4h ~ [%]

DOT ~ dot4h
dot4h ~ [.]

# DOQ4H ~ doq4h
doq4h ~ ["]

FAS ~ fas4h
fas4h ~ [/]

optHep ::=
optHep ::= HEP
HEP ~ hep4h
hep4h ~ [-]

KEL ~ kel4h
kel4h ~ '{'
KER ~ ker4h
ker4h ~ '}'

KET ~ ket4h
ket4h ~ '^'
LUS ~ lus4h
lus4h ~ '+'

PAM ~ pam4h
pam4h ~ '&'

PEL ~ pel4h
pel4h ~ '('
PER ~ per4h
per4h ~ ')'

SEL ~ sel4h
sel4h ~ '['
SER ~ ser4h
ser4h ~ ']'

SIG ~ sig4h
sig4h ~ '~'

soq4h ~ [']

TIS ~ tis4h
tis4h ~ '='

ZAP ~ zap4h
zap4h ~ '!'

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

SYM4K ~ low4k sym4kRest
low4k ~ [a-z]
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

dit4k ~ [0-9]

hit4k ~ dit4k
hit4k ~ [a-f][A-F]

mes4k ~ hit4k hit4k

gon4k ~ bas4h gay4i fas4h

# === Hoon library: 4l ===

# TODO: crub(4l) is incomplete

crub4l ::= crub4l_part1
crub4l ::= crub4l_part1 DOT DOT crub4l_part2
crub4l ::= crub4l_part1 DOT DOT crub4l_part2 DOT DOT crub4l_part3
crub4l_part1 ::= DIM4J optHep DOT MOT4J DOT DIP4J
crub4l_part2 ::= dum4j DOT dum4j DOT dum4j
crub4l_part3 ::= crub4l_part3_elements
crub4l_part3_elements ::= crub4l_part3_element+ separator=>DOT proper=>1
crub4l_part3_element ::= qix4j

# nuck(4l) is the coin parser
nuck4l ::= SYM4K

# tash(4l) is the signed dime parser
nuck4l ::= TODO_tash4l
TODO_tash4l ::= UNICORN

# perd(4l) parses dimes or tuples without their standard prefixes
nuck4l ::= TODO_perl4l
TODO_perl4l ::= UNICORN

# Can be either '$~' or '%~'
# -- both seem to have the same semantics
nuck4l ::= moldNullSig
moldNullSig ::= '~'

# perd(4l) parses sig-prefixed coins after the sig prefix
nuck4l ::= SIG twid4l

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

bont5d ::= CEN SYM4K ([.] GAP) hoon
bont5d ::= wideBont5d
wideBont5d ::= CEN SYM4K ([.]) wideHoon
wideBont5d ::= CEN SYM4K ([.] ACE) wideHoon

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
moldAura ~ '@' OptLCs OptUCs
OptLCs ~ [a-z]*
OptUCs ~ [A-Z]*

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
# TODO: NYI
# Differs from scad(5)

# '%'
# TODO: NYI
# Differs from scad(5)

# '&'
# Not in scad(5)
# TODO: Finish
hoonPrimary ::= prefixPam
hoonPrimary ::= pamPlusPrefix
prefixPam ::= (PAM '(') wideHoonSeq (')')
pamPlusPrefix ::= (PAM) wede5d

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
# TODO: Finish
# Superset of scad(5)
hoonPrimary ::= prefixTar
prefixTar ::= '*' wideMold

# '@'
# TODO: NYI
# Same as scad(5)

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
wideBucwut ::= ('?(') wideMoldSeq (')')

# '['
# Differs from scad(5)
hoonPrimary ::= rupl5d

# '^'
# Differs from scad(5)
# TODO: NYI

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
prefixBar ::= (BAR) wede5d rank=>1
circumBarParen ::= (BAR PEL) wideHoonSeq (PER) rank=>1

# '~'
# Differs from scad(5)
# See also the '[' subcase
# TODO: Finish
hoonPrimary ::= circumSigParen
circumSigParen ::= ('~(') rope5d (ACE) wideHoon (ACE) wideHoonSeq (')')

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
tallBuccenMold ::= (BUCCEN GAP) moldSeq (GAP '==')
wideBuccenMold ::= (BUCCEN '(') wideMoldSeq (')')

# [':' (rune col %bccl exqs)]
# ++  exqs  |.((butt hunk))                           ::  closed gapped roots
# Running syntax
mold ::= tallBuccolMold
wideMold ::= wideBuccolMold
tallBuccolMold ::= (BUCCOL GAP) moldSeq (GAP '==')
wideBuccolMold ::= (BUCCOL '(') wideMoldSeq (')')

# BUCHEP mold mold
mold ::= tallBuchepMold
wideMold ::= wideBuchepMold
tallBuchepMold ::= (BUCHEP GAP) mold (GAP)  mold
wideBuchepMold ::= (BUCHEP) [(] wideMold (ACE) wideMold [)]

# BUCPAT mold mold
mold ::= tallBucpatMold
wideMold ::= wideBucpatMold
tallBucpatMold ::= (BUCPAT GAP) mold (GAP)  mold
wideBucpatMold ::= (BUCPAT) [(] wideMold (ACE) wideMold [)]

# Running syntax
mold ::= tallBucwutMold
wideMold ::= wideBucwutMold
tallBucwutMold ::= (BUCWUT GAP) moldSeq (GAP '==')
wideBucwutMold ::= (BUCWUT '(') wideMoldSeq (')')

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

# === ATOMS: SAND ===

atom ::= SIG twid4l
atom ::= loobean

# @c    UTF-32                   ~-foobar
# @da   128-bit absolute date    ~2016.4.23..20.09.26..f27b..dead..beef..babe
#                                ~2016.4.23
# @dr   128-bit relative date    ~s17          (17 seconds)
#                                ~m20          (20 minutes)
#                                ~d42          (42 days)

# @f    loobean                  &             (0, yes)
#                                |             (1, no)
loobean ::= '%.y'
loobean ::= '%.n'
loobean ::= '&'
loobean ::= '|'
loobean ::= '%&'
loobean ::= '%|'
# TODO: Same as %& and %| except "%leaf" instead of "%rock".
# What does that imply for semantics?
loobean ::= '$&'
loobean ::= '$|'

# @p                             ~zod          (0)
# @rd   64-bit IEEE float        .~3.14        (pi)
#                                .~-3.14       (negative pi)
# @rs   32-bit IEEE float        .3.14         (pi)
#                                .-3.14        (negative pi)
# @rq   128-bit IEEE float       .~~~3.14      (pi)
# @rh   16-bit IEEE float        .~~3.14       (pi)
# @sb   signed binary            --0b10        (2)
#                                -0b101        (-5)
# @sd   signed decimal           --2           (2)
#                                -5            (-5)
# @sv   signed base32            --0v68        (200)
#                                -0vfk         (-500)
# @sw   signed base64            --0w38        (200)
#                                -0w7Q         (500)
# @sx   signed hexadecimal       --0x2         (2)
#                                -0x5          -5
# @t    UTF-8 text (cord)        'foobar'
# @ta   ASCII text (knot)        ~.foobar

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

# TODO: Are these %sand atoms?

atom ::= NIL
NIL ~ '~'

atom ::= type
type ::= '*' # noun
type ::= '^' # cell
type ::= '?' # loobean
# LATER: commented out because these create ambiguities
# type ::= '~' # null
# type ::= '@' # cell

# === ATOMS: ROCK

atom ::= AURA
atom ::= term
atom ::= currentDirectory

currentDirectory ::= '%'

AURA ~ '@'
AURA ~ '@' optAlphas
optAlphas ~ [a-zA-Z]*

term ::= TERM
term ::= dollarTerm
dollarTerm ::= ('$') NAME
dollarTerm ::= dollarNil
dollarNil ::= ('$~')

TERM ~ '%$' # [%rock p=%tas q=0]
TERM ~ '%' firstTermChar
TERM ~ '%' firstTermChar optMedialTermChars lastTermChar
firstTermChar ~ [a-z]
optMedialTermChars ~ medialTermChar*
medialTermChar ~ [a-z0-9-]
lastTermChar ~ [a-z0-9]

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
poor5d ::= gash5d CEN porc5d
porc5d ::= FAS gash5d
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
hasp5d ::= BUC
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

hoonPrimary ::= atom
hoonPrimary ::= rope5d

toga ::= NAME
toga ::= togaSeq
togaSeq ::= ('[') togaElements (']')
togaElements ::= togaElement+ separator=>ACE proper=>1
togaElement ::= toga
togaElement ::= NIL

# the wing type is parsed by the rope(5d)
rope5d ::= limb+ separator=>[.] proper=>1
limb ::= ','
limb ::= optKets '$'
limb ::= optKets SYM4K
optKets ::= KET*
limb ::= BAR DIM4J
limb ::= LUS DIM4J
limb ::= PAM DIM4J
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

# Cannot use BARTIS because BAR TIS must also be accepted
# BARTIS mold hoon
hoon ::= tallBartis
hoonPrimary ::= wideBartis
tallBartis ::= (BAR TIS GAP) mold (GAP) hoon
wideBartis ::= (BAR TIS) [(] wideMold (ACE) wideHoon [)]

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

# FIXED: buctis term hoon

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

# FIXED: dotlus atom

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

# FIXED: sigbar hoon hoon
# FIXED: sigbuc term hoon
# FIXED: sigcen term rope5d hoon hoon
# FIXED: sigfas term hoon
# FIXED: siggal hoon hoon

# FIXED: siggar bont5d hoon

# FIXED: siglus hoon
# FIXED: sigpam hoon hoon

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
tallWutbar ::= (BAR GAP) hoon
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

# FIXED: zapwut atom hoon

# zapzap (= crash) is nullary
ZAPZAP ~ [!] [!]
wideHoon ::= ZAPZAP

END_OF_DSL_MAIN

# Here is meta-programming to write piece 2

# ace and gap are not really char names,
# and are omitted
my %glyphs = (
    bar => '|',
    bas => '\\',
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
    sel => '\xb5', # '['
    sem => ';',
    ser => ']',
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
    my $glyphname1 = substr($runeName, 0, 3);
    my $glyphname2 = substr($runeName, 3, 3);
    my $glyph1 = $glyphs{$glyphname1} or die "no glyph for $glyphname1";
    my $glyph2 = $glyphs{$glyphname2};
    my $lexeme = uc $runeName;
    my $tallLHS = 'tall' . ucfirst $runeName;
    my $wideLHS = 'wide' . ucfirst $runeName;

    # BARHEP ~ [|] [-]
    push @result, $lexeme . q{ ~ [} . $glyph1 . q{] [} . $glyph2 . q{]};

    # hoon ::= tallBarhep
    push @result, 'hoon ::= ' . $tallLHS;

    # hoonPrimary ::= wideBarhep
    push @result, 'hoonPrimary ::= ' . $wideLHS;

    # tallBarhep ::= (BARHEP GAP) hoon (GAP) hoon
    push @result, $tallLHS . ' ::= (' . $lexeme . ' GAP)' . (join ' (GAP) ', @samples);
    state $wideEquiv = {
        hoon => 'wideHoon',
        bont5d => 'wideBont5d',
    };
    my @wideSamples = map { $wideEquiv->{$_} // $_; } @samples;

    # wideBarhep ::= ([|] [-]) (ACE) wideHoon (ACE) wideHoon
    push @result, $wideLHS . ' ::= (' . $lexeme . ') [(] ' . (join ' (ACE) ', @wideSamples) . q{ [)]};
    return join "\n", @result, '';
}

# === DSL Piece: Automatically generated Marpa rules ===

my @dsl_auto_rules = ();
DESC: for my $desc (split "\n", $dsl_main) {
    my $originalDesc = $desc;
    chomp $desc; # remove newline
    next DESC if not $desc =~ s/^[#] FIXED: //;
    $desc =~ s/^\s+//; # eliminate leading spaces
    $desc =~ s/\s+$//; # eliminate trailing spaces
    my ($rune, @samples) = split /\s+/, $desc;
    die $originalDesc if not $rune;
    push @dsl_auto_rules, doFixedRune( $rune, @samples );
}
my $dsl_auto_rules = join "\n", @dsl_auto_rules;

# Print the two pieces
say join '', $dsl_warning, $dsl_main, $dsl_auto_rules;


