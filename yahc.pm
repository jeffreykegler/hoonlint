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

    say STDERR "Left main READ loop" if $main::DEBUG;

    # Return ref to value and new offset
    return \$value, $terminatorPos + length $terminator;
}

sub parse {
    my ($input) = @_;
    my $recce = Marpa::R2::Scanless::R->new(
        {
            grammar => $grammar,

            # trace_lexers    => 1,
            # trace_terminals => 1,
        }
    );

    my $input_length = length ${$input};
    my $this_pos;
    my $resume_pos;

    # The main read loop.  Read starting at $offset.
    # If interrupted execute the handler logic,
    # and, possibly, resume.

  READ:
    for (
        $this_pos = $recce->read( $input ) ;
        $this_pos < $input_length ;
        $this_pos = $recce->resume($resume_pos)
      )
    {

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
            say STDERR "$name event" if $main::DEBUG;
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
              if $main::DEBUG
	      ;
            next READ;
        }

        die "read() ended prematurely\n",
          "  input length = $input_length\n",
          "  length read = $this_pos\n",
          "  the cause may be an unexpected event";

    }

    if ( $recce->ambiguity_metric() > 1 ) {

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
        mold                 => 1,
        moldSeq              => 1,
        pathHoon             => 1,
        togaElements         => 1,
        wing                 => 1,
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

# BACKSLASH ~ backslash
bas4h ~ backslash
backslash ~ [\x5c] # hex 5c is backslash

CAB ~ cab4h
cab4h ~ [_]

CEN ~ cen4h
cen4h ~ [%]

DOT ~ dot4h
dot4h ~ [.]

FAS ~ fas4h
fas4h ~ [/]

optHep ::=
optHep ::= HEP
HEP ~ hep4h
hep4h ~ [-]

KET ~ ket4h
ket4h ~ '^'
LUS ~ lus4h
lus4h ~ '+'

PAM ~ pam4h
pam4h ~ '&'

SIG ~ sig4h
sig4h ~ '~'

soq4h ~ [']

TIS ~ tis4h
tis4h ~ '='

ZAP ~ zap4h
zap4h ~ '!'

# === Hoon library: 4i ===

# vul4i ~ '::' optNonNLs nl

# === Hoon library: 4j ===

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

gay4k ~ # empty
gay4k ~ gap4k

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

qit4k ::= <UNESCAPED CORD CHARS>
qit4k ::= <ESCAPED CORD CHAR>
<UNESCAPED CORD CHARS> ~ unescapedCordChar+

# LATER Single string element also allow escapes
# LATER: Add \xx hex escapes, and more backslash escapes
qut4k ::= <single quote string>
<single quote string> ::= ([']) <single soq cord> (['])
<single soq cord> ::= qit4k* separator=>gon4k proper=>1

# <TRIPLE QUOTE START> triggers an event -- the quoted
# string is actually supplies as <TRIPLE QUOTE STRING>.
qut4k ::= <TRIPLE QUOTE START>
qut4k ::= <TRIPLE QUOTE STRING>
:lexeme ~ <TRIPLE QUOTE START> event=>tripleQuote pause=>before
<TRIPLE QUOTE START> ~ ['] ['] [']
<TRIPLE QUOTE STRING> ~ unicorn # implemented with a combinator

# All the printable (non-control) characters except
# bas and soq
unescapedCordChar ~ [\x20-\x26\x28-\x5b\x5d-\x7e\x80-\xff]
<ESCAPED CORD CHAR> ~ bas4h bas4h | bas4h soq4h | bas4h mes4k

dit4k ~ [0-9]

hit4k ~ dit4k
hit4k ~ [a-f][A-F]

mes4k ~ hit4k hit4k

gon4k ~ bas4h gay4k fas4h

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
wideMold ::= moldInfixCol
moldInfixCol ::= wing+ separator=>[:] proper=>1

# '='
# Differs from scat(5d)
wideMold ::= moldPrefixTis
moldPrefixTis ::= ('=') wideMold (')')

wideMold ::= moldInfixTis
moldInfixTis ::= SYM4K ('=') wideMold

# ['a' 'z']
# Differs from scat(5d)
# see the KET subcase

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
pathHoon ::= prefixZap
prefixZap ::= (ZAP) wideHoon

# '_'
# Same as scad(5)
pathHoon ::= prefixCab
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
prefixPam ::= (PAM '(') wideHoonSeq (')')

# '\''
# Not in scad(5)
hoonPrimary ::= singleQuoteString
singleQuoteString ::= qut4k

# '('
# Differs from scad(5)
# See https://raw.githubusercontent.com/urbit/old-urbit.org/master/doc/hoon/lan/irregular.markdown
# and cenhep in https://urbit.org/docs/hoon/irregular/
pathHoon ::= circumParen1
pathHoon ::= circumParen2
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
# TODO: NYI
# Not in scad(5)

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
pathHoon ::= rupl5d

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

# ['a' 'z']
# Differs from scad(5)

# '|'
# Not in scad(5)
hoonPrimary ::= prefixBar
prefixBar ::= (BAR) wideHoon

# '~'
# Differs from scad(5)
# See also the '[' subcase
hoonPrimary ::= irrCensig
hoonPrimary ::= circumSigBracket
irrCensig ::= ('~(') wideHoonSeq (')')
circumSigBracket ::= ('~[') wideHoonSeq (']')

# '/'
# Not in scad(5)

# '<'
# Not in scad(5)
pathHoon ::= circumGalgar
circumGalgar ::= ('<') wideHoon ('>')

# '>'
# Not in scad(5)
pathHoon ::= circumGargal
circumGargal ::= ('>') wideHoon ('<')

# TODO: Finish adding rules from scat(5d)

# TODO: Where is this from?
wideMold ::= moldInfixFas
moldInfixFas ::= SYM4K ('/') wideMold

# Molds from norm(5d)

# Running syntax
mold ::= tallBuccenMold
wideMold ::= wideBuccenMold
tallBuccenMold ::= (BUCCEN GAP) moldSeq (GAP '==')
wideBuccenMold ::= (BUCCEN '(') wideMoldSeq (')')

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

atom ::= NUMBER
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

atom ::= doubleString

# LATER: Add \xx hex escapes, and more backslash escapes
# LATER: See https://urbit.org/docs/hoon/atom/knit/ for interpolation
doubleString ::= (["]) doubleStringElements (["])
doubleStringElements ::= doubleStringElement*
doubleStringElement ::= stringInterpolation
doubleStringElement ::= DOUBLESTRINGCHARS
DOUBLESTRINGCHARS ~ doubleStringChar+
# hex 5C is backslash
# From syntax.vim, might need correction
doubleStringChar ~ [^"\x5c{] | backslash ["] | backslash backslash
stringInterpolation ::= '{' wideHoon '}'

# === PATHS ==

atom ::= path
path ::= [/] optPathSeq
optPathSeq ::= pathElement* separator=>[/]
pathElement ::= PATHSTRING
pathElement ::= qut4k

# pathHoon is hoon that is legal as part of a path
pathElement ::= pathHoon

PATHSTRING ~ pathStringChars
pathStringChars ~ pathStringChar+
# Do path strings allow escapes?
pathStringChar ~ [a-zA-Z-]

# === CELLS BY TYPE ==

hoonSeq ::= hoon+ separator=>GAP proper=>1
hoon ::= wideHoon
wideHoon ::= hoonExpression
hoonExpression ::= infixColon
hoonExpression ::= infixKet
hoonExpression ::= infixEqual
hoonExpression ::= infixPlus
hoonExpression ::= infixSlash
hoonExpression ::= hoonPrimary
infixColon ::= hoonPrimary (':') wideHoon
infixKet ::= hoonPrimary ('^') wideHoon
infixEqual ::= toga ('=') hoonExpression
infixPlus ::=  wedeFirst ('+') hoonExpression
infixSlash ::= wedeFirst ('/') hoonExpression

# LHS of wede(5d)
wedeFirst ::= wing
wedeFirst ::= '&'
wedeFirst ::= '|'
wedeFirst ::= bisk4l
wedeFirst ::= '~'

hoonPrimary ::= pathHoon

hoonPrimary ::= atom
hoonPrimary ::= wing

toga ::= NAME
toga ::= togaSeq
togaSeq ::= ('[') togaElements (']')
togaElements ::= togaElement+ separator=>ACE proper=>1
togaElement ::= toga
togaElement ::= NIL

# the wing type is parsed by the rope(5d)
wing ::= limb+ separator=>[.] proper=>1
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

# Running syntax
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
moldBucsem ::= (BUCSEM GAP) wing (GAP) wede5d (GAP '==')
moldBucsem ::= (BUCSEM GAP) wing (GAP '==')
wideMold ::= wideMoldBucsem
wideMoldBucsem ::= (BUCSEM '(') wing (ACE) wede5d (')')
wideMoldBucsem ::= (BUCSEM '(') wing (')')

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

# FIXED: censig wing hoon hoon

hoonPrimary ::= irrCentis
hoon ::= tallCentis
CENTIS ~ [%] [=]
tallCentis ::= CENTIS (GAP) wing (GAP) hoonJogging (GAP '==')
# TODO: Where to move?  Is this a value form?  model?  both?
irrCentis ::= wing ('(') wideHoonJogging (')')

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
# FIXED: sigcen term wing hoon hoon
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

# FIXED: tisbar hoon hoon
# FIXED: tiscom hoon hoon
# FIXED: tisdot wing hoon hoon
# FIXED: tishep hoon hoon

# tisfas taco hoon hoon
# FIXED: tisfas hoon hoon hoon

# FIXED: tisgal hoon hoon

# FIXED: tisgar hoon hoon
# FIXED: tisket hoon wing hoon hoon
# FIXED: tislus hoon hoon

# tissem taco hoon hoon
# FIXED: tissem hoon hoon hoon

# FIXED: tistar SYM4K hoon hoon
# FIXED: tiswut wing hoon hoon hoon

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
# FIXED: wutket wing hoon hoon

WUTPAM ~ [?] [&]
hoon ::= tallWutpam
tallWutpam ::= (WUTPAM GAP) hoonSeq (GAP '==')
hoonPrimary ::= wideWutpam
wideWutpam ::= (WUTPAM '(') wideHoonSeq (')')

# FIXED: wutpat wing hoon hoon
# FIXED: wutsig wing hoon hoon
# FIXED: wuttis hoon wing

WUTHEP ~ [?] [-]
hoon ::= tallWuthep
tallWuthep ::= WUTHEP (GAP) wing (GAP) hoonJogging (GAP '==')

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

# BARCOL hoon hoon
BARCOL ~ [|] [:]
hoon ::= tallBarcol
hoonPrimary ::= wideBarcol
tallBarcol ::= (BARCOL GAP)hoon (GAP) hoon
wideBarcol ::= (BARCOL) [(] wideHoon (ACE) wideHoon [)]

# BARDOT hoon
BARDOT ~ [|] [.]
hoon ::= tallBardot
hoonPrimary ::= wideBardot
tallBardot ::= (BARDOT GAP)hoon
wideBardot ::= (BARDOT) [(] wideHoon [)]

# BARHEP hoon
BARHEP ~ [|] [-]
hoon ::= tallBarhep
hoonPrimary ::= wideBarhep
tallBarhep ::= (BARHEP GAP)hoon
wideBarhep ::= (BARHEP) [(] wideHoon [)]

# BARSIG hoon hoon
BARSIG ~ [|] [~]
hoon ::= tallBarsig
hoonPrimary ::= wideBarsig
tallBarsig ::= (BARSIG GAP)hoon (GAP) hoon
wideBarsig ::= (BARSIG) [(] wideHoon (ACE) wideHoon [)]

# BARTAR mold hoon
BARTAR ~ [|] [*]
hoon ::= tallBartar
hoonPrimary ::= wideBartar
tallBartar ::= (BARTAR GAP)mold (GAP) hoon
wideBartar ::= (BARTAR) [(] mold (ACE) wideHoon [)]

# BARWUT hoon
BARWUT ~ [|] [?]
hoon ::= tallBarwut
hoonPrimary ::= wideBarwut
tallBarwut ::= (BARWUT GAP)hoon
wideBarwut ::= (BARWUT) [(] wideHoon [)]

# BUCCAB hoon
BUCCAB ~ [$] [_]
hoon ::= tallBuccab
hoonPrimary ::= wideBuccab
tallBuccab ::= (BUCCAB GAP)hoon
wideBuccab ::= (BUCCAB) [(] wideHoon [)]

# BUCHEP mold mold
BUCHEP ~ [$] [-]
hoon ::= tallBuchep
hoonPrimary ::= wideBuchep
tallBuchep ::= (BUCHEP GAP)mold (GAP) mold
wideBuchep ::= (BUCHEP) [(] mold (ACE) mold [)]

# BUCKET hoon hoon
BUCKET ~ [$] [\^]
hoon ::= tallBucket
hoonPrimary ::= wideBucket
tallBucket ::= (BUCKET GAP)hoon (GAP) hoon
wideBucket ::= (BUCKET) [(] wideHoon (ACE) wideHoon [)]

# BUCPAT hoon hoon
BUCPAT ~ [$] [@]
hoon ::= tallBucpat
hoonPrimary ::= wideBucpat
tallBucpat ::= (BUCPAT GAP)hoon (GAP) hoon
wideBucpat ::= (BUCPAT) [(] wideHoon (ACE) wideHoon [)]

# BUCTAR hoon
BUCTAR ~ [$] [*]
hoon ::= tallBuctar
hoonPrimary ::= wideBuctar
tallBuctar ::= (BUCTAR GAP)hoon
wideBuctar ::= (BUCTAR) [(] wideHoon [)]

# BUCTIS term hoon
BUCTIS ~ [$] [=]
hoon ::= tallBuctis
hoonPrimary ::= wideBuctis
tallBuctis ::= (BUCTIS GAP)term (GAP) hoon
wideBuctis ::= (BUCTIS) [(] term (ACE) wideHoon [)]

# CENDOT hoon hoon
CENDOT ~ [%] [.]
hoon ::= tallCendot
hoonPrimary ::= wideCendot
tallCendot ::= (CENDOT GAP)hoon (GAP) hoon
wideCendot ::= (CENDOT) [(] wideHoon (ACE) wideHoon [)]

# CENHEP hoon hoon
CENHEP ~ [%] [-]
hoon ::= tallCenhep
hoonPrimary ::= wideCenhep
tallCenhep ::= (CENHEP GAP)hoon (GAP) hoon
wideCenhep ::= (CENHEP) [(] wideHoon (ACE) wideHoon [)]

# CENKET hoon hoon hoon hoon
CENKET ~ [%] [\^]
hoon ::= tallCenket
hoonPrimary ::= wideCenket
tallCenket ::= (CENKET GAP)hoon (GAP) hoon (GAP) hoon (GAP) hoon
wideCenket ::= (CENKET) [(] wideHoon (ACE) wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# CENLUS hoon hoon hoon
CENLUS ~ [%] [+]
hoon ::= tallCenlus
hoonPrimary ::= wideCenlus
tallCenlus ::= (CENLUS GAP)hoon (GAP) hoon (GAP) hoon
wideCenlus ::= (CENLUS) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# CENSIG wing hoon hoon
CENSIG ~ [%] [~]
hoon ::= tallCensig
hoonPrimary ::= wideCensig
tallCensig ::= (CENSIG GAP)wing (GAP) hoon (GAP) hoon
wideCensig ::= (CENSIG) [(] wing (ACE) wideHoon (ACE) wideHoon [)]

# COLCAB hoon hoon
COLCAB ~ [:] [_]
hoon ::= tallColcab
hoonPrimary ::= wideColcab
tallColcab ::= (COLCAB GAP)hoon (GAP) hoon
wideColcab ::= (COLCAB) [(] wideHoon (ACE) wideHoon [)]

# COLHEP hoon hoon
COLHEP ~ [:] [-]
hoon ::= tallColhep
hoonPrimary ::= wideColhep
tallColhep ::= (COLHEP GAP)hoon (GAP) hoon
wideColhep ::= (COLHEP) [(] wideHoon (ACE) wideHoon [)]

# COLLUS hoon hoon hoon
COLLUS ~ [:] [+]
hoon ::= tallCollus
hoonPrimary ::= wideCollus
tallCollus ::= (COLLUS GAP)hoon (GAP) hoon (GAP) hoon
wideCollus ::= (COLLUS) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# COLKET hoon hoon hoon hoon
COLKET ~ [:] [\^]
hoon ::= tallColket
hoonPrimary ::= wideColket
tallColket ::= (COLKET GAP)hoon (GAP) hoon (GAP) hoon (GAP) hoon
wideColket ::= (COLKET) [(] wideHoon (ACE) wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# DOTTIS hoon hoon
DOTTIS ~ [.] [=]
hoon ::= tallDottis
hoonPrimary ::= wideDottis
tallDottis ::= (DOTTIS GAP)hoon (GAP) hoon
wideDottis ::= (DOTTIS) [(] wideHoon (ACE) wideHoon [)]

# DOTLUS atom
DOTLUS ~ [.] [+]
hoon ::= tallDotlus
hoonPrimary ::= wideDotlus
tallDotlus ::= (DOTLUS GAP)atom
wideDotlus ::= (DOTLUS) [(] atom [)]

# DOTTAR hoon hoon
DOTTAR ~ [.] [*]
hoon ::= tallDottar
hoonPrimary ::= wideDottar
tallDottar ::= (DOTTAR GAP)hoon (GAP) hoon
wideDottar ::= (DOTTAR) [(] wideHoon (ACE) wideHoon [)]

# DOTWUT hoon
DOTWUT ~ [.] [?]
hoon ::= tallDotwut
hoonPrimary ::= wideDotwut
tallDotwut ::= (DOTWUT GAP)hoon
wideDotwut ::= (DOTWUT) [(] wideHoon [)]

# FASSEM hoon hoon
FASSEM ~ [/] [;]
hoon ::= tallFassem
hoonPrimary ::= wideFassem
tallFassem ::= (FASSEM GAP)hoon (GAP) hoon
wideFassem ::= (FASSEM) [(] wideHoon (ACE) wideHoon [)]

# KETBAR hoon
KETBAR ~ [\^] [|]
hoon ::= tallKetbar
hoonPrimary ::= wideKetbar
tallKetbar ::= (KETBAR GAP)hoon
wideKetbar ::= (KETBAR) [(] wideHoon [)]

# KETHEP hoon hoon
KETHEP ~ [\^] [-]
hoon ::= tallKethep
hoonPrimary ::= wideKethep
tallKethep ::= (KETHEP GAP)hoon (GAP) hoon
wideKethep ::= (KETHEP) [(] wideHoon (ACE) wideHoon [)]

# KETLUS hoon hoon
KETLUS ~ [\^] [+]
hoon ::= tallKetlus
hoonPrimary ::= wideKetlus
tallKetlus ::= (KETLUS GAP)hoon (GAP) hoon
wideKetlus ::= (KETLUS) [(] wideHoon (ACE) wideHoon [)]

# KETSIG hoon
KETSIG ~ [\^] [~]
hoon ::= tallKetsig
hoonPrimary ::= wideKetsig
tallKetsig ::= (KETSIG GAP)hoon
wideKetsig ::= (KETSIG) [(] wideHoon [)]

# KETTIS toga hoon
KETTIS ~ [\^] [=]
hoon ::= tallKettis
hoonPrimary ::= wideKettis
tallKettis ::= (KETTIS GAP)toga (GAP) hoon
wideKettis ::= (KETTIS) [(] toga (ACE) wideHoon [)]

# KETWUT hoon
KETWUT ~ [\^] [?]
hoon ::= tallKetwut
hoonPrimary ::= wideKetwut
tallKetwut ::= (KETWUT GAP)hoon
wideKetwut ::= (KETWUT) [(] wideHoon [)]

# SIGBAR hoon hoon
SIGBAR ~ [~] [|]
hoon ::= tallSigbar
hoonPrimary ::= wideSigbar
tallSigbar ::= (SIGBAR GAP)hoon (GAP) hoon
wideSigbar ::= (SIGBAR) [(] wideHoon (ACE) wideHoon [)]

# SIGBUC term hoon
SIGBUC ~ [~] [$]
hoon ::= tallSigbuc
hoonPrimary ::= wideSigbuc
tallSigbuc ::= (SIGBUC GAP)term (GAP) hoon
wideSigbuc ::= (SIGBUC) [(] term (ACE) wideHoon [)]

# SIGCEN term wing hoon hoon
SIGCEN ~ [~] [%]
hoon ::= tallSigcen
hoonPrimary ::= wideSigcen
tallSigcen ::= (SIGCEN GAP)term (GAP) wing (GAP) hoon (GAP) hoon
wideSigcen ::= (SIGCEN) [(] term (ACE) wing (ACE) wideHoon (ACE) wideHoon [)]

# SIGFAS term hoon
SIGFAS ~ [~] [/]
hoon ::= tallSigfas
hoonPrimary ::= wideSigfas
tallSigfas ::= (SIGFAS GAP)term (GAP) hoon
wideSigfas ::= (SIGFAS) [(] term (ACE) wideHoon [)]

# SIGGAL hoon hoon
SIGGAL ~ [~] [<]
hoon ::= tallSiggal
hoonPrimary ::= wideSiggal
tallSiggal ::= (SIGGAL GAP)hoon (GAP) hoon
wideSiggal ::= (SIGGAL) [(] wideHoon (ACE) wideHoon [)]

# SIGGAR bont5d hoon
SIGGAR ~ [~] [>]
hoon ::= tallSiggar
hoonPrimary ::= wideSiggar
tallSiggar ::= (SIGGAR GAP)bont5d (GAP) hoon
wideSiggar ::= (SIGGAR) [(] wideBont5d (ACE) wideHoon [)]

# SIGLUS hoon
SIGLUS ~ [~] [+]
hoon ::= tallSiglus
hoonPrimary ::= wideSiglus
tallSiglus ::= (SIGLUS GAP)hoon
wideSiglus ::= (SIGLUS) [(] wideHoon [)]

# SIGPAM hoon hoon
SIGPAM ~ [~] [&]
hoon ::= tallSigpam
hoonPrimary ::= wideSigpam
tallSigpam ::= (SIGPAM GAP)hoon (GAP) hoon
wideSigpam ::= (SIGPAM) [(] wideHoon (ACE) wideHoon [)]

# SEMSEM hoon hoon
SEMSEM ~ [;] [;]
hoon ::= tallSemsem
hoonPrimary ::= wideSemsem
tallSemsem ::= (SEMSEM GAP)hoon (GAP) hoon
wideSemsem ::= (SEMSEM) [(] wideHoon (ACE) wideHoon [)]

# SIGCAB hoon hoon
SIGCAB ~ [~] [_]
hoon ::= tallSigcab
hoonPrimary ::= wideSigcab
tallSigcab ::= (SIGCAB GAP)hoon (GAP) hoon
wideSigcab ::= (SIGCAB) [(] wideHoon (ACE) wideHoon [)]

# SIGWUT hoon hoon hoon
SIGWUT ~ [~] [?]
hoon ::= tallSigwut
hoonPrimary ::= wideSigwut
tallSigwut ::= (SIGWUT GAP)hoon (GAP) hoon (GAP) hoon
wideSigwut ::= (SIGWUT) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# SIGZAP hoon hoon
SIGZAP ~ [~] [!]
hoon ::= tallSigzap
hoonPrimary ::= wideSigzap
tallSigzap ::= (SIGZAP GAP)hoon (GAP) hoon
wideSigzap ::= (SIGZAP) [(] wideHoon (ACE) wideHoon [)]

# TISBAR hoon hoon
TISBAR ~ [=] [|]
hoon ::= tallTisbar
hoonPrimary ::= wideTisbar
tallTisbar ::= (TISBAR GAP)hoon (GAP) hoon
wideTisbar ::= (TISBAR) [(] wideHoon (ACE) wideHoon [)]

# TISCOM hoon hoon
TISCOM ~ [=] [,]
hoon ::= tallTiscom
hoonPrimary ::= wideTiscom
tallTiscom ::= (TISCOM GAP)hoon (GAP) hoon
wideTiscom ::= (TISCOM) [(] wideHoon (ACE) wideHoon [)]

# TISDOT wing hoon hoon
TISDOT ~ [=] [.]
hoon ::= tallTisdot
hoonPrimary ::= wideTisdot
tallTisdot ::= (TISDOT GAP)wing (GAP) hoon (GAP) hoon
wideTisdot ::= (TISDOT) [(] wing (ACE) wideHoon (ACE) wideHoon [)]

# TISHEP hoon hoon
TISHEP ~ [=] [-]
hoon ::= tallTishep
hoonPrimary ::= wideTishep
tallTishep ::= (TISHEP GAP)hoon (GAP) hoon
wideTishep ::= (TISHEP) [(] wideHoon (ACE) wideHoon [)]

# TISFAS hoon hoon hoon
TISFAS ~ [=] [/]
hoon ::= tallTisfas
hoonPrimary ::= wideTisfas
tallTisfas ::= (TISFAS GAP)hoon (GAP) hoon (GAP) hoon
wideTisfas ::= (TISFAS) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# TISGAL hoon hoon
TISGAL ~ [=] [<]
hoon ::= tallTisgal
hoonPrimary ::= wideTisgal
tallTisgal ::= (TISGAL GAP)hoon (GAP) hoon
wideTisgal ::= (TISGAL) [(] wideHoon (ACE) wideHoon [)]

# TISGAR hoon hoon
TISGAR ~ [=] [>]
hoon ::= tallTisgar
hoonPrimary ::= wideTisgar
tallTisgar ::= (TISGAR GAP)hoon (GAP) hoon
wideTisgar ::= (TISGAR) [(] wideHoon (ACE) wideHoon [)]

# TISKET hoon wing hoon hoon
TISKET ~ [=] [\^]
hoon ::= tallTisket
hoonPrimary ::= wideTisket
tallTisket ::= (TISKET GAP)hoon (GAP) wing (GAP) hoon (GAP) hoon
wideTisket ::= (TISKET) [(] wideHoon (ACE) wing (ACE) wideHoon (ACE) wideHoon [)]

# TISLUS hoon hoon
TISLUS ~ [=] [+]
hoon ::= tallTislus
hoonPrimary ::= wideTislus
tallTislus ::= (TISLUS GAP)hoon (GAP) hoon
wideTislus ::= (TISLUS) [(] wideHoon (ACE) wideHoon [)]

# TISSEM hoon hoon hoon
TISSEM ~ [=] [;]
hoon ::= tallTissem
hoonPrimary ::= wideTissem
tallTissem ::= (TISSEM GAP)hoon (GAP) hoon (GAP) hoon
wideTissem ::= (TISSEM) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# TISTAR SYM4K hoon hoon
TISTAR ~ [=] [*]
hoon ::= tallTistar
hoonPrimary ::= wideTistar
tallTistar ::= (TISTAR GAP)SYM4K (GAP) hoon (GAP) hoon
wideTistar ::= (TISTAR) [(] SYM4K (ACE) wideHoon (ACE) wideHoon [)]

# TISWUT wing hoon hoon hoon
TISWUT ~ [=] [?]
hoon ::= tallTiswut
hoonPrimary ::= wideTiswut
tallTiswut ::= (TISWUT GAP)wing (GAP) hoon (GAP) hoon (GAP) hoon
wideTiswut ::= (TISWUT) [(] wing (ACE) wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# WUTCOL hoon hoon hoon
WUTCOL ~ [?] [:]
hoon ::= tallWutcol
hoonPrimary ::= wideWutcol
tallWutcol ::= (WUTCOL GAP)hoon (GAP) hoon (GAP) hoon
wideWutcol ::= (WUTCOL) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# WUTDOT hoon hoon hoon
WUTDOT ~ [?] [.]
hoon ::= tallWutdot
hoonPrimary ::= wideWutdot
tallWutdot ::= (WUTDOT GAP)hoon (GAP) hoon (GAP) hoon
wideWutdot ::= (WUTDOT) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# WUTGAL hoon hoon
WUTGAL ~ [?] [<]
hoon ::= tallWutgal
hoonPrimary ::= wideWutgal
tallWutgal ::= (WUTGAL GAP)hoon (GAP) hoon
wideWutgal ::= (WUTGAL) [(] wideHoon (ACE) wideHoon [)]

# WUTGAR hoon hoon
WUTGAR ~ [?] [>]
hoon ::= tallWutgar
hoonPrimary ::= wideWutgar
tallWutgar ::= (WUTGAR GAP)hoon (GAP) hoon
wideWutgar ::= (WUTGAR) [(] wideHoon (ACE) wideHoon [)]

# WUTZAP hoon
WUTZAP ~ [?] [!]
hoon ::= tallWutzap
hoonPrimary ::= wideWutzap
tallWutzap ::= (WUTZAP GAP)hoon
wideWutzap ::= (WUTZAP) [(] wideHoon [)]

# WUTKET wing hoon hoon
WUTKET ~ [?] [\^]
hoon ::= tallWutket
hoonPrimary ::= wideWutket
tallWutket ::= (WUTKET GAP)wing (GAP) hoon (GAP) hoon
wideWutket ::= (WUTKET) [(] wing (ACE) wideHoon (ACE) wideHoon [)]

# WUTPAT wing hoon hoon
WUTPAT ~ [?] [@]
hoon ::= tallWutpat
hoonPrimary ::= wideWutpat
tallWutpat ::= (WUTPAT GAP)wing (GAP) hoon (GAP) hoon
wideWutpat ::= (WUTPAT) [(] wing (ACE) wideHoon (ACE) wideHoon [)]

# WUTSIG wing hoon hoon
WUTSIG ~ [?] [~]
hoon ::= tallWutsig
hoonPrimary ::= wideWutsig
tallWutsig ::= (WUTSIG GAP)wing (GAP) hoon (GAP) hoon
wideWutsig ::= (WUTSIG) [(] wing (ACE) wideHoon (ACE) wideHoon [)]

# WUTTIS hoon wing
WUTTIS ~ [?] [=]
hoon ::= tallWuttis
hoonPrimary ::= wideWuttis
tallWuttis ::= (WUTTIS GAP)hoon (GAP) wing
wideWuttis ::= (WUTTIS) [(] wideHoon (ACE) wing [)]

# ZAPGAR hoon
ZAPGAR ~ [!] [>]
hoon ::= tallZapgar
hoonPrimary ::= wideZapgar
tallZapgar ::= (ZAPGAR GAP)hoon
wideZapgar ::= (ZAPGAR) [(] wideHoon [)]

# ZAPWUT atom hoon
ZAPWUT ~ [!] [?]
hoon ::= tallZapwut
hoonPrimary ::= wideZapwut
tallZapwut ::= (ZAPWUT GAP)atom (GAP) hoon
wideZapwut ::= (ZAPWUT) [(] atom (ACE) wideHoon [)]

