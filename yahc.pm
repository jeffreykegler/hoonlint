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

sub parse {
    my ( $input ) = @_;
    my $recce = Marpa::R2::Scanless::R->new(
        {
            grammar         => $grammar,
            # trace_lexers    => 1,
            # trace_terminals => 1,
        }
    );

    my $input_length = ${$input};
    my $length_read  = $recce->read($input);
    if ( $length_read != length $input_length ) {
        die "read() ended prematurely\n",
          "  input length = $input_length\n",
          "  length read = $length_read\n",
          "  the cause may be an unexpected event";
    } ## end if ( $length_read != length $input_length )
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
        flatHoon        => 1,
        flatHoonJogging => 1,
        flatHoonJogs    => 1,
        flatHoonSeq     => 1,
        hoon            => 1,
	hoonExpression => 1,
	hoonFile => 1,
        hoonJog     => 1,
        hoonJogging => 1,
        hoonJogs    => 1,
	hoonPrimary => 1,
        hoonSeq         => 1,
	pathHoon => 1,
	togaElements => 1,
	wing => 1,
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

# BACKSLASH ~ backslash
backslash ~ [\x5c] # hex 5c is backslash
# HEP ~ hep4h
hep4h ~ '-'
KET ~ ket4h
ket4h ~ '^'

# === Hoon library: 4j ===

DIM4J ~ dim4j # a natural number
dim4j ~ '0'
dim4j ~ [1-9] dim4jRest
dim4jRest ~ [0-9]

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

# === HOON FILE ===
:start ::= hoonFile
# LATER: This is a simplication, which does not
# catch all the subtleties of "ford" files
hoonFile ::= (leader) hoonSeq (trailer)

trailer ::= optWs
leader  ::= optWs

# === WHITESPACE ===

optWs ::=
optWs ::= ACE
optWs ::= gap

gap ::= ACE aces # a "flat" gap
gap ::= tallGapPrefix optGapLines optAces
# The prefix must contain an <NL> to ensure that this *is* a tall gap
tallGapPrefix ::= optAces NL
tallGapPrefix ::= optAces COMMENT
optGapLines ::= gapLine*
gapLine ::= optAces COMMENT
gapLine ::= optAces NL

optAces ::= ACE*
aces ::= ACE+

ACE ~ ' '
COMMENT ~ '::' nonNLs nl

# TODO: Is this treatment of these fas runes OK?
COMMENT ~ '/?' nonNLs nl
COMMENT ~ '/+' nonNLs nl
COMMENT ~ '/-' nonNLs nl

NL ~ nl
nl ~ [\n]
nonNLs ~ nonNL*
nonNL ~ [^\n]

wsChars ~ wsChar*
wsChar ~ [ \n]

# === ATOMS: SAND ===

atom ::= NUMBER
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
atom ::= SINGLESTRING

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
stringInterpolation ::= '{' flatHoon '}'

# LATER Single string element also allow escapes
# LATER: Add \xx hex escapes, and more backslash escapes
SINGLESTRING ~ ['] singleStringElements [']
singleStringElements ~ singleStringElement*
# hex 5C is backslash
# From syntax.vim, might need correction
singleStringElement ~ [^"\x5c] | backslash ["] | backslash backslash

# syn region      hoonString        start=+'+ skip=+\\[\\']+ end=+'+ contains=@spell
# syn region      hoonBlock         start=+'''+ end=+'''+
# syn region      hoonString        start=+"+ skip=+\\[\\"]+ end=+"+ contains=@spell

# === PATHS ==

atom ::= path
path ::= [/] optPathSeq
optPathSeq ::= pathElement* separator=>[/]
pathElement ::= PATHSTRING
pathElement ::= SINGLESTRING

# pathHoon is hoon that is legal as part of a path
pathElement ::= pathHoon

PATHSTRING ~ pathStringChars
pathStringChars ~ pathStringChar+
# Do path strings allow escapes?
pathStringChar ~ [a-zA-Z-]

# === CELLS BY TYPE ==

hoonSeq ::= hoon+ separator=>gap proper=>1
hoon ::= flatHoon
flatHoon ::= hoonExpression
hoonExpression ::= infixColon
hoonExpression ::= infixKet
hoonExpression ::= infixEqual
hoonExpression ::= infixPlus
hoonExpression ::= infixSlash
hoonExpression ::= hoonPrimary
infixColon ::= hoonPrimary (':') flatHoon
infixKet ::= hoonPrimary ('^') flatHoon
infixEqual ::= toga ('=') hoonExpression
infixPlus ::=  NAME ('+') hoonExpression
infixSlash ::= NAME ('/') hoonExpression

hoonPrimary ::= pathHoon

hoonPrimary ::= atom
hoonPrimary ::= wing

toga ::= NAME
toga ::= togaSeq
togaSeq ::= ('[') togaElements (']')
togaElements ::= togaElement+ separator=>ACE proper=>1
togaElement ::= toga
togaElement ::= NIL

# parsed by the rope arm in hoon.hoon
wing ::= limb+ separator=>[.] proper=>1
limb ::= ','
limb ::= optKets '$'
limb ::= optKets SYM4K
optKets ::= KET*
limb ::= [+&|] DIM4J
limb ::= VEN4K
limb ::= '.'

flatHoonSeq ::= flatHoon+ separator=>ACE proper=>1

hoonJogging ::= hoonJogs
hoonJogs ::= hoonJog+ separator=>gap proper=>1
hoonJog ::= hoon (gap) hoon
flatHoonJogging ::= flatHoonJogs
flatHoonJogs ::= flatHoonJog+ separator=>flatHoonJoggingSeparator proper=>1
flatHoonJog ::= flatHoon (ACE) flatHoon
flatHoonJoggingSeparator ::= ',' ACE

battery ::= batteryElement* separator=>gap proper=>1
batteryElement ::= ('++' gap) NAME (gap) hoon

# === CELLS BY RUNE ==

BARCAB ~ [|] [_]
hoon ::= tallBarcab
tallBarcab ::= (BARCAB gap) hoon (gap) battery (gap '--')

BARCEN ~ [|] [%]
hoon ::= tallBarcen
tallBarcen ::= (BARCEN gap) battery (gap '--')

# FIXED: barcol hoon hoon
# FIXED: bardot hoon
# FIXED: barhep hoon

BARKET ~ [|] '^'
hoon ::= tallBarket
tallBarket ::= (BARKET gap) hoon (gap) battery (gap '--')

# FIXED: barsig hoon hoon
# FIXED: bartar hoon hoon

# # LATER: Should eventually be (BARTIS) (gap) type (gap) hoon
# # where <type> is buc??? runes and irregular forms thereof
# FIXED: bartis hoon hoon

# FIXED: barwut hoon

# FIXED: buccab hoon

# Running syntax
BUCCOL ~ [$] [:]
hoon ::= tallBuccol
tallBuccol ::= (BUCCOL gap) hoonSeq (gap '==')
hoonPrimary ::= flatBuccol
flatBuccol ::= (BUCCOL '(') flatHoonSeq (')')
flatBuccol ::= (':bccl(') flatHoonSeq (')')
flatBuccol ::= ('{') flatHoonSeq ('}')
flatBuccol ::= (',[') flatHoonSeq (']')

# BUCCEN is "2-running"
BUCCEN ~ [$] [%]
hoon ::= tallBuccen
tallBuccen ::= (BUCCEN gap) buccenBody (gap '==')

buccenBody ::= buccenPair+ separator=>gap proper=>1
buccenPair ::= ('[' ACE) term (ACE) foot (']')
buccenPair ::= curlyPair
curlyPair ::= ('{') term (ACE) foot ('}')
foot ::= flatHoon

# FIXED: buchep hoon hoon
# FIXED: bucket hoon hoon
# FIXED: bucpat hoon hoon

# FIXED: buctar hoon

# Undocumented runes
# $*  ::  bunt (irregular form is *)
hoonPrimary ::= irrBuctar
irrBuctar ::= '*' flatHoon

# TODO: Should all unary expression be <hoonPrimary>?

# FIXED: buctis term hoon

BUCWUT ~ [$] [?]
hoon ::= tallBucwut
tallBucwut ::= (BUCWUT gap) hoonSeq (gap '==')
hoonPrimary ::= flatBucwut
flatBucwut ::= (BUCWUT '(') flatHoonSeq (')')
flatBucwut ::= (':bcwt(') flatHoonSeq (')')
flatBucwut ::= ('?(') flatHoonSeq (')')

# FIXED: cendot hoon hoon

# FIXED: cenhep hoon hoon

# FIXED: cenket hoon hoon hoon hoon
# FIXED: cenlus hoon hoon hoon

# FIXED: censig wing hoon hoon
hoonPrimary ::= irrCensig
irrCensig ::= ('~(') flatHoonSeq (')')

hoonPrimary ::= irrCentis
hoon ::= tallCentis
CENTIS ~ [%] [=]
tallCentis ::= CENTIS (gap) wing (gap) hoonJogging (gap '==')
irrCentis ::= wing ('(') flatHoonJogging (')')

# FIXED: colcab hoon hoon

# FIXED: colhep hoon hoon

# FIXED: collus hoon hoon hoon
# FIXED: colket hoon hoon hoon hoon

# Running syntax
COLSIG ~ [:] [~]
hoon ::= tallColsig
tallColsig ::= (COLSIG gap) hoonSeq (gap '==')
hoonPrimary ::= flatColsig
hoonPrimary ::= flatColsig2
hoonPrimary ::= flatColsig3
hoonPrimary ::= flatColsig4
flatColsig ::= (COLSIG '(') flatHoonSeq (')')
flatColsig ::= (':clsg(') flatHoonSeq (')')
flatColsig2 ::= ('~[') flatHoonSeq (']')
flatColsig3 ::= ('[') flatHoonSeq (']~')
flatColsig4 ::= ('`') flatHoon

# Running syntax
COLTAR ~ [:] [*]
hoon ::= tallColtar
tallColtar ::= (COLTAR gap) hoonSeq (gap '==')
hoonPrimary ::= flatColtar
flatColtar ::= (COLTAR '(') flatHoonSeq (')')
flatColtar ::= (':cltr(') flatHoonSeq (')')

# DOTKET hoon hoon
DOTKET ~ [.] [\^]
hoon ::= tallDotket
hoonPrimary ::= flatDotket
tallDotket ::= (DOTKET gap)hoon (gap) hoonSeq
flatDotket ::= (DOTKET) [(] flatHoon (ACE) flatHoonSeq [)]
flatDotket ::= (':dtkt') [(] flatHoon (ACE) flatHoonSeq [)]

# FIXED: dottis hoon hoon
hoonPrimary ::= irrDottis
irrDottis ::= ('=(') flatHoon (ACE) flatHoon (')')

# FIXED: dotlus atom
hoonPrimary ::= irrDotlus
irrDotlus ::= ('+(') flatHoon (')')

# FIXED: dottar hoon hoon
# FIXED: dotwut hoon

# FAS group are (usually?) ford runes:

# FIXED: fassem hoon hoon

FASTIS ~ [\/] [=]
hoon ::= tallFastis
tallFastis ::= (FASTIS gap) NAME (gap) hoon
hoonPrimary ::= flatFastis
flatFastis ::= (FASTIS) NAME '=' hoon

# FIXED: ketbar hoon

# FIXED: kethep hoon hoon
hoonPrimary ::= irrKethep
irrKethep ::= ('`') hoon ('`') hoon

# FIXED: ketlus hoon hoon
# FIXED: ketsig hoon

# FIXED: kettis toga hoon

# FIXED: ketwut hoon

# FIXED: sigbar hoon hoon
# FIXED: sigbuc term hoon
# FIXED: sigcen term wing hoon hoon
# FIXED: sigfas term hoon
# FIXED: siggal hoon hoon
# FIXED: siggar hoon hoon
# FIXED: siglus hoon
# FIXED: sigpam hoon hoon

# #semsem hoon value
# FIXED: semsem hoon hoon

# 1-fixed, then running syntax
SEMSIG ~ [;] [~]
hoon ::= tallSemsig
tallSemsig ::= (SEMSIG gap) hoon (gap) hoonSeq (gap '==')
hoonPrimary ::= flatSemsig
flatSemsig ::= (SEMSIG '(') hoon (ACE) flatHoonSeq (')')
flatSemsig ::= (':smsg(') hoon (ACE) flatHoonSeq (')')

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

# FIXED: tistar term hoon hoon
# FIXED: tiswut wing hoon hoon hoon

WUTBAR ~ [?] [|]
hoon ::= tallWutbar
tallWutbar ::= (WUTBAR gap) hoonSeq (gap '==')
hoonPrimary ::= flatWutbar
flatWutbar ::= (WUTBAR '(') flatHoonSeq (')')
flatWutbar ::= (':wtbr(') flatHoonSeq (')')
flatWutbar ::= ('|(') flatHoonSeq (')')

# FIXED: wutcol hoon hoon hoon
# FIXED: wutdot hoon hoon hoon
# FIXED: wutgal hoon hoon
# FIXED: wutgar hoon hoon
# FIXED: wutzap hoon
# FIXED: wutket wing hoon hoon

WUTPAM ~ [?] [&]
hoon ::= tallWutpam
tallWutpam ::= (WUTPAM gap) hoonSeq (gap '==')
hoonPrimary ::= flatWutpam
flatWutpam ::= (WUTPAM '(') flatHoonSeq (')')
flatWutpam ::= (':wtpm(') flatHoonSeq (')')
flatWutpam ::= ('&(') flatHoonSeq (')')

# FIXED: wutpat wing hoon hoon
# FIXED: wutsig wing hoon hoon
# FIXED: wuttis hoon wing

hoonPrimary ::= irrWutzap
irrWutzap ::= ('!') flatHoon

WUTHEP ~ [?] [-]
hoon ::= tallWuthep
tallWuthep ::= WUTHEP (gap) wing (gap) hoonJogging (gap '==')

WUTLUS ~ [?] [+]
hoon ::= tallWutlus
tallWutlus ::= WUTLUS (gap) wing (gap) hoon (gap) hoonJogging (gap '==')

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
# FIXED: zaptis hoon
# FIXED: zapwut atom hoon

# !;  ::  using the "type of type", emit the type for an expression
# !,  ::  emit AST of expression

# zapzap (= crash) is nullary
ZAPZAP ~ [!] [!]
flatHoon ::= ZAPZAP

# ^.  ::  use gate to transform type
# ^&  ::  zinc (covariant) -- see the docs on advanced types

# === IRREGULAR FORMS: PREFIX ===

pathHoon ::= prefixCab
prefixCab ::= ('_') flatHoon

# === IRREGULAR FORMS: CIRCUMFIX ===

# TODO TO JK: Census circum irregular forms for those which should be broken out by
# n-ary, for n==1, n==2, n>=3.

# See https://raw.githubusercontent.com/urbit/old-urbit.org/master/doc/hoon/lan/irregular.markdown
# and cenhep in https://urbit.org/docs/hoon/irregular/
pathHoon ::= circumParen1
pathHoon ::= circumParen2
pathHoon ::= circumParen3
circumParen1 ::= ('(') flatHoon (')')
circumParen2 ::= ('(') flatHoon (ACE) flatHoon (')')
circumParen3 ::= ('(') flatHoon (ACE) flatHoon (ACE) flatHoonSeq (')')

pathHoon ::= circumSelser1
pathHoon ::= circumSelser2
pathHoon ::= circumSelser3
pathHoon ::= circumSelser4
pathHoon ::= circumSelser5
circumSelser1 ::= ('[') flatHoon (']')
circumSelser2 ::= ('[') flatHoon (ACE) flatHoon (']')
circumSelser3 ::= ('[') flatHoon (ACE) flatHoon (ACE) flatHoon (']')
circumSelser4 ::= ('[') flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon (']')
circumSelser5 ::= ('[') flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoonSeq (']')

pathHoon ::= circumGalgar
circumGalgar ::= ('<') flatHoon ('>')

pathHoon ::= circumGargal
circumGargal ::= ('>') flatHoon ('<')

# BARCOL hoon hoon
BARCOL ~ [|] [:]
hoon ::= tallBarcol
hoonPrimary ::= flatBarcol
tallBarcol ::= (BARCOL gap)hoon (gap) hoon
flatBarcol ::= (BARCOL) [(] flatHoon (ACE) flatHoon [)]
flatBarcol ::= (':brcl') [(] flatHoon (ACE) flatHoon [)]

# BARDOT hoon
BARDOT ~ [|] [.]
hoon ::= tallBardot
hoonPrimary ::= flatBardot
tallBardot ::= (BARDOT gap)hoon
flatBardot ::= (BARDOT) [(] flatHoon [)]
flatBardot ::= (':brdt') [(] flatHoon [)]

# BARHEP hoon
BARHEP ~ [|] [-]
hoon ::= tallBarhep
hoonPrimary ::= flatBarhep
tallBarhep ::= (BARHEP gap)hoon
flatBarhep ::= (BARHEP) [(] flatHoon [)]
flatBarhep ::= (':brhp') [(] flatHoon [)]

# BARSIG hoon hoon
BARSIG ~ [|] [~]
hoon ::= tallBarsig
hoonPrimary ::= flatBarsig
tallBarsig ::= (BARSIG gap)hoon (gap) hoon
flatBarsig ::= (BARSIG) [(] flatHoon (ACE) flatHoon [)]
flatBarsig ::= (':brsg') [(] flatHoon (ACE) flatHoon [)]

# BARTAR hoon hoon
BARTAR ~ [|] [*]
hoon ::= tallBartar
hoonPrimary ::= flatBartar
tallBartar ::= (BARTAR gap)hoon (gap) hoon
flatBartar ::= (BARTAR) [(] flatHoon (ACE) flatHoon [)]
flatBartar ::= (':brtr') [(] flatHoon (ACE) flatHoon [)]

# BARTIS hoon hoon
BARTIS ~ [|] [=]
hoon ::= tallBartis
hoonPrimary ::= flatBartis
tallBartis ::= (BARTIS gap)hoon (gap) hoon
flatBartis ::= (BARTIS) [(] flatHoon (ACE) flatHoon [)]
flatBartis ::= (':brts') [(] flatHoon (ACE) flatHoon [)]

# BARWUT hoon
BARWUT ~ [|] [?]
hoon ::= tallBarwut
hoonPrimary ::= flatBarwut
tallBarwut ::= (BARWUT gap)hoon
flatBarwut ::= (BARWUT) [(] flatHoon [)]
flatBarwut ::= (':brwt') [(] flatHoon [)]

# BUCCAB hoon
BUCCAB ~ [$] [_]
hoon ::= tallBuccab
hoonPrimary ::= flatBuccab
tallBuccab ::= (BUCCAB gap)hoon
flatBuccab ::= (BUCCAB) [(] flatHoon [)]
flatBuccab ::= (':bccb') [(] flatHoon [)]

# BUCHEP hoon hoon
BUCHEP ~ [$] [-]
hoon ::= tallBuchep
hoonPrimary ::= flatBuchep
tallBuchep ::= (BUCHEP gap)hoon (gap) hoon
flatBuchep ::= (BUCHEP) [(] flatHoon (ACE) flatHoon [)]
flatBuchep ::= (':bchp') [(] flatHoon (ACE) flatHoon [)]

# BUCKET hoon hoon
BUCKET ~ [$] [\^]
hoon ::= tallBucket
hoonPrimary ::= flatBucket
tallBucket ::= (BUCKET gap)hoon (gap) hoon
flatBucket ::= (BUCKET) [(] flatHoon (ACE) flatHoon [)]
flatBucket ::= (':bckt') [(] flatHoon (ACE) flatHoon [)]

# BUCPAT hoon hoon
BUCPAT ~ [$] [@]
hoon ::= tallBucpat
hoonPrimary ::= flatBucpat
tallBucpat ::= (BUCPAT gap)hoon (gap) hoon
flatBucpat ::= (BUCPAT) [(] flatHoon (ACE) flatHoon [)]
flatBucpat ::= (':bcpt') [(] flatHoon (ACE) flatHoon [)]

# BUCTAR hoon
BUCTAR ~ [$] [*]
hoon ::= tallBuctar
hoonPrimary ::= flatBuctar
tallBuctar ::= (BUCTAR gap)hoon
flatBuctar ::= (BUCTAR) [(] flatHoon [)]
flatBuctar ::= (':bctr') [(] flatHoon [)]

# BUCTIS term hoon
BUCTIS ~ [$] [=]
hoon ::= tallBuctis
hoonPrimary ::= flatBuctis
tallBuctis ::= (BUCTIS gap)term (gap) hoon
flatBuctis ::= (BUCTIS) [(] term (ACE) flatHoon [)]
flatBuctis ::= (':bcts') [(] term (ACE) flatHoon [)]

# CENDOT hoon hoon
CENDOT ~ [%] [.]
hoon ::= tallCendot
hoonPrimary ::= flatCendot
tallCendot ::= (CENDOT gap)hoon (gap) hoon
flatCendot ::= (CENDOT) [(] flatHoon (ACE) flatHoon [)]
flatCendot ::= (':cndt') [(] flatHoon (ACE) flatHoon [)]

# CENHEP hoon hoon
CENHEP ~ [%] [-]
hoon ::= tallCenhep
hoonPrimary ::= flatCenhep
tallCenhep ::= (CENHEP gap)hoon (gap) hoon
flatCenhep ::= (CENHEP) [(] flatHoon (ACE) flatHoon [)]
flatCenhep ::= (':cnhp') [(] flatHoon (ACE) flatHoon [)]

# CENKET hoon hoon hoon hoon
CENKET ~ [%] [\^]
hoon ::= tallCenket
hoonPrimary ::= flatCenket
tallCenket ::= (CENKET gap)hoon (gap) hoon (gap) hoon (gap) hoon
flatCenket ::= (CENKET) [(] flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatCenket ::= (':cnkt') [(] flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# CENLUS hoon hoon hoon
CENLUS ~ [%] [+]
hoon ::= tallCenlus
hoonPrimary ::= flatCenlus
tallCenlus ::= (CENLUS gap)hoon (gap) hoon (gap) hoon
flatCenlus ::= (CENLUS) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatCenlus ::= (':cnls') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# CENSIG wing hoon hoon
CENSIG ~ [%] [~]
hoon ::= tallCensig
hoonPrimary ::= flatCensig
tallCensig ::= (CENSIG gap)wing (gap) hoon (gap) hoon
flatCensig ::= (CENSIG) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatCensig ::= (':cnsg') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# COLCAB hoon hoon
COLCAB ~ [:] [_]
hoon ::= tallColcab
hoonPrimary ::= flatColcab
tallColcab ::= (COLCAB gap)hoon (gap) hoon
flatColcab ::= (COLCAB) [(] flatHoon (ACE) flatHoon [)]
flatColcab ::= (':clcb') [(] flatHoon (ACE) flatHoon [)]

# COLHEP hoon hoon
COLHEP ~ [:] [-]
hoon ::= tallColhep
hoonPrimary ::= flatColhep
tallColhep ::= (COLHEP gap)hoon (gap) hoon
flatColhep ::= (COLHEP) [(] flatHoon (ACE) flatHoon [)]
flatColhep ::= (':clhp') [(] flatHoon (ACE) flatHoon [)]

# COLLUS hoon hoon hoon
COLLUS ~ [:] [+]
hoon ::= tallCollus
hoonPrimary ::= flatCollus
tallCollus ::= (COLLUS gap)hoon (gap) hoon (gap) hoon
flatCollus ::= (COLLUS) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatCollus ::= (':clls') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# COLKET hoon hoon hoon hoon
COLKET ~ [:] [\^]
hoon ::= tallColket
hoonPrimary ::= flatColket
tallColket ::= (COLKET gap)hoon (gap) hoon (gap) hoon (gap) hoon
flatColket ::= (COLKET) [(] flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatColket ::= (':clkt') [(] flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# DOTTIS hoon hoon
DOTTIS ~ [.] [=]
hoon ::= tallDottis
hoonPrimary ::= flatDottis
tallDottis ::= (DOTTIS gap)hoon (gap) hoon
flatDottis ::= (DOTTIS) [(] flatHoon (ACE) flatHoon [)]
flatDottis ::= (':dtts') [(] flatHoon (ACE) flatHoon [)]

# DOTLUS atom
DOTLUS ~ [.] [+]
hoon ::= tallDotlus
hoonPrimary ::= flatDotlus
tallDotlus ::= (DOTLUS gap)atom
flatDotlus ::= (DOTLUS) [(] atom [)]
flatDotlus ::= (':dtls') [(] atom [)]

# DOTTAR hoon hoon
DOTTAR ~ [.] [*]
hoon ::= tallDottar
hoonPrimary ::= flatDottar
tallDottar ::= (DOTTAR gap)hoon (gap) hoon
flatDottar ::= (DOTTAR) [(] flatHoon (ACE) flatHoon [)]
flatDottar ::= (':dttr') [(] flatHoon (ACE) flatHoon [)]

# DOTWUT hoon
DOTWUT ~ [.] [?]
hoon ::= tallDotwut
hoonPrimary ::= flatDotwut
tallDotwut ::= (DOTWUT gap)hoon
flatDotwut ::= (DOTWUT) [(] flatHoon [)]
flatDotwut ::= (':dtwt') [(] flatHoon [)]

# FASSEM hoon hoon
FASSEM ~ [/] [;]
hoon ::= tallFassem
hoonPrimary ::= flatFassem
tallFassem ::= (FASSEM gap)hoon (gap) hoon
flatFassem ::= (FASSEM) [(] flatHoon (ACE) flatHoon [)]
flatFassem ::= (':fssm') [(] flatHoon (ACE) flatHoon [)]

# KETBAR hoon
KETBAR ~ [\^] [|]
hoon ::= tallKetbar
hoonPrimary ::= flatKetbar
tallKetbar ::= (KETBAR gap)hoon
flatKetbar ::= (KETBAR) [(] flatHoon [)]
flatKetbar ::= (':ktbr') [(] flatHoon [)]

# KETHEP hoon hoon
KETHEP ~ [\^] [-]
hoon ::= tallKethep
hoonPrimary ::= flatKethep
tallKethep ::= (KETHEP gap)hoon (gap) hoon
flatKethep ::= (KETHEP) [(] flatHoon (ACE) flatHoon [)]
flatKethep ::= (':kthp') [(] flatHoon (ACE) flatHoon [)]

# KETLUS hoon hoon
KETLUS ~ [\^] [+]
hoon ::= tallKetlus
hoonPrimary ::= flatKetlus
tallKetlus ::= (KETLUS gap)hoon (gap) hoon
flatKetlus ::= (KETLUS) [(] flatHoon (ACE) flatHoon [)]
flatKetlus ::= (':ktls') [(] flatHoon (ACE) flatHoon [)]

# KETSIG hoon
KETSIG ~ [\^] [~]
hoon ::= tallKetsig
hoonPrimary ::= flatKetsig
tallKetsig ::= (KETSIG gap)hoon
flatKetsig ::= (KETSIG) [(] flatHoon [)]
flatKetsig ::= (':ktsg') [(] flatHoon [)]

# KETTIS toga hoon
KETTIS ~ [\^] [=]
hoon ::= tallKettis
hoonPrimary ::= flatKettis
tallKettis ::= (KETTIS gap)toga (gap) hoon
flatKettis ::= (KETTIS) [(] toga (ACE) flatHoon [)]
flatKettis ::= (':ktts') [(] toga (ACE) flatHoon [)]

# KETWUT hoon
KETWUT ~ [\^] [?]
hoon ::= tallKetwut
hoonPrimary ::= flatKetwut
tallKetwut ::= (KETWUT gap)hoon
flatKetwut ::= (KETWUT) [(] flatHoon [)]
flatKetwut ::= (':ktwt') [(] flatHoon [)]

# SIGBAR hoon hoon
SIGBAR ~ [~] [|]
hoon ::= tallSigbar
hoonPrimary ::= flatSigbar
tallSigbar ::= (SIGBAR gap)hoon (gap) hoon
flatSigbar ::= (SIGBAR) [(] flatHoon (ACE) flatHoon [)]
flatSigbar ::= (':sgbr') [(] flatHoon (ACE) flatHoon [)]

# SIGBUC term hoon
SIGBUC ~ [~] [$]
hoon ::= tallSigbuc
hoonPrimary ::= flatSigbuc
tallSigbuc ::= (SIGBUC gap)term (gap) hoon
flatSigbuc ::= (SIGBUC) [(] term (ACE) flatHoon [)]
flatSigbuc ::= (':sgbc') [(] term (ACE) flatHoon [)]

# SIGCEN term wing hoon hoon
SIGCEN ~ [~] [%]
hoon ::= tallSigcen
hoonPrimary ::= flatSigcen
tallSigcen ::= (SIGCEN gap)term (gap) wing (gap) hoon (gap) hoon
flatSigcen ::= (SIGCEN) [(] term (ACE) wing (ACE) flatHoon (ACE) flatHoon [)]
flatSigcen ::= (':sgcn') [(] term (ACE) wing (ACE) flatHoon (ACE) flatHoon [)]

# SIGFAS term hoon
SIGFAS ~ [~] [/]
hoon ::= tallSigfas
hoonPrimary ::= flatSigfas
tallSigfas ::= (SIGFAS gap)term (gap) hoon
flatSigfas ::= (SIGFAS) [(] term (ACE) flatHoon [)]
flatSigfas ::= (':sgfs') [(] term (ACE) flatHoon [)]

# SIGGAL hoon hoon
SIGGAL ~ [~] [<]
hoon ::= tallSiggal
hoonPrimary ::= flatSiggal
tallSiggal ::= (SIGGAL gap)hoon (gap) hoon
flatSiggal ::= (SIGGAL) [(] flatHoon (ACE) flatHoon [)]
flatSiggal ::= (':sggl') [(] flatHoon (ACE) flatHoon [)]

# SIGGAR hoon hoon
SIGGAR ~ [~] [>]
hoon ::= tallSiggar
hoonPrimary ::= flatSiggar
tallSiggar ::= (SIGGAR gap)hoon (gap) hoon
flatSiggar ::= (SIGGAR) [(] flatHoon (ACE) flatHoon [)]
flatSiggar ::= (':sggr') [(] flatHoon (ACE) flatHoon [)]

# SIGLUS hoon
SIGLUS ~ [~] [+]
hoon ::= tallSiglus
hoonPrimary ::= flatSiglus
tallSiglus ::= (SIGLUS gap)hoon
flatSiglus ::= (SIGLUS) [(] flatHoon [)]
flatSiglus ::= (':sgls') [(] flatHoon [)]

# SIGPAM hoon hoon
SIGPAM ~ [~] [&]
hoon ::= tallSigpam
hoonPrimary ::= flatSigpam
tallSigpam ::= (SIGPAM gap)hoon (gap) hoon
flatSigpam ::= (SIGPAM) [(] flatHoon (ACE) flatHoon [)]
flatSigpam ::= (':sgpm') [(] flatHoon (ACE) flatHoon [)]

# SEMSEM hoon hoon
SEMSEM ~ [;] [;]
hoon ::= tallSemsem
hoonPrimary ::= flatSemsem
tallSemsem ::= (SEMSEM gap)hoon (gap) hoon
flatSemsem ::= (SEMSEM) [(] flatHoon (ACE) flatHoon [)]
flatSemsem ::= (':smsm') [(] flatHoon (ACE) flatHoon [)]

# SIGCAB hoon hoon
SIGCAB ~ [~] [_]
hoon ::= tallSigcab
hoonPrimary ::= flatSigcab
tallSigcab ::= (SIGCAB gap)hoon (gap) hoon
flatSigcab ::= (SIGCAB) [(] flatHoon (ACE) flatHoon [)]
flatSigcab ::= (':sgcb') [(] flatHoon (ACE) flatHoon [)]

# SIGWUT hoon hoon hoon
SIGWUT ~ [~] [?]
hoon ::= tallSigwut
hoonPrimary ::= flatSigwut
tallSigwut ::= (SIGWUT gap)hoon (gap) hoon (gap) hoon
flatSigwut ::= (SIGWUT) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatSigwut ::= (':sgwt') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# SIGZAP hoon hoon
SIGZAP ~ [~] [!]
hoon ::= tallSigzap
hoonPrimary ::= flatSigzap
tallSigzap ::= (SIGZAP gap)hoon (gap) hoon
flatSigzap ::= (SIGZAP) [(] flatHoon (ACE) flatHoon [)]
flatSigzap ::= (':sgzp') [(] flatHoon (ACE) flatHoon [)]

# TISBAR hoon hoon
TISBAR ~ [=] [|]
hoon ::= tallTisbar
hoonPrimary ::= flatTisbar
tallTisbar ::= (TISBAR gap)hoon (gap) hoon
flatTisbar ::= (TISBAR) [(] flatHoon (ACE) flatHoon [)]
flatTisbar ::= (':tsbr') [(] flatHoon (ACE) flatHoon [)]

# TISCOM hoon hoon
TISCOM ~ [=] [,]
hoon ::= tallTiscom
hoonPrimary ::= flatTiscom
tallTiscom ::= (TISCOM gap)hoon (gap) hoon
flatTiscom ::= (TISCOM) [(] flatHoon (ACE) flatHoon [)]
flatTiscom ::= (':tscm') [(] flatHoon (ACE) flatHoon [)]

# TISDOT wing hoon hoon
TISDOT ~ [=] [.]
hoon ::= tallTisdot
hoonPrimary ::= flatTisdot
tallTisdot ::= (TISDOT gap)wing (gap) hoon (gap) hoon
flatTisdot ::= (TISDOT) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatTisdot ::= (':tsdt') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# TISHEP hoon hoon
TISHEP ~ [=] [-]
hoon ::= tallTishep
hoonPrimary ::= flatTishep
tallTishep ::= (TISHEP gap)hoon (gap) hoon
flatTishep ::= (TISHEP) [(] flatHoon (ACE) flatHoon [)]
flatTishep ::= (':tshp') [(] flatHoon (ACE) flatHoon [)]

# TISFAS hoon hoon hoon
TISFAS ~ [=] [/]
hoon ::= tallTisfas
hoonPrimary ::= flatTisfas
tallTisfas ::= (TISFAS gap)hoon (gap) hoon (gap) hoon
flatTisfas ::= (TISFAS) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatTisfas ::= (':tsfs') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# TISGAL hoon hoon
TISGAL ~ [=] [<]
hoon ::= tallTisgal
hoonPrimary ::= flatTisgal
tallTisgal ::= (TISGAL gap)hoon (gap) hoon
flatTisgal ::= (TISGAL) [(] flatHoon (ACE) flatHoon [)]
flatTisgal ::= (':tsgl') [(] flatHoon (ACE) flatHoon [)]

# TISGAR hoon hoon
TISGAR ~ [=] [>]
hoon ::= tallTisgar
hoonPrimary ::= flatTisgar
tallTisgar ::= (TISGAR gap)hoon (gap) hoon
flatTisgar ::= (TISGAR) [(] flatHoon (ACE) flatHoon [)]
flatTisgar ::= (':tsgr') [(] flatHoon (ACE) flatHoon [)]

# TISKET hoon wing hoon hoon
TISKET ~ [=] [\^]
hoon ::= tallTisket
hoonPrimary ::= flatTisket
tallTisket ::= (TISKET gap)hoon (gap) wing (gap) hoon (gap) hoon
flatTisket ::= (TISKET) [(] flatHoon (ACE) wing (ACE) flatHoon (ACE) flatHoon [)]
flatTisket ::= (':tskt') [(] flatHoon (ACE) wing (ACE) flatHoon (ACE) flatHoon [)]

# TISLUS hoon hoon
TISLUS ~ [=] [+]
hoon ::= tallTislus
hoonPrimary ::= flatTislus
tallTislus ::= (TISLUS gap)hoon (gap) hoon
flatTislus ::= (TISLUS) [(] flatHoon (ACE) flatHoon [)]
flatTislus ::= (':tsls') [(] flatHoon (ACE) flatHoon [)]

# TISSEM hoon hoon hoon
TISSEM ~ [=] [;]
hoon ::= tallTissem
hoonPrimary ::= flatTissem
tallTissem ::= (TISSEM gap)hoon (gap) hoon (gap) hoon
flatTissem ::= (TISSEM) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatTissem ::= (':tssm') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# TISTAR term hoon hoon
TISTAR ~ [=] [*]
hoon ::= tallTistar
hoonPrimary ::= flatTistar
tallTistar ::= (TISTAR gap)term (gap) hoon (gap) hoon
flatTistar ::= (TISTAR) [(] term (ACE) flatHoon (ACE) flatHoon [)]
flatTistar ::= (':tstr') [(] term (ACE) flatHoon (ACE) flatHoon [)]

# TISWUT wing hoon hoon hoon
TISWUT ~ [=] [?]
hoon ::= tallTiswut
hoonPrimary ::= flatTiswut
tallTiswut ::= (TISWUT gap)wing (gap) hoon (gap) hoon (gap) hoon
flatTiswut ::= (TISWUT) [(] wing (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatTiswut ::= (':tswt') [(] wing (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# WUTCOL hoon hoon hoon
WUTCOL ~ [?] [:]
hoon ::= tallWutcol
hoonPrimary ::= flatWutcol
tallWutcol ::= (WUTCOL gap)hoon (gap) hoon (gap) hoon
flatWutcol ::= (WUTCOL) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatWutcol ::= (':wtcl') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# WUTDOT hoon hoon hoon
WUTDOT ~ [?] [.]
hoon ::= tallWutdot
hoonPrimary ::= flatWutdot
tallWutdot ::= (WUTDOT gap)hoon (gap) hoon (gap) hoon
flatWutdot ::= (WUTDOT) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatWutdot ::= (':wtdt') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# WUTGAL hoon hoon
WUTGAL ~ [?] [<]
hoon ::= tallWutgal
hoonPrimary ::= flatWutgal
tallWutgal ::= (WUTGAL gap)hoon (gap) hoon
flatWutgal ::= (WUTGAL) [(] flatHoon (ACE) flatHoon [)]
flatWutgal ::= (':wtgl') [(] flatHoon (ACE) flatHoon [)]

# WUTGAR hoon hoon
WUTGAR ~ [?] [>]
hoon ::= tallWutgar
hoonPrimary ::= flatWutgar
tallWutgar ::= (WUTGAR gap)hoon (gap) hoon
flatWutgar ::= (WUTGAR) [(] flatHoon (ACE) flatHoon [)]
flatWutgar ::= (':wtgr') [(] flatHoon (ACE) flatHoon [)]

# WUTZAP hoon
WUTZAP ~ [?] [!]
hoon ::= tallWutzap
hoonPrimary ::= flatWutzap
tallWutzap ::= (WUTZAP gap)hoon
flatWutzap ::= (WUTZAP) [(] flatHoon [)]
flatWutzap ::= (':wtzp') [(] flatHoon [)]

# WUTKET wing hoon hoon
WUTKET ~ [?] [\^]
hoon ::= tallWutket
hoonPrimary ::= flatWutket
tallWutket ::= (WUTKET gap)wing (gap) hoon (gap) hoon
flatWutket ::= (WUTKET) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatWutket ::= (':wtkt') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# WUTPAT wing hoon hoon
WUTPAT ~ [?] [@]
hoon ::= tallWutpat
hoonPrimary ::= flatWutpat
tallWutpat ::= (WUTPAT gap)wing (gap) hoon (gap) hoon
flatWutpat ::= (WUTPAT) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatWutpat ::= (':wtpt') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# WUTSIG wing hoon hoon
WUTSIG ~ [?] [~]
hoon ::= tallWutsig
hoonPrimary ::= flatWutsig
tallWutsig ::= (WUTSIG gap)wing (gap) hoon (gap) hoon
flatWutsig ::= (WUTSIG) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatWutsig ::= (':wtsg') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# WUTTIS hoon wing
WUTTIS ~ [?] [=]
hoon ::= tallWuttis
hoonPrimary ::= flatWuttis
tallWuttis ::= (WUTTIS gap)hoon (gap) wing
flatWuttis ::= (WUTTIS) [(] flatHoon (ACE) wing [)]
flatWuttis ::= (':wtts') [(] flatHoon (ACE) wing [)]

# ZAPGAR hoon
ZAPGAR ~ [!] [>]
hoon ::= tallZapgar
hoonPrimary ::= flatZapgar
tallZapgar ::= (ZAPGAR gap)hoon
flatZapgar ::= (ZAPGAR) [(] flatHoon [)]
flatZapgar ::= (':zpgr') [(] flatHoon [)]

# ZAPTIS hoon
ZAPTIS ~ [!] [=]
hoon ::= tallZaptis
hoonPrimary ::= flatZaptis
tallZaptis ::= (ZAPTIS gap)hoon
flatZaptis ::= (ZAPTIS) [(] flatHoon [)]
flatZaptis ::= (':zpts') [(] flatHoon [)]

# ZAPWUT atom hoon
ZAPWUT ~ [!] [?]
hoon ::= tallZapwut
hoonPrimary ::= flatZapwut
tallZapwut ::= (ZAPWUT gap)atom (gap) hoon
flatZapwut ::= (ZAPWUT) [(] atom (ACE) flatHoon [)]
flatZapwut ::= (':zpwt') [(] atom (ACE) flatHoon [)]

