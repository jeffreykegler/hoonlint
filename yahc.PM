use 5.010;
use strict;
use warnings;
use English qw( -no_match_vars );

# === DSL Piece: "Do not edit" warning ===
my $dsl_warning = <<END_OF_WARNING;
# !!! DO NOT EDIT !!!
# This code automatically written by $PROGRAM_NAME

END_OF_WARNING

# === DSL Piece: the main body ===

# The "FIXED:" comments lines are descriptons of the fixed length runes
# (1-fixed, 2-fixed, 3-fixed and 4-fixed) for auto-generation
# of Marpa rules for the various regular formats, both
# tall and flat.
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

# === HOON FILE ===
:start ::= hoonFile
# LATER: This is a simplication, which does not
# catch all the subtleties of "ford" files
hoonFile ::= (leader) hoonSeq (trailer)

trailer ::= optWs
leader  ::= optWs

# === CHARACTER SET ===

# BACKSLASH ~ backslash
backslash ~ [\0x5c] # 0x5c is backslash
KET ~ '^'

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
# 0x5C is backslash
# From syntax.vim, might need correction
doubleStringChar ~ [^"\x5c{] | backslash ["] | backslash backslash
stringInterpolation ::= '{' flatHoon '}'

# LATER Single string element also allow escapes
# LATER: Add \xx hex escapes, and more backslash escapes
SINGLESTRING ~ ['] singleStringElements [']
singleStringElements ~ singleStringElement*
# 0x5C is backslash
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

# pathHoon is hoon that is legal as part of a path
pathElement ::= pathHoon

PATHSTRING ~ pathStringElements
pathStringElements ~ pathStringElement
# Do path strings allow escapes?
pathStringElement ~ [^/]

# === CELLS BY TYPE ==

hoonSeq ::= hoon+ separator=>gap proper=>1
hoon ::= flatHoon
flatHoon ::= hoonExpression
hoonExpression ::= infixColon
hoonExpression ::= infixSlash
hoonExpression ::= infixPlus
hoonExpression ::= infixEqual
hoonExpression ::= hoonPrimary
infixColon ::= hoonPrimary (':') flatHoon
infixEqual ::= toga ('=') hoonExpression
infixPlus ::=  NAME ('+') hoonExpression
infixSlash ::= NAME ('/') hoonExpression

hoonPrimary ::= pathHoon

hoonPrimary ::= atom
hoonPrimary ::= wing

toga ::= NAME
toga ::= '[' togaSeq ']'
togaSeq ::= togaElement+ separator=>ACE proper=>1
togaElement ::= toga
togaElement ::= NIL

wing ::= limb+ separator=>[.] proper=>1
limb ::= optKets NAME
optKets ::= KET*
limb ::= lark
lark ::= '.'
lark ::= [+&|] NUMBER
lark ::= carCdr
lark ::= carCdrPairs
lark ::= carCdrPairs carCdr
carCdrPairs ::= carCdrPair+
# The [-+] and [<>] syntax alternates for readability
carCdrPair ::= [-+][<>]
carCdr ::= [-+]

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
flatHoon  ::= irrBuccab
irrBuccab ::= ('_') flatHoon

# Running syntax
BUCCOL ~ [$] [:]
hoon ::= tallBuccol
tallBuccol ::= (BUCCOL gap) hoonSeq (gap '==')
hoonPrimary ::= flatBuccol
flatBuccol ::= (BUCCOL '(') flatHoonSeq (')')
flatBuccol ::= (':bccl(') flatHoonSeq (')')
flatBuccol ::= ('{') flatHoonSeq ('}')
flatBuccol ::= ('[') flatHoonSeq (']')
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

# Undocumented runes
# $*  ::  bunt (irregular form is *)
# FIXED: buctar hoon
flatHoon ::= irrBuctar
irrBuctar ::= '*' flatHoon

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
tallCentis ::= CENTIS (gap) hoon (gap) hoonJogging (gap '==')
irrCentis ::= NAME ('(') flatHoonJogging (')')

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

flatWutzap ::= irrWutzap
irrWutzap ::= ('!') flatHoon

WUTHEP ~ [?] [-]
hoon ::= tallWuthep
tallWuthep ::= WUTHEP (gap) wing (gap) hoonJogging (gap '==')

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
hoon ::= tallZapzap
tallZapzap ::= ZAPZAP

# ^.  ::  use gate to transform type
# ^&  ::  zinc (covariant) -- see the docs on advanced types

# === IRREGULAR FORMS ===

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
# for the tall and the 2 regular flat forms.
sub doFixedRune {
    my ($runeName, @samples) = @_;
    my @result = (join ' ', '#', (uc $runeName), @samples);
    my $glyphname1 = substr($runeName, 0, 3);
    my $glyphname2 = substr($runeName, 3, 3);
    my $glyph1 = $glyphs{$glyphname1} or die "no glyph for $glyphname1";
    my $glyph2 = $glyphs{$glyphname2};
    my $lexeme = uc $runeName;
    my $tallLHS = 'tall' . ucfirst $runeName;
    my $flatLHS = 'flat' . ucfirst $runeName;
    # BARHEP ~ [|] [-]
    push @result, $lexeme . q{ ~ [} . $glyph1 . q{] [} . $glyph2 . q{]};
    # hoon ::= tallBarhep
    push @result, 'hoon ::= ' . $tallLHS;
    # hoonPrimary ::= flatBarhep
    push @result, 'hoonPrimary ::= ' . $flatLHS;
    # tallBarhep ::= (BARHEP gap) hoon (gap) hoon
    push @result, $tallLHS . ' ::= (' . $lexeme . ' gap)' . (join ' (gap) ', @samples);
    state $flatEquiv = {
        hoon => 'flatHoon',
    };
    my @flatSamples = map { $flatEquiv->{$_} // $_; } @samples;
    # flatBarhep ::= ([|] [-]) (ACE) flatHoon (ACE) flatHoon
    push @result, $flatLHS . ' ::= (' . $lexeme . ') [(] ' . (join ' (ACE) ', @flatSamples) . q{ [)]};
    my $regularName = join '', ':',
      substr($glyphname1, 0, 1), substr($glyphname1, 2, 1),
      substr($glyphname2, 0, 1), substr($glyphname2, 2, 1);
    # flatBarhep ::= ([:] 'brhp') (ACE) flatHoon (ACE) flatHoon
    push @result, $flatLHS . q{ ::= ('} . $regularName . q{') [(] } . (join ' (ACE) ', @flatSamples) . q{ [)]};
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


