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
    };

    state $nonSemantic = {
        flatHoon        => 1,
        flatHoonJogging => 1,
        flatHoonJogs    => 1,
        flatHoonSeq     => 1,
        hoon            => 1,
	hoonFile => 1,
        hoonJog     => 1,
        hoonJogging => 1,
        hoonJogs    => 1,
        hoonSeq         => 1,
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

TERM ~ '%' firstTermChar
TERM ~ '%' firstTermChar optMedialTermChars lastTermChar
firstTermChar ~ [a-z]
optMedialTermChars ~ medialTermChar*
medialTermChar ~ [a-z0-9-]
lastTermChar ~ [a-z0-9]

# === STRINGS ==

atom ::= STRING

# LATER: Add \xx hex escapes, and more backslash escapes
# LATER: See https://urbit.org/docs/hoon/atom/knit/ for interpolation
STRING ~ ["] doubleStringElements ["]
doubleStringElements ~ doubleStringElement*
# 0x5C is backslash
# From syntax.vim, might need correction
doubleStringElement ~ [^"\x5c] | backslash ["] | backslash backslash

# LATER Single string element -- they also allow escapes

# syn region      hoonString        start=+'+ skip=+\\[\\']+ end=+'+ contains=@spell
# syn region      hoonBlock         start=+'''+ end=+'''+
# syn region      hoonString        start=+"+ skip=+\\[\\"]+ end=+"+ contains=@spell

# === CELLS BY TYPE ==

hoonSeq ::= hoon+ separator=>gap proper=>1
hoon ::= flatHoon

# flatHoons ::= flatHoon+
flatHoon ::= atom
flatHoon ::= wing

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
flatHoon ::= flatBuccol
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

# FIXED: buctis term hoon
flatHoon ::= irrBuctisSlash
irrBuctisSlash ::= NAME ('/') hoon

# TODO: what is ace-separated slash -- another form of buctis?
# Or something else?
# irrBuctisSlash ::= NAME (ACE '/' ACE) hoon

BUCWUT ~ [$] [?]
hoon ::= tallBucwut
tallBucwut ::= (BUCWUT gap) hoonSeq (gap '==')
flatHoon ::= flatBucwut
flatBucwut ::= (BUCWUT '(') flatHoonSeq (')')
flatBucwut ::= (':bcwt(') flatHoonSeq (')')
flatBucwut ::= ('?(') flatHoonSeq (')')

# TODO: From clay file system?
# FIXED: cenbar hoon hoon

# FIXED: cendot hoon hoon

# FIXED: cenhep hoon hoon
flatHoon ::= irrCenhep
# See https://raw.githubusercontent.com/urbit/old-urbit.org/master/doc/hoon/lan/irregular.markdown
# and cenhep in https://urbit.org/docs/hoon/irregular/
irrCenhep ::= ('(') flatHoonSeq (')')

# FIXED: cenket hoon hoon hoon hoon
# FIXED: cenlus hoon hoon hoon

# TODO: From clay file system?
# FIXED: cenpam hoon

# FIXED: censig wing hoon hoon
flatHoon ::= irrCensig
irrCensig ::= ('~(') flatHoonSeq (')')

# FIXED: colcab hoon hoon
# FIXED: colhep hoon hoon
# FIXED: collus hoon hoon hoon
# FIXED: colket hoon hoon hoon hoon

flatHoon ::= irrCentis
hoon ::= tallCentis
CENTIS ~ [%] [=]
tallCentis ::= CENTIS (gap) hoon (gap) hoonJogging (gap '==')
irrCentis ::= NAME ('(') flatHoonJogging (')')

# FIXED: dotket hoon hoon

# FIXED: dottis hoon hoon
flatHoon ::= irrDottis
irrDottis ::= ('=(') flatHoon (ACE) flatHoon (')')

# FIXED: dotlus atom
flatHoon ::= irrDotlus
irrDotlus ::= ('+(') flatHoon (')')

# FIXED: dottar hoon hoon
# FIXED: dotwut hoon

# FAS group are (usually?) ford runes:

FASTIS ~ [\/] [=]
hoon ::= tallFastis
tallFastis ::= (FASTIS gap) NAME (gap) hoon 
flatHoon ::= flatFastis
flatFastis ::= (FASTIS) NAME '=' hoon

# FIXED: ketbar hoon
# FIXED: kethep hoon hoon
# FIXED: ketlus hoon hoon
# FIXED: ketsig hoon

# FIXED: kettis toga hoon
flatHoon ::= irrKettis
irrKettis ::= toga ('=') flatHoon

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
flatHoon ::= irrTisgal
irrTisgal ::= flatHoon (':') flatHoon

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
flatHoon ::= flatWutbar
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
flatHoon ::= flatWutpam
flatWutpam ::= (WUTPAM '(') flatHoonSeq (')')
flatWutpam ::= (':wtpm(') flatHoonSeq (')')
flatWutpam ::= ('&(') flatHoonSeq (')')

# FIXED: wutpat wing hoon hoon
# FIXED: wutsig wing hoon hoon
# FIXED: wuttis hoon wing

flatWutzap ::= irrWutzap
irrWutzap ::= ('!') flatHoon

hoon ::= tallWuthep
WUTHEP ~ [?] [-]
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

NAME ~ name
name ~ nameFirstChar nameLaterChars
name ~ '$'

nameFirstChar ~ [a-z]
nameLaterChars ~ nameLaterChar*
nameLaterChar ~ [a-z0-9-]

# BARCOL hoon hoon
BARCOL ~ [|] [:]
hoon ::= tallBarcol
flatHoon ::= flatBarcol
tallBarcol ::= (BARCOL gap)hoon (gap) hoon
flatBarcol ::= (BARCOL) [(] flatHoon (ACE) flatHoon [)]
flatBarcol ::= (':brcl') [(] flatHoon (ACE) flatHoon [)]

# BARDOT hoon
BARDOT ~ [|] [.]
hoon ::= tallBardot
flatHoon ::= flatBardot
tallBardot ::= (BARDOT gap)hoon
flatBardot ::= (BARDOT) [(] flatHoon [)]
flatBardot ::= (':brdt') [(] flatHoon [)]

# BARHEP hoon
BARHEP ~ [|] [-]
hoon ::= tallBarhep
flatHoon ::= flatBarhep
tallBarhep ::= (BARHEP gap)hoon
flatBarhep ::= (BARHEP) [(] flatHoon [)]
flatBarhep ::= (':brhp') [(] flatHoon [)]

# BARSIG hoon hoon
BARSIG ~ [|] [~]
hoon ::= tallBarsig
flatHoon ::= flatBarsig
tallBarsig ::= (BARSIG gap)hoon (gap) hoon
flatBarsig ::= (BARSIG) [(] flatHoon (ACE) flatHoon [)]
flatBarsig ::= (':brsg') [(] flatHoon (ACE) flatHoon [)]

# BARTAR hoon hoon
BARTAR ~ [|] [*]
hoon ::= tallBartar
flatHoon ::= flatBartar
tallBartar ::= (BARTAR gap)hoon (gap) hoon
flatBartar ::= (BARTAR) [(] flatHoon (ACE) flatHoon [)]
flatBartar ::= (':brtr') [(] flatHoon (ACE) flatHoon [)]

# BARTIS hoon hoon
BARTIS ~ [|] [=]
hoon ::= tallBartis
flatHoon ::= flatBartis
tallBartis ::= (BARTIS gap)hoon (gap) hoon
flatBartis ::= (BARTIS) [(] flatHoon (ACE) flatHoon [)]
flatBartis ::= (':brts') [(] flatHoon (ACE) flatHoon [)]

# BARWUT hoon
BARWUT ~ [|] [?]
hoon ::= tallBarwut
flatHoon ::= flatBarwut
tallBarwut ::= (BARWUT gap)hoon
flatBarwut ::= (BARWUT) [(] flatHoon [)]
flatBarwut ::= (':brwt') [(] flatHoon [)]

# BUCCAB hoon
BUCCAB ~ [$] [_]
hoon ::= tallBuccab
flatHoon ::= flatBuccab
tallBuccab ::= (BUCCAB gap)hoon
flatBuccab ::= (BUCCAB) [(] flatHoon [)]
flatBuccab ::= (':bccb') [(] flatHoon [)]

# BUCHEP hoon hoon
BUCHEP ~ [$] [-]
hoon ::= tallBuchep
flatHoon ::= flatBuchep
tallBuchep ::= (BUCHEP gap)hoon (gap) hoon
flatBuchep ::= (BUCHEP) [(] flatHoon (ACE) flatHoon [)]
flatBuchep ::= (':bchp') [(] flatHoon (ACE) flatHoon [)]

# BUCKET hoon hoon
BUCKET ~ [$] [\^]
hoon ::= tallBucket
flatHoon ::= flatBucket
tallBucket ::= (BUCKET gap)hoon (gap) hoon
flatBucket ::= (BUCKET) [(] flatHoon (ACE) flatHoon [)]
flatBucket ::= (':bckt') [(] flatHoon (ACE) flatHoon [)]

# BUCPAT hoon hoon
BUCPAT ~ [$] [@]
hoon ::= tallBucpat
flatHoon ::= flatBucpat
tallBucpat ::= (BUCPAT gap)hoon (gap) hoon
flatBucpat ::= (BUCPAT) [(] flatHoon (ACE) flatHoon [)]
flatBucpat ::= (':bcpt') [(] flatHoon (ACE) flatHoon [)]

# BUCTIS term hoon
BUCTIS ~ [$] [=]
hoon ::= tallBuctis
flatHoon ::= flatBuctis
tallBuctis ::= (BUCTIS gap)term (gap) hoon
flatBuctis ::= (BUCTIS) [(] term (ACE) flatHoon [)]
flatBuctis ::= (':bcts') [(] term (ACE) flatHoon [)]

# CENBAR hoon hoon
CENBAR ~ [%] [|]
hoon ::= tallCenbar
flatHoon ::= flatCenbar
tallCenbar ::= (CENBAR gap)hoon (gap) hoon
flatCenbar ::= (CENBAR) [(] flatHoon (ACE) flatHoon [)]
flatCenbar ::= (':cnbr') [(] flatHoon (ACE) flatHoon [)]

# CENDOT hoon hoon
CENDOT ~ [%] [.]
hoon ::= tallCendot
flatHoon ::= flatCendot
tallCendot ::= (CENDOT gap)hoon (gap) hoon
flatCendot ::= (CENDOT) [(] flatHoon (ACE) flatHoon [)]
flatCendot ::= (':cndt') [(] flatHoon (ACE) flatHoon [)]

# CENHEP hoon hoon
CENHEP ~ [%] [-]
hoon ::= tallCenhep
flatHoon ::= flatCenhep
tallCenhep ::= (CENHEP gap)hoon (gap) hoon
flatCenhep ::= (CENHEP) [(] flatHoon (ACE) flatHoon [)]
flatCenhep ::= (':cnhp') [(] flatHoon (ACE) flatHoon [)]

# CENKET hoon hoon hoon hoon
CENKET ~ [%] [\^]
hoon ::= tallCenket
flatHoon ::= flatCenket
tallCenket ::= (CENKET gap)hoon (gap) hoon (gap) hoon (gap) hoon
flatCenket ::= (CENKET) [(] flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatCenket ::= (':cnkt') [(] flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# CENLUS hoon hoon hoon
CENLUS ~ [%] [+]
hoon ::= tallCenlus
flatHoon ::= flatCenlus
tallCenlus ::= (CENLUS gap)hoon (gap) hoon (gap) hoon
flatCenlus ::= (CENLUS) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatCenlus ::= (':cnls') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# CENPAM hoon
CENPAM ~ [%] [&]
hoon ::= tallCenpam
flatHoon ::= flatCenpam
tallCenpam ::= (CENPAM gap)hoon
flatCenpam ::= (CENPAM) [(] flatHoon [)]
flatCenpam ::= (':cnpm') [(] flatHoon [)]

# CENSIG wing hoon hoon
CENSIG ~ [%] [~]
hoon ::= tallCensig
flatHoon ::= flatCensig
tallCensig ::= (CENSIG gap)wing (gap) hoon (gap) hoon
flatCensig ::= (CENSIG) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatCensig ::= (':cnsg') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# COLCAB hoon hoon
COLCAB ~ [:] [_]
hoon ::= tallColcab
flatHoon ::= flatColcab
tallColcab ::= (COLCAB gap)hoon (gap) hoon
flatColcab ::= (COLCAB) [(] flatHoon (ACE) flatHoon [)]
flatColcab ::= (':clcb') [(] flatHoon (ACE) flatHoon [)]

# COLHEP hoon hoon
COLHEP ~ [:] [-]
hoon ::= tallColhep
flatHoon ::= flatColhep
tallColhep ::= (COLHEP gap)hoon (gap) hoon
flatColhep ::= (COLHEP) [(] flatHoon (ACE) flatHoon [)]
flatColhep ::= (':clhp') [(] flatHoon (ACE) flatHoon [)]

# COLLUS hoon hoon hoon
COLLUS ~ [:] [+]
hoon ::= tallCollus
flatHoon ::= flatCollus
tallCollus ::= (COLLUS gap)hoon (gap) hoon (gap) hoon
flatCollus ::= (COLLUS) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatCollus ::= (':clls') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# COLKET hoon hoon hoon hoon
COLKET ~ [:] [\^]
hoon ::= tallColket
flatHoon ::= flatColket
tallColket ::= (COLKET gap)hoon (gap) hoon (gap) hoon (gap) hoon
flatColket ::= (COLKET) [(] flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatColket ::= (':clkt') [(] flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# DOTKET hoon hoon
DOTKET ~ [.] [\^]
hoon ::= tallDotket
flatHoon ::= flatDotket
tallDotket ::= (DOTKET gap)hoon (gap) hoon
flatDotket ::= (DOTKET) [(] flatHoon (ACE) flatHoon [)]
flatDotket ::= (':dtkt') [(] flatHoon (ACE) flatHoon [)]

# DOTTIS hoon hoon
DOTTIS ~ [.] [=]
hoon ::= tallDottis
flatHoon ::= flatDottis
tallDottis ::= (DOTTIS gap)hoon (gap) hoon
flatDottis ::= (DOTTIS) [(] flatHoon (ACE) flatHoon [)]
flatDottis ::= (':dtts') [(] flatHoon (ACE) flatHoon [)]

# DOTLUS atom
DOTLUS ~ [.] [+]
hoon ::= tallDotlus
flatHoon ::= flatDotlus
tallDotlus ::= (DOTLUS gap)atom
flatDotlus ::= (DOTLUS) [(] atom [)]
flatDotlus ::= (':dtls') [(] atom [)]

# DOTTAR hoon hoon
DOTTAR ~ [.] [*]
hoon ::= tallDottar
flatHoon ::= flatDottar
tallDottar ::= (DOTTAR gap)hoon (gap) hoon
flatDottar ::= (DOTTAR) [(] flatHoon (ACE) flatHoon [)]
flatDottar ::= (':dttr') [(] flatHoon (ACE) flatHoon [)]

# DOTWUT hoon
DOTWUT ~ [.] [?]
hoon ::= tallDotwut
flatHoon ::= flatDotwut
tallDotwut ::= (DOTWUT gap)hoon
flatDotwut ::= (DOTWUT) [(] flatHoon [)]
flatDotwut ::= (':dtwt') [(] flatHoon [)]

# KETBAR hoon
KETBAR ~ [\^] [|]
hoon ::= tallKetbar
flatHoon ::= flatKetbar
tallKetbar ::= (KETBAR gap)hoon
flatKetbar ::= (KETBAR) [(] flatHoon [)]
flatKetbar ::= (':ktbr') [(] flatHoon [)]

# KETHEP hoon hoon
KETHEP ~ [\^] [-]
hoon ::= tallKethep
flatHoon ::= flatKethep
tallKethep ::= (KETHEP gap)hoon (gap) hoon
flatKethep ::= (KETHEP) [(] flatHoon (ACE) flatHoon [)]
flatKethep ::= (':kthp') [(] flatHoon (ACE) flatHoon [)]

# KETLUS hoon hoon
KETLUS ~ [\^] [+]
hoon ::= tallKetlus
flatHoon ::= flatKetlus
tallKetlus ::= (KETLUS gap)hoon (gap) hoon
flatKetlus ::= (KETLUS) [(] flatHoon (ACE) flatHoon [)]
flatKetlus ::= (':ktls') [(] flatHoon (ACE) flatHoon [)]

# KETSIG hoon
KETSIG ~ [\^] [~]
hoon ::= tallKetsig
flatHoon ::= flatKetsig
tallKetsig ::= (KETSIG gap)hoon
flatKetsig ::= (KETSIG) [(] flatHoon [)]
flatKetsig ::= (':ktsg') [(] flatHoon [)]

# KETTIS toga hoon
KETTIS ~ [\^] [=]
hoon ::= tallKettis
flatHoon ::= flatKettis
tallKettis ::= (KETTIS gap)toga (gap) hoon
flatKettis ::= (KETTIS) [(] toga (ACE) flatHoon [)]
flatKettis ::= (':ktts') [(] toga (ACE) flatHoon [)]

# KETWUT hoon
KETWUT ~ [\^] [?]
hoon ::= tallKetwut
flatHoon ::= flatKetwut
tallKetwut ::= (KETWUT gap)hoon
flatKetwut ::= (KETWUT) [(] flatHoon [)]
flatKetwut ::= (':ktwt') [(] flatHoon [)]

# SIGBAR hoon hoon
SIGBAR ~ [~] [|]
hoon ::= tallSigbar
flatHoon ::= flatSigbar
tallSigbar ::= (SIGBAR gap)hoon (gap) hoon
flatSigbar ::= (SIGBAR) [(] flatHoon (ACE) flatHoon [)]
flatSigbar ::= (':sgbr') [(] flatHoon (ACE) flatHoon [)]

# SIGBUC term hoon
SIGBUC ~ [~] [$]
hoon ::= tallSigbuc
flatHoon ::= flatSigbuc
tallSigbuc ::= (SIGBUC gap)term (gap) hoon
flatSigbuc ::= (SIGBUC) [(] term (ACE) flatHoon [)]
flatSigbuc ::= (':sgbc') [(] term (ACE) flatHoon [)]

# SIGCEN term wing hoon hoon
SIGCEN ~ [~] [%]
hoon ::= tallSigcen
flatHoon ::= flatSigcen
tallSigcen ::= (SIGCEN gap)term (gap) wing (gap) hoon (gap) hoon
flatSigcen ::= (SIGCEN) [(] term (ACE) wing (ACE) flatHoon (ACE) flatHoon [)]
flatSigcen ::= (':sgcn') [(] term (ACE) wing (ACE) flatHoon (ACE) flatHoon [)]

# SIGFAS term hoon
SIGFAS ~ [~] [/]
hoon ::= tallSigfas
flatHoon ::= flatSigfas
tallSigfas ::= (SIGFAS gap)term (gap) hoon
flatSigfas ::= (SIGFAS) [(] term (ACE) flatHoon [)]
flatSigfas ::= (':sgfs') [(] term (ACE) flatHoon [)]

# SIGGAL hoon hoon
SIGGAL ~ [~] [<]
hoon ::= tallSiggal
flatHoon ::= flatSiggal
tallSiggal ::= (SIGGAL gap)hoon (gap) hoon
flatSiggal ::= (SIGGAL) [(] flatHoon (ACE) flatHoon [)]
flatSiggal ::= (':sggl') [(] flatHoon (ACE) flatHoon [)]

# SIGGAR hoon hoon
SIGGAR ~ [~] [>]
hoon ::= tallSiggar
flatHoon ::= flatSiggar
tallSiggar ::= (SIGGAR gap)hoon (gap) hoon
flatSiggar ::= (SIGGAR) [(] flatHoon (ACE) flatHoon [)]
flatSiggar ::= (':sggr') [(] flatHoon (ACE) flatHoon [)]

# SIGLUS hoon
SIGLUS ~ [~] [+]
hoon ::= tallSiglus
flatHoon ::= flatSiglus
tallSiglus ::= (SIGLUS gap)hoon
flatSiglus ::= (SIGLUS) [(] flatHoon [)]
flatSiglus ::= (':sgls') [(] flatHoon [)]

# SIGPAM hoon hoon
SIGPAM ~ [~] [&]
hoon ::= tallSigpam
flatHoon ::= flatSigpam
tallSigpam ::= (SIGPAM gap)hoon (gap) hoon
flatSigpam ::= (SIGPAM) [(] flatHoon (ACE) flatHoon [)]
flatSigpam ::= (':sgpm') [(] flatHoon (ACE) flatHoon [)]

# SEMSEM hoon hoon
SEMSEM ~ [;] [;]
hoon ::= tallSemsem
flatHoon ::= flatSemsem
tallSemsem ::= (SEMSEM gap)hoon (gap) hoon
flatSemsem ::= (SEMSEM) [(] flatHoon (ACE) flatHoon [)]
flatSemsem ::= (':smsm') [(] flatHoon (ACE) flatHoon [)]

# SIGCAB hoon hoon
SIGCAB ~ [~] [_]
hoon ::= tallSigcab
flatHoon ::= flatSigcab
tallSigcab ::= (SIGCAB gap)hoon (gap) hoon
flatSigcab ::= (SIGCAB) [(] flatHoon (ACE) flatHoon [)]
flatSigcab ::= (':sgcb') [(] flatHoon (ACE) flatHoon [)]

# SIGWUT hoon hoon hoon
SIGWUT ~ [~] [?]
hoon ::= tallSigwut
flatHoon ::= flatSigwut
tallSigwut ::= (SIGWUT gap)hoon (gap) hoon (gap) hoon
flatSigwut ::= (SIGWUT) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatSigwut ::= (':sgwt') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# SIGZAP hoon hoon
SIGZAP ~ [~] [!]
hoon ::= tallSigzap
flatHoon ::= flatSigzap
tallSigzap ::= (SIGZAP gap)hoon (gap) hoon
flatSigzap ::= (SIGZAP) [(] flatHoon (ACE) flatHoon [)]
flatSigzap ::= (':sgzp') [(] flatHoon (ACE) flatHoon [)]

# TISBAR hoon hoon
TISBAR ~ [=] [|]
hoon ::= tallTisbar
flatHoon ::= flatTisbar
tallTisbar ::= (TISBAR gap)hoon (gap) hoon
flatTisbar ::= (TISBAR) [(] flatHoon (ACE) flatHoon [)]
flatTisbar ::= (':tsbr') [(] flatHoon (ACE) flatHoon [)]

# TISCOM hoon hoon
TISCOM ~ [=] [,]
hoon ::= tallTiscom
flatHoon ::= flatTiscom
tallTiscom ::= (TISCOM gap)hoon (gap) hoon
flatTiscom ::= (TISCOM) [(] flatHoon (ACE) flatHoon [)]
flatTiscom ::= (':tscm') [(] flatHoon (ACE) flatHoon [)]

# TISDOT wing hoon hoon
TISDOT ~ [=] [.]
hoon ::= tallTisdot
flatHoon ::= flatTisdot
tallTisdot ::= (TISDOT gap)wing (gap) hoon (gap) hoon
flatTisdot ::= (TISDOT) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatTisdot ::= (':tsdt') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# TISHEP hoon hoon
TISHEP ~ [=] [-]
hoon ::= tallTishep
flatHoon ::= flatTishep
tallTishep ::= (TISHEP gap)hoon (gap) hoon
flatTishep ::= (TISHEP) [(] flatHoon (ACE) flatHoon [)]
flatTishep ::= (':tshp') [(] flatHoon (ACE) flatHoon [)]

# TISFAS hoon hoon hoon
TISFAS ~ [=] [/]
hoon ::= tallTisfas
flatHoon ::= flatTisfas
tallTisfas ::= (TISFAS gap)hoon (gap) hoon (gap) hoon
flatTisfas ::= (TISFAS) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatTisfas ::= (':tsfs') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# TISGAL hoon hoon
TISGAL ~ [=] [<]
hoon ::= tallTisgal
flatHoon ::= flatTisgal
tallTisgal ::= (TISGAL gap)hoon (gap) hoon
flatTisgal ::= (TISGAL) [(] flatHoon (ACE) flatHoon [)]
flatTisgal ::= (':tsgl') [(] flatHoon (ACE) flatHoon [)]

# TISGAR hoon hoon
TISGAR ~ [=] [>]
hoon ::= tallTisgar
flatHoon ::= flatTisgar
tallTisgar ::= (TISGAR gap)hoon (gap) hoon
flatTisgar ::= (TISGAR) [(] flatHoon (ACE) flatHoon [)]
flatTisgar ::= (':tsgr') [(] flatHoon (ACE) flatHoon [)]

# TISKET hoon wing hoon hoon
TISKET ~ [=] [\^]
hoon ::= tallTisket
flatHoon ::= flatTisket
tallTisket ::= (TISKET gap)hoon (gap) wing (gap) hoon (gap) hoon
flatTisket ::= (TISKET) [(] flatHoon (ACE) wing (ACE) flatHoon (ACE) flatHoon [)]
flatTisket ::= (':tskt') [(] flatHoon (ACE) wing (ACE) flatHoon (ACE) flatHoon [)]

# TISLUS hoon hoon
TISLUS ~ [=] [+]
hoon ::= tallTislus
flatHoon ::= flatTislus
tallTislus ::= (TISLUS gap)hoon (gap) hoon
flatTislus ::= (TISLUS) [(] flatHoon (ACE) flatHoon [)]
flatTislus ::= (':tsls') [(] flatHoon (ACE) flatHoon [)]

# TISSEM hoon hoon hoon
TISSEM ~ [=] [;]
hoon ::= tallTissem
flatHoon ::= flatTissem
tallTissem ::= (TISSEM gap)hoon (gap) hoon (gap) hoon
flatTissem ::= (TISSEM) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatTissem ::= (':tssm') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# TISTAR term hoon hoon
TISTAR ~ [=] [*]
hoon ::= tallTistar
flatHoon ::= flatTistar
tallTistar ::= (TISTAR gap)term (gap) hoon (gap) hoon
flatTistar ::= (TISTAR) [(] term (ACE) flatHoon (ACE) flatHoon [)]
flatTistar ::= (':tstr') [(] term (ACE) flatHoon (ACE) flatHoon [)]

# TISWUT wing hoon hoon hoon
TISWUT ~ [=] [?]
hoon ::= tallTiswut
flatHoon ::= flatTiswut
tallTiswut ::= (TISWUT gap)wing (gap) hoon (gap) hoon (gap) hoon
flatTiswut ::= (TISWUT) [(] wing (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatTiswut ::= (':tswt') [(] wing (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# WUTCOL hoon hoon hoon
WUTCOL ~ [?] [:]
hoon ::= tallWutcol
flatHoon ::= flatWutcol
tallWutcol ::= (WUTCOL gap)hoon (gap) hoon (gap) hoon
flatWutcol ::= (WUTCOL) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatWutcol ::= (':wtcl') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# WUTDOT hoon hoon hoon
WUTDOT ~ [?] [.]
hoon ::= tallWutdot
flatHoon ::= flatWutdot
tallWutdot ::= (WUTDOT gap)hoon (gap) hoon (gap) hoon
flatWutdot ::= (WUTDOT) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatWutdot ::= (':wtdt') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# WUTGAL hoon hoon
WUTGAL ~ [?] [<]
hoon ::= tallWutgal
flatHoon ::= flatWutgal
tallWutgal ::= (WUTGAL gap)hoon (gap) hoon
flatWutgal ::= (WUTGAL) [(] flatHoon (ACE) flatHoon [)]
flatWutgal ::= (':wtgl') [(] flatHoon (ACE) flatHoon [)]

# WUTGAR hoon hoon
WUTGAR ~ [?] [>]
hoon ::= tallWutgar
flatHoon ::= flatWutgar
tallWutgar ::= (WUTGAR gap)hoon (gap) hoon
flatWutgar ::= (WUTGAR) [(] flatHoon (ACE) flatHoon [)]
flatWutgar ::= (':wtgr') [(] flatHoon (ACE) flatHoon [)]

# WUTZAP hoon
WUTZAP ~ [?] [!]
hoon ::= tallWutzap
flatHoon ::= flatWutzap
tallWutzap ::= (WUTZAP gap)hoon
flatWutzap ::= (WUTZAP) [(] flatHoon [)]
flatWutzap ::= (':wtzp') [(] flatHoon [)]

# WUTKET wing hoon hoon
WUTKET ~ [?] [\^]
hoon ::= tallWutket
flatHoon ::= flatWutket
tallWutket ::= (WUTKET gap)wing (gap) hoon (gap) hoon
flatWutket ::= (WUTKET) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatWutket ::= (':wtkt') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# WUTPAT wing hoon hoon
WUTPAT ~ [?] [@]
hoon ::= tallWutpat
flatHoon ::= flatWutpat
tallWutpat ::= (WUTPAT gap)wing (gap) hoon (gap) hoon
flatWutpat ::= (WUTPAT) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatWutpat ::= (':wtpt') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# WUTSIG wing hoon hoon
WUTSIG ~ [?] [~]
hoon ::= tallWutsig
flatHoon ::= flatWutsig
tallWutsig ::= (WUTSIG gap)wing (gap) hoon (gap) hoon
flatWutsig ::= (WUTSIG) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatWutsig ::= (':wtsg') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# WUTTIS hoon wing
WUTTIS ~ [?] [=]
hoon ::= tallWuttis
flatHoon ::= flatWuttis
tallWuttis ::= (WUTTIS gap)hoon (gap) wing
flatWuttis ::= (WUTTIS) [(] flatHoon (ACE) wing [)]
flatWuttis ::= (':wtts') [(] flatHoon (ACE) wing [)]

# ZAPGAR hoon
ZAPGAR ~ [!] [>]
hoon ::= tallZapgar
flatHoon ::= flatZapgar
tallZapgar ::= (ZAPGAR gap)hoon
flatZapgar ::= (ZAPGAR) [(] flatHoon [)]
flatZapgar ::= (':zpgr') [(] flatHoon [)]

# ZAPTIS hoon
ZAPTIS ~ [!] [=]
hoon ::= tallZaptis
flatHoon ::= flatZaptis
tallZaptis ::= (ZAPTIS gap)hoon
flatZaptis ::= (ZAPTIS) [(] flatHoon [)]
flatZaptis ::= (':zpts') [(] flatHoon [)]

# ZAPWUT atom hoon
ZAPWUT ~ [!] [?]
hoon ::= tallZapwut
flatHoon ::= flatZapwut
tallZapwut ::= (ZAPWUT gap)atom (gap) hoon
flatZapwut ::= (ZAPWUT) [(] atom (ACE) flatHoon [)]
flatZapwut ::= (':zpwt') [(] atom (ACE) flatHoon [)]

