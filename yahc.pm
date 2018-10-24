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

1;

__DATA__
# start and length will be needed for production
# :default ::= action => [name,start,length,values]
:default ::= action => [name,values]
lexeme default = latm => 1

# LATER: This is a simplication, which does not
# catch all the subtleties of "ford" files
top ::= (leader) hoonSeq (trailer)

trailer ::= optWs
optWs ::=
optWs ::= ACE
optWs ::= gap

leader ::= optWsElements
optWsElements ::= wsElement*
wsElement ::= ACE
wsElement ::= gap

hoonSeq ::= hoon+ separator=>gap proper=>1
hoon ::= tallHoon
hoon ::= flatHoon

# tallHoons ::= tallHoon*

# flatHoons ::= flatHoon*
flatHoon ::= irrBarcen
flatHoon ::= irrCenhep
flatHoon ::= irrCensig
flatHoon ::= irrCentis
flatHoon ::= irrCentisSlash
flatHoon ::= irrDotdot
flatHoon ::= irrDotlus
flatHoon ::= irrDottis
flatHoon ::= irrKettis
flatHoon ::= irrTisgal
flatHoon ::= atom
flatHoon ::= wing

# TODO: Is a name a simple case of a wing?
atom ::= NAME
atom ::= NUMBER
atom ::= STRING
atom ::= TERM
atom ::= NIL
atom ::= AURA

toga ::= NAME
toga ::= '[' togaSeq ']'
togaSeq ::= togaElement+ separator=>ACE proper=>1
togaElement ::= toga
togaElement ::= NIL

wing ::= limb+ separator=>[.] proper=>1
# TODO: Is a name a simple case of a wing?
# If we make this limb ::= optKets , the a NAME is a trivial case of a wing
limb ::= kets NAME
kets ::= KET+
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

optArmSeq ::= arm* separator=>gap proper=>1
arm ::= ('++' gap) NAME (gap) hoon

# TODO: Zero or more?  One or more?
irrBarcen ::= ('|%' gap) optArmSeq (gap '--')

tallHoon ::= tallCentis
CENTIS ~ [%] [=]
tallCentis ::= CENTIS (gap) hoon (gap) hoonJogging (gap) '=='

# See https://raw.githubusercontent.com/urbit/old-urbit.org/master/doc/hoon/lan/irregular.markdown
# and cenhep in https://urbit.org/docs/hoon/irregular/
irrCenhep ::= ('(') flatHoonSeq (')')

irrCensig ::= ('~(') flatHoonSeq (')')

irrCentis ::= NAME ('(') flatHoonJogging (')')

irrCentisSlash ::= NAME ('/') NAME

# TODO: Is this really an atom?
irrDotdot ::= ('..') NAME

irrDottis ::= ('=(') flatHoon (ACE) flatHoon (')')

irrDotlus ::= ('+(') flatHoon (')')

irrKettis ::= toga ('=') flatHoon

irrTisgal ::= flatHoon (':') flatHoon

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

backslash ~ [\0x5c] # 0x5c is backslash

ACE ~ ' '
COMMENT ~ '::' nonNLs nl
NL ~ nl
nl ~ [\n]
nonNLs ~ nonNL*
nonNL ~ [^\n]
NAME ~ name
name ~ nameFirstChar nameLaterChars
name ~ '$'

nameFirstChar ~ [a-z]
nameLaterChars ~ nameLaterChar*
nameLaterChar ~ [a-z0-9-]

NIL ~ '~'
KET ~ '^'

wsChars ~ wsChar*
wsChar ~ [ \n]

NUMBER ~ decimalNumber
NUMBER ~ hexNumber
NUMBER ~ binaryNumber
decimalNumber ~ decimalPrefix decimalGroups
decimalPrefix ~ decimalDigit
decimalPrefix ~ decimalDigit decimalDigit
decimalPrefix ~ decimalDigit decimalDigit decimalDigit
decimalDigit ~ [0-9]
decimalGroups ~ decimalGroup*
decimalGroup ~ [.] wsChars decimalDigit decimalDigit decimalDigit

hexNumber ~ '0x' hexPrefix hexGroups
hexPrefix ~ hexDigit
hexPrefix ~ hexDigit hexDigit
hexPrefix ~ hexDigit hexDigit hexDigit
hexPrefix ~ hexDigit hexDigit hexDigit hexDigit
hexDigit ~ [0-9a-fA-F]
hexGroups ~ hexGroup*
hexGroup ~ [.] wsChars hexDigit hexDigit hexDigit hexDigit

binaryNumber ~ '0b' binaryPrefix binaryGroups
binaryPrefix ~ binaryDigit
binaryPrefix ~ binaryDigit binaryDigit
binaryPrefix ~ binaryDigit binaryDigit binaryDigit
binaryPrefix ~ binaryDigit binaryDigit binaryDigit binaryDigit
binaryDigit ~ [01]
binaryGroups ~ binaryGroup*
binaryGroup ~ [.] wsChars binaryDigit binaryDigit binaryDigit binaryDigit

# syn match       hoonNumber        "\d\{1,3\}\%(\.\_s\?\d\{3\}\)*"
# syn match       hoonNumber        "0x\x\{1,4\}\%(\.\_s*\x\{4\}\)*"
# syn match       hoonNumber        "0b[01]\{1,4\}\%(\.\_s*[01]\{4\}\)*"

# LATER: @v and @w format
# syn match       hoonNumber        "0v[0-9a-v]\{1,5\}\%(\.\_s*[0-9a-v]\{5\}\)*"
# syn match       hoonNumber        "0w[-~0-9a-zA-Z]\{1,5\}\%(\.\_s*[-~0-9a-zA-Z]\{5\}\)*"

# TODO: Add \xx hex escapes, and more backslash escapes
# LATER: See https://urbit.org/docs/hoon/atom/knit/ for interpolation
STRING ~ ["] doubleStringElements ["]
doubleStringElements ~ doubleStringElement*
# 0x5C is backslash
# From syntax.vim, might need correction
doubleStringElement ~ [^"\x5c] | backslash ["] | backslash backslash

# TODO Single string element -- they also allow escapes

# syn region      hoonString        start=+'+ skip=+\\[\\']+ end=+'+ contains=@spell
# syn region      hoonBlock         start=+'''+ end=+'''+
# syn region      hoonString        start=+"+ skip=+\\[\\"]+ end=+"+ contains=@spell

# TODO: Is this right?
TERM ~ '%' firstTermChar
TERM ~ '%' firstTermChar optMedialTermChars lastTermChar
firstTermChar ~ [a-z]
optMedialTermChars ~ medialTermChar*
medialTermChar ~ [a-z0-9-]
lastTermChar ~ [a-z0-9]

AURA ~ '@'
AURA ~ '@' name

# CENLUS
CENLUS ~ [%] [+]
tallHoon ::= tallCenlus
flatHoon ::= flatCenlus
tallCenlus ::= (CENLUS gap)hoon (gap) hoon (gap) hoon
flatCenlus ::= (CENLUS) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatCenlus ::= (':cnls') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# CENDOT
CENDOT ~ [%] [.]
tallHoon ::= tallCendot
flatHoon ::= flatCendot
tallCendot ::= (CENDOT gap)hoon (gap) hoon
flatCendot ::= (CENDOT) [(] flatHoon (ACE) flatHoon [)]
flatCendot ::= (':cndt') [(] flatHoon (ACE) flatHoon [)]

# CENKET
CENKET ~ [%] [\^]
tallHoon ::= tallCenket
flatHoon ::= flatCenket
tallCenket ::= (CENKET gap)hoon (gap) hoon (gap) hoon (gap) hoon
flatCenket ::= (CENKET) [(] flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatCenket ::= (':cnkt') [(] flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# CENSIG
CENSIG ~ [%] [~]
tallHoon ::= tallCensig
flatHoon ::= flatCensig
tallCensig ::= (CENSIG gap)wing (gap) hoon (gap) hoon
flatCensig ::= (CENSIG) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatCensig ::= (':cnsg') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# CENHEP
CENHEP ~ [%] [-]
tallHoon ::= tallCenhep
flatHoon ::= flatCenhep
tallCenhep ::= (CENHEP gap)hoon (gap) hoon
flatCenhep ::= (CENHEP) [(] flatHoon (ACE) flatHoon [)]
flatCenhep ::= (':cnhp') [(] flatHoon (ACE) flatHoon [)]

# BUCPAT
BUCPAT ~ [$] [@]
tallHoon ::= tallBucpat
flatHoon ::= flatBucpat
tallBucpat ::= (BUCPAT gap)hoon (gap) hoon
flatBucpat ::= (BUCPAT) [(] flatHoon (ACE) flatHoon [)]
flatBucpat ::= (':bcpt') [(] flatHoon (ACE) flatHoon [)]

# BUCCAB
BUCCAB ~ [$] [_]
tallHoon ::= tallBuccab
flatHoon ::= flatBuccab
tallBuccab ::= (BUCCAB gap)hoon
flatBuccab ::= (BUCCAB) [(] flatHoon [)]
flatBuccab ::= (':bccb') [(] flatHoon [)]

# BUCKET
BUCKET ~ [$] [\^]
tallHoon ::= tallBucket
flatHoon ::= flatBucket
tallBucket ::= (BUCKET gap)hoon (gap) hoon
flatBucket ::= (BUCKET) [(] flatHoon (ACE) flatHoon [)]
flatBucket ::= (':bckt') [(] flatHoon (ACE) flatHoon [)]

# BUCHEP
BUCHEP ~ [$] [-]
tallHoon ::= tallBuchep
flatHoon ::= flatBuchep
tallBuchep ::= (BUCHEP gap)hoon (gap) hoon
flatBuchep ::= (BUCHEP) [(] flatHoon (ACE) flatHoon [)]
flatBuchep ::= (':bchp') [(] flatHoon (ACE) flatHoon [)]

# BARCOL
BARCOL ~ [|] [:]
tallHoon ::= tallBarcol
flatHoon ::= flatBarcol
tallBarcol ::= (BARCOL gap)hoon (gap) hoon
flatBarcol ::= (BARCOL) [(] flatHoon (ACE) flatHoon [)]
flatBarcol ::= (':brcl') [(] flatHoon (ACE) flatHoon [)]

# BARDOT
BARDOT ~ [|] [.]
tallHoon ::= tallBardot
flatHoon ::= flatBardot
tallBardot ::= (BARDOT gap)hoon
flatBardot ::= (BARDOT) [(] flatHoon [)]
flatBardot ::= (':brdt') [(] flatHoon [)]

# BARHEP
BARHEP ~ [|] [-]
tallHoon ::= tallBarhep
flatHoon ::= flatBarhep
tallBarhep ::= (BARHEP gap)hoon
flatBarhep ::= (BARHEP) [(] flatHoon [)]
flatBarhep ::= (':brhp') [(] flatHoon [)]

# BARSIG
BARSIG ~ [|] [~]
tallHoon ::= tallBarsig
flatHoon ::= flatBarsig
tallBarsig ::= (BARSIG gap)hoon (gap) hoon
flatBarsig ::= (BARSIG) [(] flatHoon (ACE) flatHoon [)]
flatBarsig ::= (':brsg') [(] flatHoon (ACE) flatHoon [)]

# BARTAR
BARTAR ~ [|] [*]
tallHoon ::= tallBartar
flatHoon ::= flatBartar
tallBartar ::= (BARTAR gap)hoon (gap) hoon
flatBartar ::= (BARTAR) [(] flatHoon (ACE) flatHoon [)]
flatBartar ::= (':brtr') [(] flatHoon (ACE) flatHoon [)]

# BARTIS
BARTIS ~ [|] [=]
tallHoon ::= tallBartis
flatHoon ::= flatBartis
tallBartis ::= (BARTIS gap)hoon (gap) hoon
flatBartis ::= (BARTIS) [(] flatHoon (ACE) flatHoon [)]
flatBartis ::= (':brts') [(] flatHoon (ACE) flatHoon [)]

# BARWUT
BARWUT ~ [|] [?]
tallHoon ::= tallBarwut
flatHoon ::= flatBarwut
tallBarwut ::= (BARWUT gap)hoon
flatBarwut ::= (BARWUT) [(] flatHoon [)]
flatBarwut ::= (':brwt') [(] flatHoon [)]

# COLCAB
COLCAB ~ [:] [_]
tallHoon ::= tallColcab
flatHoon ::= flatColcab
tallColcab ::= (COLCAB gap)hoon (gap) hoon
flatColcab ::= (COLCAB) [(] flatHoon (ACE) flatHoon [)]
flatColcab ::= (':clcb') [(] flatHoon (ACE) flatHoon [)]

# COLHEP
COLHEP ~ [:] [-]
tallHoon ::= tallColhep
flatHoon ::= flatColhep
tallColhep ::= (COLHEP gap)hoon (gap) hoon
flatColhep ::= (COLHEP) [(] flatHoon (ACE) flatHoon [)]
flatColhep ::= (':clhp') [(] flatHoon (ACE) flatHoon [)]

# COLLUS
COLLUS ~ [:] [+]
tallHoon ::= tallCollus
flatHoon ::= flatCollus
tallCollus ::= (COLLUS gap)hoon (gap) hoon (gap) hoon
flatCollus ::= (COLLUS) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatCollus ::= (':clls') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# COLKET
COLKET ~ [:] [\^]
tallHoon ::= tallColket
flatHoon ::= flatColket
tallColket ::= (COLKET gap)hoon (gap) hoon (gap) hoon (gap) hoon
flatColket ::= (COLKET) [(] flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatColket ::= (':clkt') [(] flatHoon (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# DOTLUS
DOTLUS ~ [.] [+]
tallHoon ::= tallDotlus
flatHoon ::= flatDotlus
tallDotlus ::= (DOTLUS gap)atom
flatDotlus ::= (DOTLUS) [(] atom [)]
flatDotlus ::= (':dtls') [(] atom [)]

# DOTTAR
DOTTAR ~ [.] [*]
tallHoon ::= tallDottar
flatHoon ::= flatDottar
tallDottar ::= (DOTTAR gap)hoon (gap) hoon
flatDottar ::= (DOTTAR) [(] flatHoon (ACE) flatHoon [)]
flatDottar ::= (':dttr') [(] flatHoon (ACE) flatHoon [)]

# DOTTIS
DOTTIS ~ [.] [=]
tallHoon ::= tallDottis
flatHoon ::= flatDottis
tallDottis ::= (DOTTIS gap)hoon (gap) hoon
flatDottis ::= (DOTTIS) [(] flatHoon (ACE) flatHoon [)]
flatDottis ::= (':dtts') [(] flatHoon (ACE) flatHoon [)]

# DOTWUT
DOTWUT ~ [.] [?]
tallHoon ::= tallDotwut
flatHoon ::= flatDotwut
tallDotwut ::= (DOTWUT gap)hoon
flatDotwut ::= (DOTWUT) [(] flatHoon [)]
flatDotwut ::= (':dtwt') [(] flatHoon [)]

# KETBAR
KETBAR ~ [\^] [|]
tallHoon ::= tallKetbar
flatHoon ::= flatKetbar
tallKetbar ::= (KETBAR gap)hoon
flatKetbar ::= (KETBAR) [(] flatHoon [)]
flatKetbar ::= (':ktbr') [(] flatHoon [)]

# KETHEP
KETHEP ~ [\^] [-]
tallHoon ::= tallKethep
flatHoon ::= flatKethep
tallKethep ::= (KETHEP gap)hoon (gap) hoon
flatKethep ::= (KETHEP) [(] flatHoon (ACE) flatHoon [)]
flatKethep ::= (':kthp') [(] flatHoon (ACE) flatHoon [)]

# KETLUS
KETLUS ~ [\^] [+]
tallHoon ::= tallKetlus
flatHoon ::= flatKetlus
tallKetlus ::= (KETLUS gap)hoon (gap) hoon
flatKetlus ::= (KETLUS) [(] flatHoon (ACE) flatHoon [)]
flatKetlus ::= (':ktls') [(] flatHoon (ACE) flatHoon [)]

# KETSIG
KETSIG ~ [\^] [~]
tallHoon ::= tallKetsig
flatHoon ::= flatKetsig
tallKetsig ::= (KETSIG gap)hoon
flatKetsig ::= (KETSIG) [(] flatHoon [)]
flatKetsig ::= (':ktsg') [(] flatHoon [)]

# KETTIS
KETTIS ~ [\^] [=]
tallHoon ::= tallKettis
flatHoon ::= flatKettis
tallKettis ::= (KETTIS gap)toga (gap) hoon
flatKettis ::= (KETTIS) [(] toga (ACE) flatHoon [)]
flatKettis ::= (':ktts') [(] toga (ACE) flatHoon [)]

# KETWUT
KETWUT ~ [\^] [?]
tallHoon ::= tallKetwut
flatHoon ::= flatKetwut
tallKetwut ::= (KETWUT gap)hoon
flatKetwut ::= (KETWUT) [(] flatHoon [)]
flatKetwut ::= (':ktwt') [(] flatHoon [)]

# SEMSEM
SEMSEM ~ [;] [;]
tallHoon ::= tallSemsem
flatHoon ::= flatSemsem
tallSemsem ::= (SEMSEM gap)hoon (gap) hoon
flatSemsem ::= (SEMSEM) [(] flatHoon (ACE) flatHoon [)]
flatSemsem ::= (':smsm') [(] flatHoon (ACE) flatHoon [)]

# SIGBAR
SIGBAR ~ [~] [|]
tallHoon ::= tallSigbar
flatHoon ::= flatSigbar
tallSigbar ::= (SIGBAR gap)hoon (gap) hoon
flatSigbar ::= (SIGBAR) [(] flatHoon (ACE) flatHoon [)]
flatSigbar ::= (':sgbr') [(] flatHoon (ACE) flatHoon [)]

# SIGBUC
SIGBUC ~ [~] [$]
tallHoon ::= tallSigbuc
flatHoon ::= flatSigbuc
tallSigbuc ::= (SIGBUC gap)TERM (gap) hoon
flatSigbuc ::= (SIGBUC) [(] TERM (ACE) flatHoon [)]
flatSigbuc ::= (':sgbc') [(] TERM (ACE) flatHoon [)]

# SIGCEN
SIGCEN ~ [~] [%]
tallHoon ::= tallSigcen
flatHoon ::= flatSigcen
tallSigcen ::= (SIGCEN gap)TERM (gap) wing (gap) hoon (gap) hoon
flatSigcen ::= (SIGCEN) [(] TERM (ACE) wing (ACE) flatHoon (ACE) flatHoon [)]
flatSigcen ::= (':sgcn') [(] TERM (ACE) wing (ACE) flatHoon (ACE) flatHoon [)]

# SIGFAS
SIGFAS ~ [~] [/]
tallHoon ::= tallSigfas
flatHoon ::= flatSigfas
tallSigfas ::= (SIGFAS gap)TERM (gap) hoon
flatSigfas ::= (SIGFAS) [(] TERM (ACE) flatHoon [)]
flatSigfas ::= (':sgfs') [(] TERM (ACE) flatHoon [)]

# SIGGAL
SIGGAL ~ [~] [<]
tallHoon ::= tallSiggal
flatHoon ::= flatSiggal
tallSiggal ::= (SIGGAL gap)hoon (gap) hoon
flatSiggal ::= (SIGGAL) [(] flatHoon (ACE) flatHoon [)]
flatSiggal ::= (':sggl') [(] flatHoon (ACE) flatHoon [)]

# SIGGAR
SIGGAR ~ [~] [>]
tallHoon ::= tallSiggar
flatHoon ::= flatSiggar
tallSiggar ::= (SIGGAR gap)hoon (gap) hoon
flatSiggar ::= (SIGGAR) [(] flatHoon (ACE) flatHoon [)]
flatSiggar ::= (':sggr') [(] flatHoon (ACE) flatHoon [)]

# SIGLUS
SIGLUS ~ [~] [+]
tallHoon ::= tallSiglus
flatHoon ::= flatSiglus
tallSiglus ::= (SIGLUS gap)hoon
flatSiglus ::= (SIGLUS) [(] flatHoon [)]
flatSiglus ::= (':sgls') [(] flatHoon [)]

# SIGPAM
SIGPAM ~ [~] [&]
tallHoon ::= tallSigpam
flatHoon ::= flatSigpam
tallSigpam ::= (SIGPAM gap)hoon (gap) hoon
flatSigpam ::= (SIGPAM) [(] flatHoon (ACE) flatHoon [)]
flatSigpam ::= (':sgpm') [(] flatHoon (ACE) flatHoon [)]

# SIGWUT
SIGWUT ~ [~] [?]
tallHoon ::= tallSigwut
flatHoon ::= flatSigwut
tallSigwut ::= (SIGWUT gap)hoon (gap) hoon (gap) hoon
flatSigwut ::= (SIGWUT) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatSigwut ::= (':sgwt') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# SIGZAP
SIGZAP ~ [~] [!]
tallHoon ::= tallSigzap
flatHoon ::= flatSigzap
tallSigzap ::= (SIGZAP gap)hoon (gap) hoon
flatSigzap ::= (SIGZAP) [(] flatHoon (ACE) flatHoon [)]
flatSigzap ::= (':sgzp') [(] flatHoon (ACE) flatHoon [)]

# TISBAR
TISBAR ~ [=] [|]
tallHoon ::= tallTisbar
flatHoon ::= flatTisbar
tallTisbar ::= (TISBAR gap)hoon (gap) hoon
flatTisbar ::= (TISBAR) [(] flatHoon (ACE) flatHoon [)]
flatTisbar ::= (':tsbr') [(] flatHoon (ACE) flatHoon [)]

# TISCOM
TISCOM ~ [=] [,]
tallHoon ::= tallTiscom
flatHoon ::= flatTiscom
tallTiscom ::= (TISCOM gap)hoon (gap) hoon
flatTiscom ::= (TISCOM) [(] flatHoon (ACE) flatHoon [)]
flatTiscom ::= (':tscm') [(] flatHoon (ACE) flatHoon [)]

# TISDOT
TISDOT ~ [=] [.]
tallHoon ::= tallTisdot
flatHoon ::= flatTisdot
tallTisdot ::= (TISDOT gap)wing (gap) hoon (gap) hoon
flatTisdot ::= (TISDOT) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatTisdot ::= (':tsdt') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# TISHEP
TISHEP ~ [=] [-]
tallHoon ::= tallTishep
flatHoon ::= flatTishep
tallTishep ::= (TISHEP gap)hoon (gap) hoon
flatTishep ::= (TISHEP) [(] flatHoon (ACE) flatHoon [)]
flatTishep ::= (':tshp') [(] flatHoon (ACE) flatHoon [)]

# TISFAS
TISFAS ~ [=] [/]
tallHoon ::= tallTisfas
flatHoon ::= flatTisfas
tallTisfas ::= (TISFAS gap)hoon (gap) hoon (gap) hoon
flatTisfas ::= (TISFAS) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatTisfas ::= (':tsfs') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# TISGAL
TISGAL ~ [=] [<]
tallHoon ::= tallTisgal
flatHoon ::= flatTisgal
tallTisgal ::= (TISGAL gap)hoon (gap) hoon
flatTisgal ::= (TISGAL) [(] flatHoon (ACE) flatHoon [)]
flatTisgal ::= (':tsgl') [(] flatHoon (ACE) flatHoon [)]

# TISGAR
TISGAR ~ [=] [>]
tallHoon ::= tallTisgar
flatHoon ::= flatTisgar
tallTisgar ::= (TISGAR gap)hoon (gap) hoon
flatTisgar ::= (TISGAR) [(] flatHoon (ACE) flatHoon [)]
flatTisgar ::= (':tsgr') [(] flatHoon (ACE) flatHoon [)]

# TISKET
TISKET ~ [=] [\^]
tallHoon ::= tallTisket
flatHoon ::= flatTisket
tallTisket ::= (TISKET gap)hoon (gap) wing (gap) hoon (gap) hoon
flatTisket ::= (TISKET) [(] flatHoon (ACE) wing (ACE) flatHoon (ACE) flatHoon [)]
flatTisket ::= (':tskt') [(] flatHoon (ACE) wing (ACE) flatHoon (ACE) flatHoon [)]

# TISLUS
TISLUS ~ [=] [+]
tallHoon ::= tallTislus
flatHoon ::= flatTislus
tallTislus ::= (TISLUS gap)hoon (gap) hoon
flatTislus ::= (TISLUS) [(] flatHoon (ACE) flatHoon [)]
flatTislus ::= (':tsls') [(] flatHoon (ACE) flatHoon [)]

# TISSEM
TISSEM ~ [=] [;]
tallHoon ::= tallTissem
flatHoon ::= flatTissem
tallTissem ::= (TISSEM gap)hoon (gap) hoon (gap) hoon
flatTissem ::= (TISSEM) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatTissem ::= (':tssm') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# TISTAR
TISTAR ~ [=] [*]
tallHoon ::= tallTistar
flatHoon ::= flatTistar
tallTistar ::= (TISTAR gap)TERM (gap) hoon (gap) hoon
flatTistar ::= (TISTAR) [(] TERM (ACE) flatHoon (ACE) flatHoon [)]
flatTistar ::= (':tstr') [(] TERM (ACE) flatHoon (ACE) flatHoon [)]

# TISWUT
TISWUT ~ [=] [?]
tallHoon ::= tallTiswut
flatHoon ::= flatTiswut
tallTiswut ::= (TISWUT gap)wing (gap) hoon (gap) hoon (gap) hoon
flatTiswut ::= (TISWUT) [(] wing (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatTiswut ::= (':tswt') [(] wing (ACE) flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# WUTCOL
WUTCOL ~ [?] [:]
tallHoon ::= tallWutcol
flatHoon ::= flatWutcol
tallWutcol ::= (WUTCOL gap)hoon (gap) hoon (gap) hoon
flatWutcol ::= (WUTCOL) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatWutcol ::= (':wtcl') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# WUTDOT
WUTDOT ~ [?] [.]
tallHoon ::= tallWutdot
flatHoon ::= flatWutdot
tallWutdot ::= (WUTDOT gap)hoon (gap) hoon (gap) hoon
flatWutdot ::= (WUTDOT) [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]
flatWutdot ::= (':wtdt') [(] flatHoon (ACE) flatHoon (ACE) flatHoon [)]

# WUTGAL
WUTGAL ~ [?] [<]
tallHoon ::= tallWutgal
flatHoon ::= flatWutgal
tallWutgal ::= (WUTGAL gap)hoon (gap) hoon
flatWutgal ::= (WUTGAL) [(] flatHoon (ACE) flatHoon [)]
flatWutgal ::= (':wtgl') [(] flatHoon (ACE) flatHoon [)]

# WUTGAR
WUTGAR ~ [?] [>]
tallHoon ::= tallWutgar
flatHoon ::= flatWutgar
tallWutgar ::= (WUTGAR gap)hoon (gap) hoon
flatWutgar ::= (WUTGAR) [(] flatHoon (ACE) flatHoon [)]
flatWutgar ::= (':wtgr') [(] flatHoon (ACE) flatHoon [)]

# WUTZAP
WUTZAP ~ [?] [!]
tallHoon ::= tallWutzap
flatHoon ::= flatWutzap
tallWutzap ::= (WUTZAP gap)hoon
flatWutzap ::= (WUTZAP) [(] flatHoon [)]
flatWutzap ::= (':wtzp') [(] flatHoon [)]

# WUTKET
WUTKET ~ [?] [\^]
tallHoon ::= tallWutket
flatHoon ::= flatWutket
tallWutket ::= (WUTKET gap)wing (gap) hoon (gap) hoon
flatWutket ::= (WUTKET) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatWutket ::= (':wtkt') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# WUTPAT
WUTPAT ~ [?] [@]
tallHoon ::= tallWutpat
flatHoon ::= flatWutpat
tallWutpat ::= (WUTPAT gap)wing (gap) hoon (gap) hoon
flatWutpat ::= (WUTPAT) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatWutpat ::= (':wtpt') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# WUTSIG
WUTSIG ~ [?] [~]
tallHoon ::= tallWutsig
flatHoon ::= flatWutsig
tallWutsig ::= (WUTSIG gap)wing (gap) hoon (gap) hoon
flatWutsig ::= (WUTSIG) [(] wing (ACE) flatHoon (ACE) flatHoon [)]
flatWutsig ::= (':wtsg') [(] wing (ACE) flatHoon (ACE) flatHoon [)]

# WUTTIS
WUTTIS ~ [?] [=]
tallHoon ::= tallWuttis
flatHoon ::= flatWuttis
tallWuttis ::= (WUTTIS gap)hoon (gap) wing
flatWuttis ::= (WUTTIS) [(] flatHoon (ACE) wing [)]
flatWuttis ::= (':wtts') [(] flatHoon (ACE) wing [)]

# ZAPGAR
ZAPGAR ~ [!] [>]
tallHoon ::= tallZapgar
flatHoon ::= flatZapgar
tallZapgar ::= (ZAPGAR gap)hoon
flatZapgar ::= (ZAPGAR) [(] flatHoon [)]
flatZapgar ::= (':zpgr') [(] flatHoon [)]

# ZAPTIS
ZAPTIS ~ [!] [=]
tallHoon ::= tallZaptis
flatHoon ::= flatZaptis
tallZaptis ::= (ZAPTIS gap)hoon
flatZaptis ::= (ZAPTIS) [(] flatHoon [)]
flatZaptis ::= (':zpts') [(] flatHoon [)]

# ZAPWUT
ZAPWUT ~ [!] [?]
tallHoon ::= tallZapwut
flatHoon ::= flatZapwut
tallZapwut ::= (ZAPWUT gap)atom (gap) hoon
flatZapwut ::= (ZAPWUT) [(] atom (ACE) flatHoon [)]
flatZapwut ::= (':zpwt') [(] atom (ACE) flatHoon [)]

