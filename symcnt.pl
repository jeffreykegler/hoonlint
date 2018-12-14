# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

require "./yahc.pm";

my @syms = qw(
aMane
aura
bip4j
bip4j_Piece
bisk4l
bonk5d
bont5d
bony5d
bonz5d
bonzElement
boog5d
bucBar
bucBuc
bucPam
cenBar
cenBuc
cenDirectories
cenPam
cenPath
cenTerm
circumBarParen
circumBraces
circumBracket
circumBracketSig
circumColParen
circumGalgar
circumGargal
circumParen1
circumParen2
circumScatParen
circumSigParen
circumWutParen
classicWhitespace
commaAce
commaWS
crub4l
date
date_part1
date_part2
date_part3
dem4k
dollarTerm
doubleQuoteCord
doubleQuoteElement
doubleQuoteString
faspamSym
faspamSyms
fed4j
fordFasbar
fordFasbuc
fordFascab
fordFascen
fordFascol
fordFascom
fordFascomBody
fordFascomElement
fordFascomElements
fordFasdot
fordFashax
fordFasket
fordFaspam
fordFassem
fordFassig
fordFastis
fordFaswut
fordFile
fordHath
fordHave
fordHith
fordHive
fordHoodCase
fordHoodShip
fordHoof
fordHoofSeq
fordHoop
fordHoopSeq
fordHoot
gash5d
gasp5d
haf4j
hasp5d
hef4j
hepSoilSeq
hepSolSeqItem
hif4j
hof4j
hopefullyQuote
horn
hornRune
hornSeq
huf4j
hyf4j
hyf4jSeq
inaccessible_ok
infixCol
infixDot
infixFas
infixKet
infixTis
irrDottis
lib4j_Piece
limb
limp5d
lip4j
lobo5d
long5dWide
lusHepCell
lusLusCell
lusSoilSeq
lusSolSeqItem
lusTisCell
lute5d
moldAura
moldBucbar
moldBucbuc
moldBucNuck4l
moldBucpam
moldBucSingleString
moldCenbar
moldCenbuc
moldCenNuck4l
moldCenpam
moldCenSingleString
moldCircumBrace
moldCircumBracket
moldCircumParen
moldInfixCol
moldInfixCol2
moldInfixFas
moldInfixTis
moldKet
moldNullSig
moldPrefixCab
moldPrefixCom
moldPrefixTis
moldPrefixWut
moldSig
moldTar
moldWut
mota5d
norm5d
norm5dMold
nuck4l
oneToThreeGars
optBonzElements
optCen4hSeq
optClassicWhitespace
optFasSeq
optFordFashep
optFordFaslus
optFordFaswut
optFordHithElements
optGay4i
optHep
optHornSeq
optKets
optTagHeadFinal
optTagHeadInitial
optTagHeadKernel
optTallAttrs
optTisSeq
optWideAttrs
optWideBonzElements
optWideQuoteEmbedFreeStretch
optWideQuoteInnards
pamPlusPrefix
perd4l
poor5d
porc5d
prefixBar
prefixCab
prefixColFas
prefixPam
prefixSoloTec
prefixTar
prefixTecAura
prefixTecChoices
prefixTecHoon
prefixTecMold
prefixTecTar
prefixZap
qit4k
qut4k
qut4k_Piece
rick5d
rick5dJog
rood5d
rope5d
royl4l
ruck5d
ruck5dJog
rump5d
rupl5d
sailApex5d
sailWideElement
sailWideElements
scad5d
scat5d
scriptOrStyle
scriptStyleTail
scriptStyleTailElement
scriptStyleTailElements
sigCircumBracket
sigCircumBracketSig
singleQuoteCord
soil5d
soloBar
soloKet
soloPam
soloSig
soloTar
soloWut
:start
sump5d
tagHead
tagHeadFinal
tagHeadInitial
tagHeadKernel
tagHeadKernelElement
tagHeadKernelElements
tall5d
tall5dSeq
tallAttribute
tallAttributes
tallBarcab
tallBarcen
tallBarcol
tallBardot
tallBarhep
tallBarket
tallBarsig
tallBartar
tallBartis
tallBarwut
tallBuccab
tallBuccabMold
tallBuccen
tallBuccenMold
tallBuccol
tallBuccolMold
tallBuchep
tallBuchepMold
tallBucket
tallBucketMold
tallBucpat
tallBucpatMold
tallBucsem
tallBucsemMold
tallBuctis
tallBuctisMold
tallBucwut
tallBucwutMold
tallCencab
tallCencolMold
tallCendot
tallCenhep
tallCenhepMold
tallCenket
tallCenketMold
tallCenlus
tallCenlusMold
tallCensig
tallCentar
tallCentis
tallColcab
tallColhep
tallColket
tallCollus
tallColsig
tallColtar
tallDotket
tallDotlus
tallDottar
tallDottis
tallDotwut
tallElem
tallKetbar
tallKetcen
tallKetdot
tallKethep
tallKetlus
tallKetpam
tallKetsig
tallKettis
tallKetwut
tallKidOfElem 
tallKidsOfElem
tallKidsOfTop 
tallSemcol
tallSemfas
tallSemsem
tallSemsig
tallSigbar
tallSigbuc
tallSigcab
tallSigcen
tallSigfas
tallSiggal
tallSiggar
tallSiglus
tallSigpam
tallSigtis
tallSigwut
tallSigzap
tallTailCommon
tallTailOfElem
tallTailOfTop
tallTisbar
tallTiscol
tallTiscom
tallTisdot
tallTisfas
tallTisgal
tallTisgar
tallTishep
tallTisket
tallTislus
tallTissem
tallTissig
tallTistar
tallTiswut
tallTopKidSeq 
tallTopSail
tallWutbar
tallWutcol
tallWutdot
tallWutgal
tallWutgar
tallWuthep
tallWutket
tallWutlus
tallWutpam
tallWutpat
tallWutsig
tallWuttis
tallWutzap
tallZapcol
tallZapcom
tallZapdot
tallZapgar
tallZaptis
tallZapWut
tash4l
teak5d
teakChoice
till5d
till5dSeq
timePeriod
timePeriodByUnit
timePeriodDays
timePeriodFraction
timePeriodHours
timePeriodKernel
timePeriodMinutes
timePeriodSeconds
toga
togaElement
togaElements
togaSeq
tunaMode
twid4l
urs4j
ursChoice
urx4j
urxChoice
wasp5d
waspElement 
waspElements
wede5d
whap5d
wide5d
wide5dChoices
wide5dJog
wide5dJoggingSeparator
wide5dJogs
wide5dSeq
wideAttrBody
wideAttribute
wideAttrs
wideBarcol
wideBardot
wideBarhep
wideBarsig
wideBartar
wideBartis
wideBarwut
wideBont5d
wideBonz5d
wideBonzElement
wideBracketedElem
wideBuccab
wideBuccabMold
wideBuccen
wideBuccenMold
wideBuccol
wideBuccolMold
wideBuchep
wideBuchepMold
wideBucket
wideBucketMold
wideBucpat
wideBucpatMold
wideBucsem
wideBucsemMold
wideBuctis
wideBuctisMold
wideBucwut
wideBucwutMold
wideCencab
wideCencolMold
wideCendot
wideCenhep
wideCenhepMold
wideCenket
wideCenketMold
wideCenlus
wideCenlusMold
wideCensig
wideCentar
wideCentis
wideCircumFas
wideColcab
wideColhep
wideColket
wideCollus
wideColsig
wideColtar
wideDotket
wideDotlus
wideDottar
wideDottis
wideDotwut
wideElems
wideFaszap
wideFordFasbar
wideFordFasbuc
wideFordFascab
wideFordFascen
wideFordFascol
wideFordFashax
wideFordFasket
wideFordFaspam
wideFordFassem
wideFordFassig
wideFordFastis
wideHorn
wideHornRune
wideHornSeq
wideInnerTop 
wideInnerTops
wideKetbar
wideKetcen
wideKetdot
wideKethep
wideKetlus
wideKetpam
wideKetsig
wideKettis
wideKetwut
wideNorm5d
wideNorm5dMold
wideParenElems
wideQuote
wideQuoteEmbedFreeElement
wideQuoteEmbedFreeStretch
wideQuoteEmbedTerminatedStretch
wideQuoteEmbedTerminatedStretches
wideRick5d
wideRick5dJog
wideRuck5d
wideRuck5dJog
wideSailApex5d
wideSemcol
wideSemfas
wideSemsem
wideSemsig
wideSigbar
wideSigbuc
wideSigcab
wideSigcen
wideSigfas
wideSiggal
wideSiggar
wideSiglus
wideSigpam
wideSigtis
wideSigwut
wideSigzap
wideTail
wideTeak5d
wideTeakChoice
wideTisbar
wideTiscol
wideTiscom
wideTisdot
wideTisfas
wideTisgal
wideTisgar
wideTishep
wideTisket
wideTislus
wideTissem
wideTissig
wideTistar
wideTiswut
wideTopSail
wideWutbar
wideWutcol
wideWutdot
wideWutgal
wideWutgar
wideWuthep
wideWutket
wideWutlus
wideWutpam
wideWutpat
wideWutsig
wideWuttis
wideWutzap
wideZapcol
wideZapcom
wideZapdot
wideZapgar
wideZaptis
wideZapWut
wise5d
wisp5d
wrappedElems
wyde5d
wyde5dSeq
zust4l
);
# End of qw< syms >

my %count = ();
for my $sym (@syms) {
    $count{$sym} = 0;
}

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

my $recce;
my $globalFileName = '!!! ERROR !!!';

sub doNode {
    my ( undef, @stuff ) = @_;
    my @lexemes = map { @{$_}; } grep { $_; } @stuff;
    no warnings 'once';
    my $rule_id = $Marpa::R2::Context::rule;
    my $slg     = $Marpa::R2::Context::slg;
    use warnings;
    my ($lhs) =
      map { $slg->symbol_display_form($_) } $slg->rule_expand($rule_id);
    # say STDERR "LHS: ", $lhs;
    # say STDERR "Lexemes: ", join " ", @lexemes;
    for my $sym ( $lhs, @lexemes ) {
        $count{$sym} = 0 if not exists $count{$sym};
        $count{$sym}++;
    }
    return;
}

my $semantics = <<'EOS';
:default ::= action => main::doNode
lexeme default = action => [name] latm => 1
EOS
my $parser = MarpaX::YAHC::new({semantics => $semantics});

my $pFileNames = do { local $RS = undef; my $fileNames = <STDIN>; \$fileNames };

FILE: for my $fileName (split "\n", ${$pFileNames}) {
    my $origLine = $fileName;
    $fileName =~ s/\s*[#].*$//xmsg; # Eliminate comments
    $fileName =~ s/^\s*//xmsg; # Eliminate leading space
    $fileName =~ s/\s*$//xmsg; # Eliminate trailing space
    next FILE unless $fileName;
    
    $globalFileName = $fileName;

    open my $fh, '<', $fileName or die "Cannot open $fileName";
    my $testName = $fileName;
    $testName =~ s/^hoons\///;
    $testName = "Test of " . $testName;
    my $hoonSource = do { local $RS = undef; <$fh>; };
    $parser->read(\$hoonSource);
    $recce = $parser->rawRecce();
    my $astRef = $recce->value();
}

for my $sym (sort { $count{$b} <=> $count{$a} } keys %count) {
    say "$sym ", $count{$sym};
}

# vim: expandtab shiftwidth=4:
