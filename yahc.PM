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

END_OF_DSL_MAIN

# Here is meta-programming to write piece 2

# ace and gap are not really char names,
# and are omitted
my %glyphs = (
    gal => '<',
    pal => '(',
    bar => '|',
    par => ')',
    bas => '\\',
    gar => '>',
    sel => '\xb5', # '['
    buc => '$',
    hax => '#',
    sem => ';',
    cab => '_',
    hep => '-',
    ser => ']',
    cen => '%',
    kel => '{',
    sig => '~',
    col => ':',
    ker => '}',
    soq => '\'',
    com => ',',
    ket => '\\^',
    tar => '*',
    doq => '"',
    lus => '+',
    tec => '`',
    dot => '.',
    pam => '&',
    tis => '=',
    fas => '/',
    pat => '@',
    wut => '?',
    zap => '!',
);

# takes LC alphanumeric rune name and samples
# for N-fixed rune and returns the Marpa rules
# for the tall and the 2 regular flat forms.
sub doFixedRune {
    my ($runeName, @samples) = @_;
    my @result = ('# ' . uc $runeName);
    my $glyphname1 = substr($runeName, 0, 3);
    my $glyphname2 = substr($runeName, 3, 3);
    my $glyph1 = $glyphs{$glyphname1} or die "no glyph for $glyphname1";
    my $glyph2 = $glyphs{$glyphname2};
    my $lexeme = uc $runeName;
    my $tallLHS = 'tall' . ucfirst $runeName;
    my $flatLHS = 'flat' . ucfirst $runeName;
    # BARHEP ~ [|] [-]
    push @result, $lexeme . q{ ~ [} . $glyph1 . q{] [} . $glyph2 . q{]};
    # tallHoon ::= tallBarhep
    push @result, 'tallHoon ::= ' . $tallLHS;
    # flatHoon ::= flatBarhep
    push @result, 'flatHoon ::= ' . $flatLHS;
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

# This is the descripton of the fixed length runes
# (1-fixed, 2-fixed, 3-fixed and 4-fixed) for auto-generation
# of Marpa rules for the various regular formats, both
# tall and flat.
#
# The format is
#
#   rune type1 type2 ...
#
# Hash comments are allowed.  Blank lines are ignored.
#
my $fixedDesc = <<'EOS';
cenlus hoon hoon hoon
cendot hoon hoon
cenket hoon hoon hoon hoon
censig wing hoon hoon
cenhep hoon hoon
bucpat hoon hoon

# buctis @tas model

buccab hoon
bucket hoon hoon
buchep hoon hoon

barcol hoon hoon

# LATER: Should eventually be (BARTIS) (gap) type (gap) hoon
# where <type> is buc??? runes and irregular forms thereof
bardot hoon
barhep hoon
barsig hoon hoon
bartar hoon hoon
bartis hoon hoon
barwut hoon

colcab hoon hoon
colhep hoon hoon
collus hoon hoon hoon
colket hoon hoon hoon hoon

dotlus atom
dottar hoon hoon
dottis hoon hoon
dotwut hoon

ketbar hoon
kethep hoon hoon
ketlus hoon hoon
ketsig hoon
kettis toga hoon
ketwut hoon

#semsem hoon value
semsem hoon hoon

sigbar hoon hoon
sigbuc TERM hoon
sigcen TERM wing hoon hoon
sigfas TERM hoon
siggal hoon hoon
siggar hoon hoon
siglus hoon
sigpam hoon hoon
sigwut hoon hoon hoon
sigzap hoon hoon

tisbar hoon hoon
tiscom hoon hoon
tisdot wing hoon hoon
tishep hoon hoon

# tisfas taco hoon hoon
tisfas hoon hoon hoon

tisgal hoon hoon
tisgar hoon hoon
tisket hoon wing hoon hoon
tislus hoon hoon

# tissem taco hoon hoon
tissem hoon hoon hoon

tistar TERM hoon hoon
tiswut wing hoon hoon hoon

wutcol hoon hoon hoon
wutdot hoon hoon hoon
wutgal hoon hoon
wutgar hoon hoon
wutzap hoon
wutket wing hoon hoon
wutpat wing hoon hoon
wutsig wing hoon hoon
wuttis hoon wing

zapgar hoon
zaptis hoon
zapwut atom hoon
EOS

# === DSL Piece: Automatically generated Marpa rules ===

$fixedDesc =~ s/\s* [#] [^\n]* $//xmsg;
my @dsl_auto_rules = ();
DESC: for my $desc (split "\n", $fixedDesc) {
    chomp $desc; # remove newline
    $desc =~ s/^\s+//; # eliminate leading spaces
    $desc =~ s/\s+$//; # eliminate trailing spaces
    $desc =~ s/\s*[#].*//; # eliminate comments
    next DESC if length $desc <= 0; # ignore empty line
    my ($rune, @samples) = split /\s+/, $desc;
    push @dsl_auto_rules, doFixedRune( $rune, @samples );
}
my $dsl_auto_rules = join "\n", @dsl_auto_rules;

# Print the two pieces
say join '', $dsl_warning, $dsl_main, $dsl_auto_rules;


