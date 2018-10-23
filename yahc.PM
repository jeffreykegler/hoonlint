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

leader ::= optWsElements

trailer ::= optWsElements
optWsElements ::= wsElement*
wsElement ::= ACE
wsElement ::= gap

hoonSeq ::= hoon+ separator=>gap proper=>1
hoon ::= tallHoon
hoon ::= flatHoon

# tallHoons ::= tallHoon*
# tallHoon ::= tallBarhep
tallHoon ::= tallBartis
# tallHoon ::= tallCenhep
tallHoon ::= tallColhep
tallHoon ::= tallKethep
tallHoon ::= tallTislus
tallHoon ::= tallWutcol

# flatHoons ::= flatHoon*
flatHoon ::= irrCenhep
flatHoon ::= irrCentis
flatHoon ::= irrDottis
flatHoon ::= irrKettis
flatHoon ::= irrCentisSlash
flatHoon ::= atom

atom ::= NAME
atom ::= NUMBER
atom ::= STRING
atom ::= TERM
atom ::= NIL

toga ::= NAME
toga ::= '[' togaSeq ']'
togaSeq ::= togaElement+ separator=>ACE proper=>1
togaElement ::= toga
togaElement ::= NIL

# tallBarhep ::= (BARHEP) (gap) hoon

# LATER: Should eventually be (BARTIS) (gap) type (gap) hoon
# where <type> is buc??? runes and irregular forms thereof
tallBartis ::= (BARTIS) (gap) hoon (gap) hoon

# tallCenhep ::= (CENHEP) (gap) hoon (gap) hoon

# See https://raw.githubusercontent.com/urbit/old-urbit.org/master/doc/hoon/lan/irregular.markdown
# and cenhep in https://urbit.org/docs/hoon/irregular/
irrCenhep ::= ('(') flatHoonSeq (')')
flatHoonSeq ::= flatHoon+ separator=>ACE proper=>1

# A function call with '$' for the empty string
irrCentis ::= NAME ('(') flatHoonSeq (')')

tallColhep ::= (COLHEP gap) hoon (gap) hoon

irrDottis ::= ('=(') flatHoon (ACE) flatHoon (')')

tallKethep ::= (KETHEP gap) hoon (gap) hoon

irrKettis ::= toga ('=') flatHoon

tallTislus ::= (TISLUS gap) hoon (gap) hoon

tallWutcol ::= (WUTCOL gap) hoon (gap) hoon (gap) hoon

# Perhaps should be called irrBuctisSlash?
irrCentisSlash ::= NAME ('/') NAME

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

# BARHEP ~ '|-'
BARTIS ~ '|='
# CENHEP ~ '%-'
COLHEP ~ ':-'
KETHEP ~ '^-'
TISLUS ~ '=+'
WUTCOL ~ '?:'

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

# From syntax.vim, probably need correction
TERM ~ '%' name

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
    ket => '^',
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
    # tallBarhep ::= (BARHEP gap) hoon (gap) hoon
    push @result, $tallLHS . ' ::= (' . $lexeme . ' gap)' . (join ' (gap) ', @samples);
    my @flatSamples = map { s/^hoon$/flatHoon/; $_; } @samples;
    # flatHoon ::= ([|] [-]) (ACE) flatHoon (ACE) flatHoon
    push @result, 'flatHoon ::= (' . $lexeme . ') [(] ' . (join ' (ACE) ', @flatSamples) . q{ [)]};
    my $regularName = join '', ':',
      substr($glyphname1, 0, 1), substr($glyphname1, 2, 1),
      substr($glyphname2, 0, 1), substr($glyphname2, 2, 1);
    # flatHoon ::= ([:] 'brhp') (ACE) flatHoon (ACE) flatHoon
    push @result, q{flatHoon ::= ('} . $regularName . q{') [(] } . (join ' (ACE) ', @flatSamples) . q{ [)]};
    return join "\n", @result, '';
}

my $fixedDesc = <<'EOS';
# cenlus hoon hoon hoon
# cendot hoon hoon
# cenket hoon hoon hoon hoon
# censig wing hoon hoon
cenhep hoon hoon
# bucpat model model
# buctis @tas model
# buccab hoon
# bucket model model
# buchep model model
# semsem model value
# barcol hoon hoon
# bartis model hoon
# barwut hoon
# bardot hoon
# bartar model hoon
# barsig model hoon
barhep hoon
# wutgal hoon hoon
# wutgar hoon hoon
# wutpat wing hoon hoon
# wutcol hoon hoon hoon
# wuttis model wing
# wutdot hoon hoon hoon
# wutket wing hoon hoon
# wutzap hoon
# wutsig wing hoon hoon
# siggal hoon hoon
# siglus hoon
# siggar hoon hoon
# sigbuc term hoon
# sigwut hoon hoon hoon
# sigfas term hoon
# sigzap hoon hoon
# sigcen term wing hoon hoon
# sigbar hoon hoon
# sigpam hoon hoon
# ketlus hoon hoon
# kettis toga hoon
# ketwut hoon
# ketsig hoon
# ketbar hoon
# kethep model value
# collus hoon hoon hoon
# colcab hoon hoon
# colket hoon hoon hoon hoon
# colhep hoon hoon
# dotlus atom
# dottis hoon hoon
# dotwut hoon
# dottar hoon hoon
# tissem taco hoon hoon
# tisgal hoon hoon
# tislus hoon hoon
# tisgar hoon hoon
# tiswut wing hoon hoon hoon
# tisdot wing hoon hoon
# tistar term hoon hoon
# tisfas taco hoon hoon
# tisket taco wing hoon hoon
# tisbar hoon hoon
# tishep hoon hoon
# tiscom hoon hoon
# zapgar hoon
# zaptis hoon
# zapwut atom hoon
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


