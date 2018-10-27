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
        flatHoon        => 1,
        flatHoonJogging => 1,
        flatHoonJogs    => 1,
        flatHoonSeq     => 1,
        hoon            => 1,
        hoonJog     => 1,
        hoonJogging => 1,
        hoonJogs    => 1,
        hoonSeq         => 1,
        tallHoon        => 1,
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

# tallHoons ::= tallHoon+

# flatHoons ::= flatHoon+
flatHoon ::= atom
flatHoon ::= type
flatHoon ::= wing

atom ::= NUMBER
atom ::= STRING
atom ::= term
atom ::= NIL
atom ::= AURA
atom ::= loobean

loobean ::= '%.y'
loobean ::= '%.n'
loobean ::= '&'
loobean ::= '|'

term ::= TERM
term ::= dollarTerm
dollarTerm ::= ('$') NAME
dollarTerm ::= dollarNil
dollarNil ::= ('$~')

type ::= '*' # noun
type ::= '^' # cell
type ::= '?' # loobean
# LATER: commented out because these create ambiguities
# type ::= '~' # null
# type ::= '@' # cell

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

optNamedHoonSeq ::= namedHoon* separator=>gap proper=>1
namedHoon ::= ('++' gap) NAME (gap) hoon

# FIXED: bucpat hoon hoon
# FIXED: buctis term hoon
# FIXED: buccab hoon
# FIXED: bucket hoon hoon
# FIXED: buchep hoon hoon
# FIXED: barcol hoon hoon
# FIXED: bardot hoon
# FIXED: barhep hoon
# FIXED: barsig hoon hoon
# FIXED: bartar hoon hoon

# # LATER: Should eventually be (BARTIS) (gap) type (gap) hoon
# # where <type> is buc??? runes and irregular forms thereof
# FIXED: bartis hoon hoon

# FIXED: barwut hoon

BARCAB ~ [|] [_]
tallHoon ::= tallBarcab
tallBarcab ::= (BARCAB gap) hoon (gap) optNamedHoonSeq (gap '--')

BARCEN ~ [|] [%]
tallHoon ::= tallBarcen
tallBarcen ::= (BARCEN gap) optNamedHoonSeq (gap '--')

BUCCOL ~ [$] [:]
tallHoon ::= tallBuccol
tallBuccol ::= (BUCCOL gap) hoonSeq (gap '==')
flatHoon ::= flatBuccol
flatBuccol ::= (BUCCOL '(') flatHoonSeq (')')
flatBuccol ::= (':bccl(') flatHoonSeq (')')
flatBuccol ::= ('{') flatHoonSeq ('}')
flatBuccol ::= ('[') flatHoonSeq (']')
flatBuccol ::= (',[') flatHoonSeq (']')

flatHoon  ::= irrBuccab
irrBuccab ::= ('_') flatHoon

BUCCEN ~ [$] [%]
tallHoon ::= tallBuccen
tallBuccen ::= (BUCCEN gap) battery (gap '==')

battery ::= batteryPair+ separator=>gap proper=>1
batteryPair ::= ('[' ACE) term (ACE) foot (']')
batteryPair ::= curlyPair
curlyPair ::= ('{') term (ACE) foot ('}')
foot ::= flatHoon

flatHoon ::= irrBuctisSlash
irrBuctisSlash ::= NAME ('/') hoon

# FIXED: cendot hoon hoon
# FIXED: cenhep hoon hoon
flatHoon ::= irrCenhep
# See https://raw.githubusercontent.com/urbit/old-urbit.org/master/doc/hoon/lan/irregular.markdown
# and cenhep in https://urbit.org/docs/hoon/irregular/
irrCenhep ::= ('(') flatHoonSeq (')')

# FIXED: cenket hoon hoon hoon hoon
# FIXED: cenlus hoon hoon hoon

# FIXED: censig wing hoon hoon
flatHoon ::= irrCensig
irrCensig ::= ('~(') flatHoonSeq (')')

# FIXED: colcab hoon hoon
# FIXED: colhep hoon hoon
# FIXED: collus hoon hoon hoon
# FIXED: colket hoon hoon hoon hoon

flatHoon ::= irrCentis
tallHoon ::= tallCentis
CENTIS ~ [%] [=]
tallCentis ::= CENTIS (gap) hoon (gap) hoonJogging (gap '==')
irrCentis ::= NAME ('(') flatHoonJogging (')')

# FIXED: dottis hoon hoon
flatHoon ::= irrDottis
irrDottis ::= ('=(') flatHoon (ACE) flatHoon (')')

# FIXED: dotlus atom
flatHoon ::= irrDotlus
irrDotlus ::= ('+(') flatHoon (')')

# FIXED: dottar hoon hoon
# FIXED: dotwut hoon

flatHoon ::= irrKettis
irrKettis ::= toga ('=') flatHoon

flatHoon ::= irrTisgal
irrTisgal ::= flatHoon (':') flatHoon

WUTBAR ~ [?] [|]
tallHoon ::= tallWutbar
tallWutbar ::= (WUTBAR gap) hoonSeq (gap '==')
flatHoon ::= flatWutbar
flatWutbar ::= (WUTBAR '(') flatHoonSeq (')')
flatWutbar ::= (':wtbr(') flatHoonSeq (')')
flatWutbar ::= ('|(') flatHoonSeq (')')

WUTPAM ~ [?] [&]
tallHoon ::= tallWutpam
tallWutpam ::= (WUTPAM gap) hoonSeq (gap '==')
flatHoon ::= flatWutpam
flatWutpam ::= (WUTPAM '(') flatHoonSeq (')')
flatWutpam ::= (':wtpm(') flatHoonSeq (')')
flatWutpam ::= ('&(') flatHoonSeq (')')

flatWutzap ::= irrWutzap
irrWutzap ::= ('!') flatHoon

tallHoon ::= tallWuthep
WUTHEP ~ [?] [-]
tallWuthep ::= WUTHEP (gap) wing (gap) hoonJogging (gap '==')

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
NUMBER ~ vNumber
NUMBER ~ wNumber

# syn match       hoonNumber        "\d\{1,3\}\%(\.\_s\?\d\{3\}\)*"
decimalNumber ~ decimalPrefix decimalGroups
decimalPrefix ~ decimalDigit
decimalPrefix ~ decimalDigit decimalDigit
decimalPrefix ~ decimalDigit decimalDigit decimalDigit
decimalDigit ~ [0-9]
decimalGroups ~ decimalGroup*
decimalGroup ~ [.] wsChars decimalDigit decimalDigit decimalDigit

# syn match       hoonNumber        "0x\x\{1,4\}\%(\.\_s*\x\{4\}\)*"
hexNumber ~ '0x' hexPrefix hexGroups
hexPrefix ~ hexDigit
hexPrefix ~ hexDigit hexDigit
hexPrefix ~ hexDigit hexDigit hexDigit
hexPrefix ~ hexDigit hexDigit hexDigit hexDigit
hexDigit ~ [0-9a-fA-F]
hexGroups ~ hexGroup*
hexGroup ~ [.] wsChars hexDigit hexDigit hexDigit hexDigit

# syn match       hoonNumber        "0b[01]\{1,4\}\%(\.\_s*[01]\{4\}\)*"
binaryNumber ~ '0b' binaryPrefix binaryGroups
binaryPrefix ~ binaryDigit
binaryPrefix ~ binaryDigit binaryDigit
binaryPrefix ~ binaryDigit binaryDigit binaryDigit
binaryPrefix ~ binaryDigit binaryDigit binaryDigit binaryDigit
binaryDigit ~ [01]
binaryGroups ~ binaryGroup*
binaryGroup ~ [.] wsChars binaryDigit binaryDigit binaryDigit binaryDigit

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

TERM ~ '%' firstTermChar
TERM ~ '%' firstTermChar optMedialTermChars lastTermChar
firstTermChar ~ [a-z]
optMedialTermChars ~ medialTermChar*
medialTermChar ~ [a-z0-9-]
lastTermChar ~ [a-z0-9]

AURA ~ '@'
AURA ~ '@' optAlphas
optAlphas ~ [a-zA-Z]*

# FIXED: ketbar hoon
# FIXED: kethep hoon hoon
# FIXED: ketlus hoon hoon
# FIXED: ketsig hoon
# FIXED: kettis toga hoon
# FIXED: ketwut hoon

# #semsem hoon value
# FIXED: semsem hoon hoon
#
# FIXED: sigbar hoon hoon
# FIXED: sigbuc term hoon
# FIXED: sigcen term wing hoon hoon
# FIXED: sigfas term hoon
# FIXED: siggal hoon hoon
# FIXED: siggar hoon hoon
# FIXED: siglus hoon
# FIXED: sigpam hoon hoon
# FIXED: sigwut hoon hoon hoon
# FIXED: sigzap hoon hoon
#
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

# FIXED: wutcol hoon hoon hoon
# FIXED: wutdot hoon hoon hoon
# FIXED: wutgal hoon hoon
# FIXED: wutgar hoon hoon
# FIXED: wutzap hoon
# FIXED: wutket wing hoon hoon
# FIXED: wutpat wing hoon hoon
# FIXED: wutsig wing hoon hoon
# FIXED: wuttis hoon wing

# FIXED: zapgar hoon
# FIXED: zaptis hoon
# FIXED: zapwut atom hoon

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


