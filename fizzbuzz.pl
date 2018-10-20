# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => 3;

use Marpa::R2 2.090;

my $fizzbuzz = <<'EOI';
|=  end/atom
=+  count=1
|-
^-  (list tape)
?:  =(end count)
   ~
:-
  ?:  =(0 (mod count 15))
    "FizzBuzz"
  ?:  =(0 (mod count 5))
    "Fizz"
  ?:  =(0 (mod count 3))
    "Buzz"
  (scow %ud count)
$(count (add 1 count))
EOI

my $dsl = <<'EODSL';
:default ::= action => [name,start,length,values]
lexeme default = latm => 1

top ::= leader hoon trailer
leader ::= ws_elements
trailer ::= ws_elements
ws_elements ::= ws_element*
ws_element ::= ACE
ws_element ::= gap
ws_element ::= COMMENT

hoon ::= tall_barhep
hoon ::= tall_bartis
hoon ::= tall_cenhep
hoon ::= irr_censig
hoon ::= irr_centis
hoon ::= tall_colhep
hoon ::= irr_dottis
hoon ::= tall_kethep
hoon ::= irr_kettis
hoon ::= tall_tislus
hoon ::= tall_wutcol
hoon ::= slash_2cell
hoon ::= noun

noun ::= NAME
noun ::= NUMBER
noun ::= STRING
noun ::= TERM
noun ::= NIL
toga ::= NAME # TODO: is this right?

tall_barhep ::= BARHEP gap hoon

tall_bartis ::= BARTIS gap hoon gap hoon

tall_cenhep ::= CENHEP gap hoon gap hoon

# It's hard to find an exact description of this
# in the docs, but the various arities seem to
# all be irregular variations of censig.
irr_censig ::= '(' hoonAceSeq ')'
hoonAceSeq ::= hoon+ separator=>ACE proper=>1

# Apparently a form of centis with the special
# limb $ for the recursion point
irr_centis ::= '$(' hoonAceSeq ')'

tall_colhep ::= COLHEP gap hoon gap hoon

irr_dottis ::= '=(' hoon ACE hoon ')'

tall_kethep ::= KETHEP gap hoon gap hoon

irr_kettis ::= toga '=' hoon

tall_tislus ::= TISLUS gap hoon gap hoon

tall_wutcol ::= WUTCOL gap hoon gap hoon gap hoon

slash_2cell ::= NAME '/' NAME

gap ::= ACE ACE optAces
gap ::= optAces NL optAces
gap ::= optAces COMMENT comments optAces
optAces ::=
optAces ::= aces
aces ::= ACE*
comments ::= COMMENT*

backslash ~ [\0x5c] # 0x5c is backslash

ACE ~ ' '
COMMENT ~ '::' nonNLs nl
NL ~ nl
nl ~ [\n]
nonNLs ~ nonNL*
nonNL ~ [^\n]
NAME ~ nameFirstChar nameLaterChars

nameFirstChar ~ [_a-zA-Z]
nameLaterChars ~ nameLaterChar*
nameLaterChar ~ [\w]

NIL ~ '~'

BARHEP ~ '|-'
BARTIS ~ '|='
CENHEP ~ '%-'
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
# syn match       hoonNumber        "0v[0-9a-v]\{1,5\}\%(\.\_s*[0-9a-v]\{5\}\)*"
# syn match       hoonNumber        "0w[-~0-9a-zA-Z]\{1,5\}\%(\.\_s*[-~0-9a-zA-Z]\{5\}\)*"

STRING ~ ["] doubleStringElements ["]
doubleStringElements ~ doubleStringElement*
# 0x5C is backslash
# From syntax.vim, might need correction
doubleStringElement ~ [^"\x5c] | backslash ["] | backslash backslash

# syn region      hoonString        start=+'+ skip=+\\[\\']+ end=+'+ contains=@spell
# syn region      hoonBlock         start=+'''+ end=+'''+
# syn region      hoonString        start=+"+ skip=+\\[\\"]+ end=+"+ contains=@spell

# From syntax.vim, probably need correction
TERM ~ '%' termChar termChars
termChars ~ termChar*
termChar ~ [-\w]

EODSL

my $grammar = Marpa::R2::Scanless::G->new( { source => \$dsl } );

TEST: {
    my $input = \$fizzbuzz;
    my $recce = Marpa::R2::Scanless::R->new( { grammar => $grammar,
       trace_lexers => 1,
       trace_terminals => 1,
    } );
    my $valueRef;
    my $eval_ok = eval { $valueRef = doit( $recce, $input ); 1; };
    if ( !$eval_ok ) {
        Test::More::fail('Example 3 failed');
        Test::More::diag($EVAL_ERROR);
        last TEST;
    }
    # my $result = [ grep {defined} split q{ }, ${$input} ];
    local $Data::Dumper::Deepcopy    = 1;
    local $Data::Dumper::Terse    = 1;

    say Data::Dumper::Dumper($valueRef);
    # Test::More::is_deeply( $result, $valueRef, 'Example 3' );
} ## end TEST:

sub doit {
    my ( $recce, $input ) = @_;
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
        my @ambiguities = grep {defined} @{$ambiguities}[ 0 .. 1 ];
        die
            "Parse of BNF/Scanless source is ambiguous\n",
            Marpa::R2::Internal::ASF::ambiguities_show( $asf, \@ambiguities );
    } ## end if ( $recce->ambiguity_metric() > 1 )

    my $valueRef = $recce->value();
    if ( !$valueRef ) {
        die "input read, but there was no parse";
    }

    return $valueRef;
} ## end sub doit
