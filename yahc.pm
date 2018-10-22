# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use Marpa::R2 4.000;

package MarpaX::YAHC;

use English qw( -no_match_vars );


my $dsl = <<'EODSL';
# start and length will be needed for production
# :default ::= action => [name,start,length,values]
:default ::= action => [name,values]
lexeme default = latm => 1

# LATER: This is a simplication, which does not
# catch all the subtleties of "ford" files
top ::= (leader) hoon hoons (trailer)

leader ::= wsElements

trailer ::= wsElements
wsElements ::= wsElement*
wsElement ::= ACE
wsElement ::= gap
wsElement ::= COMMENT

hoons ::= hoon*
hoon ::= tallHoon
hoon ::= flatHoon

# tallHoons ::= tallHoon*
tallHoon ::= tallBarhep
tallHoon ::= tallBartis
tallHoon ::= tallCenhep
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
flatHoon ::= noun

noun ::= NAME
noun ::= NUMBER
noun ::= STRING
noun ::= TERM
noun ::= NIL

toga ::= NAME
toga ::= '[' togaElement togaElements ']'
togaElements ::= togaElement* separator=>ACE proper=>1
togaElement ::= toga
togaElement ::= NIL

tallBarhep ::= (BARHEP) (gap) hoon

# LATER: Should eventually be (BARTIS) (gap) type (gap) hoon
# where <type> is buc??? runes and irregular forms thereof
tallBartis ::= (BARTIS) (gap) hoon (gap) hoon

tallCenhep ::= (CENHEP) (gap) hoon (gap) hoon

# See https://raw.githubusercontent.com/urbit/old-urbit.org/master/doc/hoon/lan/irregular.markdown
# and cenhep in https://urbit.org/docs/hoon/irregular/
irrCenhep ::= ('(') hoonAceSeq (')')
hoonAceSeq ::= flatHoon+ separator=>ACE proper=>1

# A function call with '$' for the empty string
irrCentis ::= NAME ('(') hoonAceSeq (')')

tallColhep ::= (COLHEP gap) hoon (gap) hoon

irrDottis ::= ('=(') hoon (ACE) hoon (')')

tallKethep ::= (KETHEP gap) hoon (gap) hoon

irrKettis ::= toga ('=') hoon

tallTislus ::= (TISLUS gap) hoon (gap) hoon

tallWutcol ::= (WUTCOL gap) hoon (gap) hoon (gap) hoon

# Perhaps should be called irrBuctisSlash?
irrCentisSlash ::= NAME ('/') NAME

# LATER: For now blank lines are banned
gap ::= ACE ACE aces
gap ::= aces NL aces
gap ::= aces COMMENT comments aces
aces ::= ACE*

# LATER: For now blank lines are banned
comments ::= COMMENT*

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
TERM ~ '%' name

EODSL

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
