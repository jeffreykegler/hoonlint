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
:default ::= action => ::first
lexeme default = latm => 1

top ::= leader hoon trailer
leader ::= ws_elements
trailer ::= ws_elements
ws_elements ::= ws_element*
ws_element ::= ACE
ws_element ::= gap
ws_element ::= COMMENT

hoon ::= tall_bartis
hoon ::= tall_tislus
hoon ::= slash_2cell

tall_bartis ::= BARTIS gap model gap value
value ::= noun # probably should not be hoon
model ::= noun # probably should not be hoon
noun ::= hoon
noun ::= NAME

tall_tislus ::= TISLUS gap hoon gap hoon

gap ::= ACE ACE optAces
gap ::= optAces NL optAces
gap ::= optAces COMMENT comments optAces
optAces ::=
optAces ::= aces
aces ::= ACE*
comments ::= COMMENT*

ACE ~ ' '
COMMENT ~ '::' nonNLs nl
NL ~ nl
nl ~ '\n'
nonNLs ~ nonNL*
nonNL ~ [^\n]
NAME ~ [\w]+

BARTIS ~ '|='
TISLUS ~ '=+'

slash_2cell ::= NAME '/' NAME

EODSL

my $grammar = Marpa::R2::Scanless::G->new( { source => \$dsl } );

TEST: {
    my $input = \$fizzbuzz;
    my $recce = Marpa::R2::Scanless::R->new( { grammar => $grammar } );
    my $value_ref;
    my $eval_ok = eval { $value_ref = doit( $recce, $input ); 1; };
    if ( !$eval_ok ) {
        Test::More::fail('Example 3 failed');
        Test::More::diag($EVAL_ERROR);
        last TEST;
    }
    my $result = [ grep {defined} split q{ }, ${$input} ];
    Test::More::is_deeply( $result, flatten($value_ref), 'Example 3' );
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

    my $value_ref = $recce->value();
    if ( !$value_ref ) {
        die "input read, but there was no parse";
    }

    return $value_ref;
} ## end sub doit
