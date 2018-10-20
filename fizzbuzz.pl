# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => 3;

use Marpa::R2 2.090;

my $input = <<'EOI';
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
