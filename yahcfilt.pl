# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => 3;

use Marpa::R2 4.000;

require "./yahc.pm";

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

my $astRef = MarpaX::YAHC::parse(\$fizzbuzz);

die "Parse failed" if not $astRef;

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

say Data::Dumper::Dumper($astRef);

