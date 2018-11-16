# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

# use Test::More tests => 3;

require "yahc.pm";

my $hoonSource = do {
  local $RS = undef;
  <>;
};

local $MarpaX::YAHC::DEBUG = 1;

my $astRef;
my $ok = eval { $astRef = MarpaX::YAHC::parse(\$hoonSource); 1; };

if (not $ok) {
  die "Parse failed";
}

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

say Data::Dumper::Dumper($astRef);

