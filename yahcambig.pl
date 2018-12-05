# Check YAHC for ambiguities

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

# local $MarpaX::YAHC::DEBUG = 1;

my $self = eval {
  my $self = MarpaX::YAHC::new();
  $self->read(\$hoonSource);
  $self;
};

if (not $self) {
  say STDERR $EVAL_ERROR;
  die "Parse failed";
}

my $recce = $self->raw_recce();
my $metric = $recce->ambiguity_metric();
my $astRef = $recce->value();
my $value1 = ($astRef ? 'YES' : 'no');
$astRef = $recce->value();
my $value2 = ($astRef ? 'YES' : 'no');

say "metric: $metric; value1: $value1; value2: $value2";

# local $Data::Dumper::Deepcopy    = 1;
# local $Data::Dumper::Terse    = 1;

# say Data::Dumper::Dumper($astRef);

