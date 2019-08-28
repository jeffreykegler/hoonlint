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

my $recce = $self->rawRecce();

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

say "=== First tree ===";
my $astRef = $recce->value();
say Data::Dumper::Dumper($astRef);
say "=== Second tree ===";
$astRef = $recce->value();
if (not $astRef) {
  say "No tree";
} else {
  say Data::Dumper::Dumper($astRef);
}

# vim: expandtab shiftwidth=4:
