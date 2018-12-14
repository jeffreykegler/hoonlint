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
if ( my $ambiguous_status = $recce->ambiguous() ) {
    chomp $ambiguous_status;
    die "Parse is ambiguous\n", $ambiguous_status;
}

# vim: expandtab shiftwidth=4:
