use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );
use Scalar::Util qw(looks_like_number weaken);
use Getopt::Long;

require "hoonlint.pm";

MarpaX::YAHC::Lint->new();

# vim: expandtab shiftwidth=4:
