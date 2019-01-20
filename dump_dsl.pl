# Dump the DSL
# This is helpful in development

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

require "./yahc.pm";

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

my $parser = MarpaX::YAHC::new();
print $parser->dsl();

# vim: expandtab shiftwidth=4:
