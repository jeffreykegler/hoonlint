# Hoon "tidy" utility

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

require "yahc.pm";

my @data = ();

sub doNode {
    my (undef, @children) = @_;
    CHILD: for my $child (@children) {
        if (not ref $child) {
            push @data, $child;
            next CHILD;
        }
        push @data, join q{ }, @{$child};
    }
    return "node";
}

my $hoonSource = do {
  local $RS = undef;
  <>;
};

my $semantics = <<'EOS';
:default ::= action=>main::doNode
lexeme default = latm => 1 action=>[start,length,name]
EOS

$DB::single = 1;
my $parser = MarpaX::YAHC::new( { semantics => $semantics, all_symbols => 1 } );
$parser->read(\$hoonSource);
my $recce = $parser->rawRecce();
my $astRef = $recce->value();

die "Parse failed" if not $astRef;

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

say Data::Dumper::Dumper(\@data);

# vim: expandtab shiftwidth=4:
