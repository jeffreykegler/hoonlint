# Hoon "tidy" utility

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

require "yahc.pm";

my @data = ();
my $grammar;

sub doNode {
    my (undef, @children) = @_;
    my $ruleID         = $Marpa::R2::Context::rule;
    my ( $lhs, @rhs ) =
      map { $grammar->symbol_display_form($_) } $grammar->rule_expand($ruleID);
    my @results = ();
    if ((scalar @rhs) != (scalar @children)) {
        my $childIX = 0;
        CHILD: for (;;) {
            my @childData = @{$children[$childIX]};
            if ($childData[0] eq 'node') {
               shift @childData;
               push @results, [$rhs[0], Marpa::R2::Context::location(), @childData];
               next CHILD;
            }
            push @results, [$rhs[0], Marpa::R2::Context::location()];
            $childIX++;
            last CHILD if $childIX >= scalar @children;
            push @results, ['separator'];
        }
    } else {
        CHILD: for my $childIX (0 .. $#children) {
            my @childData = @{$children[$childIX]};
            if ($childData[0] eq 'node') {
               shift @childData;
               push @results, [$rhs[$childIX], Marpa::R2::Context::location(), @childData];
               next CHILD;
            }
            push @data, [$rhs[$childIX], Marpa::R2::Context::location()];
        }
    }
    return ["node", @results]
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
$grammar = $parser->rawGrammar();
$parser->read(\$hoonSource);
my $recce = $parser->rawRecce();
my $astRef = $recce->value();

die "Parse failed" if not $astRef;

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

say Data::Dumper::Dumper(\@data);

# vim: expandtab shiftwidth=4:
