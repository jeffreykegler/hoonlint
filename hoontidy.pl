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
    my ( undef, @children ) = @_;
    my $childCount = scalar @children;
    my $ruleID     = $Marpa::R2::Context::rule;
    my ( $lhs, @rhs ) =
      map { $grammar->symbol_display_form($_) } $grammar->rule_expand($ruleID);
    my @results = ();
  RESULT: {
        if ( $childCount <= 0 ) {
            # This is a nulled rule
            push @results, [ 'null' ];
            say STDERR join "!", __FILE__, __LINE__, @{$results[$#results]};
            last RESULT;
        }
        if ( ( scalar @rhs ) != $childCount ) {
            # This is a non-trivial (that is, longer than one item) sequence rule.
            my $childIX = 0;
          CHILD: for ( ; ; ) {
                say STDERR "childIX=$childIX; last children ix = $#children";
                my @childData = @{ $children[$childIX] };
                $childIX++;
                if ( $childData[0] eq 'node' ) {
                    shift @childData;
                    push @results,
                      [ $rhs[0], Marpa::R2::Context::location(), @childData ];
                    say STDERR join "!", __FILE__, __LINE__, @{$results[$#results]};
                }
                push @results, [ $rhs[0], @childData ];
                say STDERR join "!", __FILE__, __LINE__, @{$results[$#results]};
                last RESULT if $childIX >= $#children;
                push @results, ['separator'];
                say STDERR join "!", __FILE__, __LINE__, @{$results[$#results]};
            }
            last RESULT;
        }
      # All other rules
      CHILD: for my $childIX ( 0 .. $#children ) {
            my @childData = @{ $children[$childIX] };
            if ( $childData[0] eq 'node' ) {
                shift @childData;
                push @results,
                  [ $rhs[$childIX], Marpa::R2::Context::location(),
                    @childData ];
                say STDERR join "!", __FILE__, __LINE__, @{$results[$#results]};
                next CHILD;
            }
            push @results, [ $rhs[$childIX], Marpa::R2::Context::location() ];
            say STDERR join "!", __FILE__, __LINE__, @{$results[$#results]};
        }
        last RESULT;
    }
    return [ "node", @results ];
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

say Data::Dumper::Dumper($astRef);

# vim: expandtab shiftwidth=4:
