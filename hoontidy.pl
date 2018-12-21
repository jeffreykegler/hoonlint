# Hoon "tidy" utility

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

require "yahc.pm";

my @data = ();
my $grammar;
my $recce;

sub doNode {
    my ( undef, @children ) = @_;
    my @results = ();
    my $childCount = scalar @children;
    my $ruleID     = $Marpa::R2::Context::rule;
    my ( $lhs, @rhs ) =
      map { $grammar->symbol_display_form($_) } $grammar->rule_expand($ruleID);
    if ($childCount <= 0) {
        return [ 'null', $lhs ];
    }
  RESULT: {
        if ( ( scalar @rhs ) != $childCount ) {
            # This is a non-trivial (that is, longer than one item) sequence rule.
            my $childIX = 0;
          CHILD: for ( ; ; ) {
                say STDERR "childIX=$childIX; last children ix = $#children";
                my @childData = @{ $children[$childIX] };
                my $firstChildElement = shift @childData;
                $childIX++;
              ITEM: {
                    if ( $firstChildElement eq 'node' ) {
                        push @results, [@childData];
                        say STDERR join "!", __FILE__, __LINE__,
                          @{ $results[$#results] };
                        last ITEM;
                    }
                    if ( $firstChildElement eq 'null' ) {
                        push @results, [@childData];
                        say STDERR join "!", __FILE__, __LINE__,
                          @{ $results[$#results] };
                        last ITEM;
                    }
                    my ($lexemeLength, $lexemeName) = @childData;
                    push @results, [$lexemeName, $firstChildElement, $lexemeLength];
                }
                last RESULT if $childIX >= $#children;
                push @results, ['separator', 'NYI'];
                say STDERR join "!", __FILE__, __LINE__,
                  @{ $results[$#results] };
            }
            last RESULT;
        }
      # All other rules
      CHILD: for my $childIX ( 0 .. $#children ) {
            my @childData         = @{ $children[$childIX] };
            my $firstChildElement = shift @childData;
            if ( $firstChildElement eq 'node' ) {
                push @results, [@childData];
                say STDERR join "!", __FILE__, __LINE__,
                  @{ $results[$#results] };
                next CHILD;
            }
            if ( $firstChildElement eq 'null' ) {
                push @results, [@childData];
                say STDERR join "!", __FILE__, __LINE__,
                  @{ $results[$#results] };
                next CHILD;
            }
            my ( $lexemeLength, $lexemeName ) = @childData;
            push @results, [ $lexemeName, $firstChildElement, $lexemeLength ];
        }
        last RESULT;
    }
    my ($first_g1, $last_g1) = Marpa::R2::Context::location();
    my ($lhsStart) = $recce->g1_location_to_span($first_g1+1);
    my ($last_g1_start, $last_g1_length) = $recce->g1_location_to_span($last_g1);
    my $lhsLength = $last_g1_start+$last_g1_length-$lhsStart;
    say STDERR "Returning node for $lhs ($lhsStart, $lhsLength):\n", 
       $recce->literal($lhsStart, $lhsLength);
    return [ "node", $lhs, $lhsStart, $lhsLength, @results ];
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
$recce = $parser->rawRecce();
my $astRef = $recce->value();

die "Parse failed" if not $astRef;

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

say Data::Dumper::Dumper($astRef);

# vim: expandtab shiftwidth=4:
