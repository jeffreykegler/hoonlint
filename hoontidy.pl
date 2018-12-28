# Hoon "tidy" utility

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );
use Getopt::Long;

require "yahc.pm";

my $style;

GetOptions ( "style=s"  => \$style)   # flag
  or die("Error in command line arguments\n");

CHECK_STYLE: {
    if ( not $style ) {
        say "usage: $PROGRAM_NAME --style=roundtrip";
        die "A style option must be set";
    }
    last CHECK_STYLE if $style eq 'test';
    last CHECK_STYLE if $style eq 'roundtrip';
    die qq{Unknown style option: "$style"};
}

my @data = ();
my $grammar;
my $recce;

my %separator = qw(
  hyf4jSeq DOT
  singleQuoteCord gon4k
  dem4k gon4k
  timePeriodKernel DOT
  optBonzElements GAP
  optWideBonzElements ACE
  till5dSeq GAP
  wyde5dSeq ACE
  gash5d FAS
  togaElements ACE
  wide5dJogs wide5dJoggingSeparator
  rope5d DOT
  rick5d GAP
  wideRick5d commaAce
  ruck5d GAP
  wideRuck5d commaAce
  tallTopKidSeq  GAP_SEM
  wideInnerTops ACE
  wideAttrBody commaAce
  scriptStyleTailElements GAP
  moldInfixCol2 COL
  lusSoilSeq DOG4I
  hepSoilSeq DOG4I
  infixDot DOG4I
  waspElements GAP
  whap5d GAP
  hornSeq GAP
  wideHornSeq ACE
  fordHoopSeq GAP
  tall5dSeq GAP
  wide5dSeq ACE
  fordFascomElements GAP
  optFordHithElements FAS
  fordHoofSeq commaWS
);

sub doNode {
    my ( undef, @children ) = @_;
    my @results = ();
    my $childCount = scalar @children;
    no warnings 'once';
    my $ruleID     = $Marpa::R2::Context::rule;
    use warnings;
    my ( $lhs, @rhs ) =
      map { $grammar->symbol_display_form($_) } $grammar->rule_expand($ruleID);
    if ($childCount <= 0) {
        return [ 'null', $lhs ];
    }
    my ($first_g1, $last_g1) = Marpa::R2::Context::location();
    my ($lhsStart) = $recce->g1_location_to_span($first_g1+1);
    my ($last_g1_start, $last_g1_length) = $recce->g1_location_to_span($last_g1);
    my $lhsLength = $last_g1_start+$last_g1_length-$lhsStart;
    # say STDERR "Returning node for $lhs ($lhsStart, $lhsLength):\n", 
       # $recce->literal($lhsStart, $lhsLength);
  RESULT: {
        my $lastLocation = $lhsStart;
        if ( ( scalar @rhs ) != $childCount ) {
            # This is a non-trivial (that is, longer than one item) sequence rule.
            my $childIX = 0;
            my $lastSeparator;
          CHILD: for ( ; ; ) {
                # say STDERR "childIX=$childIX; last children ix = $#children";
                my @childData = @{ $children[$childIX] };
                my $childType = $childData[0];
                $childIX++;
              ITEM: {
                    if ( $childType eq 'node' ) {
                        push @results, [@childData];
                        if (defined $lastSeparator) {
                           $lastSeparator->[3] = $childData[2]-($lastSeparator->[2]);
                        }
                        $lastLocation = $childData[2] + $childData[3];
                        last ITEM;
                    }
                    if ( $childType eq 'null' ) {
                        push @results, ['null', $childData[1], $lastLocation, 0];
                        if (defined $lastSeparator) {
                           $lastSeparator->[3] = $childData[2]-($lastSeparator->[2]);
                        }
                        # say STDERR join "NULL !", __FILE__, __LINE__,
                          # @{ $results[$#results] };
                        last ITEM;
                    }
                    if (defined $lastSeparator) {
                       $lastSeparator->[3] = $childData[0]-($lastSeparator->[2]);
                    }
                    my ($lexemeStart, $lexemeLength, $lexemeName) = @childData;
                    push @results, ['lexeme', $lexemeName, $lexemeStart, $lexemeLength];
                }
                last RESULT if $childIX > $#children;
                my $separator = $separator{$lhs};
                next CHILD unless $separator;
                $lastSeparator = ['separator', $separator, $lastLocation, 0];
                push @results, $lastSeparator;
            }
            last RESULT;
        }
      # All other rules
      CHILD: for my $childIX ( 0 .. $#children ) {
            my @childData         = @{ $children[$childIX] };
            my $dataType = $childData[0];
            if ( $dataType eq 'node' ) {
                push @results, [@childData];
                next CHILD;
            }
            if ( $dataType eq 'null' ) {
                push @results, [@childData, $lastLocation, 0];
                next CHILD;
            }
            my ( $lexemeStart, $lexemeLength, $lexemeName ) = @childData;
            push @results, [ 'lexeme', $lexemeName, $lexemeStart, $lexemeLength ];
        }
        last RESULT;
    }
    return [ 'node', $ruleID, $lhsStart, $lhsLength, @results ];
}

my $hoonSource = do {
  local $RS = undef;
  <>;
};

my $semantics = <<'EOS';
:default ::= action=>main::doNode
lexeme default = latm => 1 action=>[start,length,name]
EOS

my $parser = MarpaX::YAHC::new( { semantics => $semantics, all_symbols => 1 } );
$grammar = $parser->rawGrammar();
$parser->read(\$hoonSource);
$recce = $parser->rawRecce();
$parser = undef; # free up memory
my $astRef = $recce->value();

die "Parse failed" if not $astRef;

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

# say Data::Dumper::Dumper($astRef);

my $astValue = ${$astRef};

if ($style eq 'roundtrip') {
    roundTrip($astValue);
}

sub roundTrip {
   # free up memory
   $grammar = undef;
   no warnings 'recursion';
   NODE: for my $node (@_) {
       my ($type, $lhs, $start, $length, @children) = @{$node};
       if (not defined $start) {
           die join "Problem node: ", @{$node};
       }
       if (not @children) {
           print $recce->literal($start, $length);
           next NODE;
       }
       for my $child (@children) {
           roundTrip($child);
       }
   }
}

if ( $style eq 'test' ) {

    my @ruleDB          = ();
    my @symbolDB        = ();
    my %symbolReverseDB = ();

    sub testStyleCensus {
      SYMBOL:
        for my $symbolID ( $grammar->symbol_ids() ) {
            my $name = $grammar->symbol_name($symbolID);
            my $data = {};
            $data->{name}   = $name;
            $data->{id}     = $symbolID;
            $data->{lexeme} = 1;           # default to lexeme
            $data->{gap} = 1 if $name eq 'GAP';
            $data->{gap} = 1 if $name =~ m/^[B-Z][AEOIU][B-Z][B-Z][AEIOU][B-Z]GAP$/;
            $symbolDB[$symbolID] = $data;
            $symbolReverseDB{$name} = $data;
        }
        my $gapID = $symbolReverseDB{'GAP'}->{id};
      RULE:
        for my $ruleID ( $grammar->rule_ids() ) {
            my $data = { id => $ruleID };
            my ( $lhs, @rhs ) = $grammar->rule_expand($ruleID);
            $data->{symbols} = [ $lhs, @rhs ];
            my $lhsName       = $grammar->symbol_name($lhs);
            my $separatorName = $separator{$lhsName};
            if ($separatorName) {
                my $separatorID = $symbolReverseDB{$separatorName}->{id};
                $data->{separator} = $separatorID;
                if ($separatorID == $gapID) {
                    $data->{gapiness} = -1;
                }
            }
            if (not defined $data->{gapiness}) {
                for my $rhsID (@rhs) {
                    $data->{gapiness}++ if $symbolDB[$rhsID]->{gap};
                }
            }
            $ruleDB[$ruleID] = $data;
            $symbolReverseDB{$lhs}->{lexeme} = 0;
        }

      # for my $symbolID ( $grammar->symbol_ids() ) { 
          # say STDERR Data::Dumper::Dumper($symbolDB[$symbolID]);
      # }
      # for my $ruleID ( $grammar->rule_ids() ) { 
          # say STDERR Data::Dumper::Dumper($ruleDB[$ruleID]);
      # }
    }

    testStyleCensus();

    sub applyTestStyle {
        no warnings 'recursion';
        my ($depth, @nodes) = @_;
        my @pieces = ();
      NODE: for my $node (@nodes) {
            my ( $type, $key, $start, $length, @children ) = @{$node};
            # say STDERR "= $type $key\n";
            if ( not defined $start ) {
                die join "Problem node: ", @{$node};
            }
            if ($type eq 'lexeme') {
                if ($key eq 'GAP') {
                  my $literal = $recce->literal( $start, $length );
                  my $lastNL = rindex $literal, "\n";
                  if ($lastNL < 0) {
                      push @pieces, $literal;
                      next NODE;
                  }
                  push @pieces, substr($literal, 0, $lastNL);
                  push @pieces, ['nl', $depth];
                    next NODE;
                }
                if ($key =~ m/^[B-Z][AEOIU][B-Z][B-Z][AEIOU][B-Z]GAP$/) {
                  my $literal = $recce->literal( $start, $length );
                  my $lastNL = rindex $literal, "\n";
                  if ($lastNL < 0) {
                      push @pieces, $literal;
                      next NODE;
                  }
                  push @pieces, substr($literal, 0, $lastNL);
                  push @pieces, "\n" . (q{ } x ($depth*2));
                    next NODE;
                }
                push @pieces, $recce->literal( $start, $length );
                next NODE;
            }
            if ($type eq 'separator') {
                push @pieces, $recce->literal( $start, $length );
                next NODE;
            }
            my $childCount = scalar @children;
            next NODE if $childCount <= 0;
            if ($childCount == 1) {
                applyTestStyle($depth, $children[0]);
                next NODE;
            }
            my $gapiness = $ruleDB[$key]->{gapiness} // 0;
            if ($gapiness < 0) { # sequence
                for my $child (@children) {
                    applyTestStyle($depth+1, $child);
                }
                next NODE;
            }
            if ($gapiness == 0) { # wide node
                for my $child (@children) {
                    applyTestStyle($depth, $child);
                }
                next NODE;
            }
            # tall node
            my $vertical_gaps = 0;
            my @isVerticalChild;
            CHILD: for my $childIX (0 .. $#children) {
                my ($type, $name, $start, $length) = @{$children[$childIX]};
                next CHILD if $type ne 'lexeme';
                next CHILD if not $symbolReverseDB{$name}->{gap};
                next CHILD unless $recce->literal($start, $length) =~ /\n/;
                $vertical_gaps++;
                $isVerticalChild[$childIX]++;
            }
            my $currentDepth = $depth + $vertical_gaps;
            CHILD: for my $childIX (0 .. $#children) {
                my $child = $children[$childIX];
                if ($isVerticalChild[$childIX]) {
                    $currentDepth--;
                    applyTestStyle($currentDepth, $child);
                    next CHILD;
                }
                applyTestStyle($currentDepth, $child);
            }
        }
        my @currentLine = ();
        my $printLine = sub {
            my @lineSoFar = ();
            PIECE: for my $piece (@currentLine) {
               if (not ref $piece) {
                   push @lineSoFar, $piece;
                   next PIECE;
               }
               $DB::single = 1;
               my ($command, $indent) = @{$piece};
               if ($command eq 'nl') {
                   push @lineSoFar, "\n";
                   push @lineSoFar, (q{ } x ($indent*2));
                   next PIECE;
               }
               die qq{Command "$command" not implemented};
            }
            print join q{}, @lineSoFar;
        };
        PIECE: for my $piece (@pieces) {
           if (not ref $piece) {
               push @currentLine, $piece;
               next PIECE;
           }
               $DB::single = 1;
           my ($command, $indent) = @{$piece};
           if ($command eq 'nl') {
               $printLine->();
               @currentLine = ($piece);
               next PIECE;
           }
           push @currentLine, $piece;
        };
        $printLine->() if @currentLine;
    }

    $grammar = undef;    # free up memory
    applyTestStyle(0, $astValue);
}

# vim: expandtab shiftwidth=4:
