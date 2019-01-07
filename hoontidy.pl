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
    my ($first_g1, $last_g1) = Marpa::R2::Context::location();
    my ($lhsStart) = $recce->g1_location_to_span($first_g1+1);
    if ($childCount <= 0) {
        return {
            type   => 'null',
            symbol  => $lhs,
            start => $lhsStart,
            length => 0,
        };
    }
    my ($last_g1_start, $last_g1_length) = $recce->g1_location_to_span($last_g1);
    my $lhsLength = $last_g1_start+$last_g1_length-$lhsStart;
  RESULT: {
        my $lastLocation = $lhsStart;
        if ( ( scalar @rhs ) != $childCount ) {
            # This is a non-trivial (that is, longer than one item) sequence rule.
            my $childIX = 0;
            my $lastSeparator;
          CHILD: for ( ; ; ) {
                # say STDERR "childIX=$childIX; last children ix = $#children";
                my $child = $children[$childIX];
                my $childType = $child->{type};
                $childIX++;
              ITEM: {
                    if (defined $lastSeparator) {
                       my $length = $child->{start}-$lastSeparator->{start};
                       $lastSeparator->{length} = $length;
                    }
                    push @results, $child;
                    $lastLocation = $child->{start} + $child->{length};
                }
                last RESULT if $childIX > $#children;
                my $separator = $separator{$lhs};
                next CHILD unless $separator;
                $lastSeparator = { type=>'separator',
                    symbol => $separator,
                    start => $lastLocation,
                    # length supplied later
                    };
                push @results, $lastSeparator;
            }
            last RESULT;
        }
      # All other rules
      CHILD: for my $childIX ( 0 .. $#children ) {
            # say STDERR Data::Dumper::Dumper( $children[$childIX] );
            my $child = $children[$childIX];
            my $refType = ref $child;
            if ($refType eq 'ARRAY') {
                my ( $lexemeStart, $lexemeLength, $lexemeName ) = @{$child};
                push @results, { type=>'lexeme',
                    start => $lexemeStart,
                    length => $lexemeLength,
                    symbol => $lexemeName,
                    };
                next CHILD;
            }
            push @results, $child;
        }
        last RESULT;
    }
    return { type=>'node',
        ruleID => $ruleID,
        start => $lhsStart,
        length => $lhsLength,
        children => \@results,
    };
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

sub literal {
    my ($start, $length) = @_;
    return substr $hoonSource, $start, $length;
}

sub column {
    my ($pos) = @_;
    return $pos - (rindex $hoonSource, "\n", $pos-1);
}

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
        my $nodeRef = ref $node;
        my $children = $node->{children};
        if ( not $children ) {
            my $start = $node->{start};
            my $length = $node->{length};
            print literal( $start, $length );
            next NODE;
        }
        for my $child (@{$children}) {
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

    my $currentColumn = 0;
    my @currentLine = ();
    my @output = ();
        my $printLine = sub {
            # say STDERR "=== called printLine";
            PIECE: for my $piece (@currentLine) {
               if (not ref $piece) {
                   # say STDERR qq{processing piece, piece="$piece"};
                   push @output, $piece;
                   $currentColumn += length $piece;
                   next PIECE;
               }
               my ($command, $indent) = @{$piece};
               if ($command eq 'nl') {
                   # say STDERR "processing nl, indent=$indent";
                   push @output, "\n";
                   push @output, (q{ } x ($indent));
                   $currentColumn = $indent;
                   next PIECE;
               }
               if ($command eq 'tab') { # indent is desired 0-based tab location
                   # say STDERR "processing tab, indent=$indent";
                   # say STDERR qq{line so far: "$line"};
                   my $spaces = $indent - $currentColumn;
                   if ($spaces < 1 and $currentColumn > 0) {
                       # Always leave at least one space between a comment and preceeding text.
                       $spaces = 1;
                   }
                   # say STDERR qq{spaces=$spaces};
                   $currentColumn += $spaces;
                   push @output, (q{ } x $spaces) if $spaces > 1;
                   next PIECE;
               }
               die qq{Command "$command" not implemented};
            }
        };

    sub applyTestStyle {
        no warnings 'recursion';
        my ( $baseIndent, $depth, $node ) = @_;
        # say STDERR "applyTestStyle($baseIndent, $depth, ...)";
        my @pieces = ();

        my $gapToPieces = sub {
            my ( $start, $length ) = @_;
            my $literal = literal( $start, $length );
            my $currentNL = index $literal, "\n";
            if ( $currentNL < 0 ) {

                # Normalize gap to 2 spaces
                push @pieces, q{  };
                return;
            }
            my $lastNL = -1;

            # Convert initialColumn to 0-based
            my $initialColumn = column($start) - 1;
          LEADING_LINES: for ( ; ; ) {
                pos $literal = $lastNL + 1;
                my ($spaces) = ( $literal =~ m/\G([ ]*)/ );
                die if not defined $spaces;   # TODO: is this necessary?
                                              # say STDERR qq{spaces="$spaces"};
                my $spaceCount      = length $spaces;
                my $firstCommentPos = $lastNL + $spaceCount + 1;
                if ( substr( $literal, $firstCommentPos, 1 ) ne "\n" ) {

                    # say STDERR "pushing tab, indent=",
                    # $initialColumn + $spaceCount;
                    push @pieces, [ 'tab', $initialColumn + $spaceCount ];

                    # say STDERR +( join " ", __FILE__, __LINE__, '' ),
                    # qq{pushing piece: "},
                    # substr( $literal, $firstCommentPos,
                    # ( $currentNL - $firstCommentPos ) ),
                    # q{"};
                    push @pieces,
                      substr( $literal, $firstCommentPos,
                        ( $currentNL - $firstCommentPos ) );
                }
                my $nextNL = index $literal, "\n", $currentNL + 1;
                last LEADING_LINES if $nextNL < 0;
                push @pieces, [ 'nl', 0 ];
                $lastNL        = $currentNL;
                $currentNL     = $nextNL;
                $initialColumn = 0;
            }
            push @pieces, [ 'nl', $baseIndent ];
            return;
        };

      NODE: {
            die Data::Dumper::Dumper($node)
              if ref $node ne 'HASH';    # TODO: delete after development
            my $type = $node->{type};

            # say STDERR "= $type $key\n";
            if ( $type eq 'null' ) {
                last NODE;
            }
            if ( $type eq 'lexeme' ) {
                my $symbol = $node->{symbol};
                my $start  = $node->{start};
                my $length = $node->{length};
                if ( $symbol eq 'GAP' ) {
                    $gapToPieces->( $start, $length );
                    last NODE;
                }
                if ( $symbol =~ m/^[B-Z][AEOIU][B-Z][B-Z][AEIOU][B-Z]GAP$/ ) {
                    push @pieces, literal( $start, 2 );
                    $gapToPieces->( $start + 2, $length - 2 );
                    last NODE;
                }

# say STDERR +(join " ", __FILE__, __LINE__, ''), qq{pushing piece: "}, literal( $start, $length ), q{"};
                push @pieces, literal( $start, $length );
                last NODE;
            }
            if ( $type eq 'separator' ) {
                my $symbol = $node->{symbol};
                my $start  = $node->{start};
                my $length = $node->{length};
                if ( $symbol eq 'GAP' ) {

                    # say STDERR +( join " ", __FILE__, __LINE__, '' ),
                    # qq{pushing piece: "}, literal( $start, $length ),
                    # q{"};
                    $gapToPieces->( $start, $length );
                    last NODE;
                }
                push @pieces, literal( $start, $length );
                last NODE;
            }
            my $ruleID = $node->{ruleID};
            die Data::Dumper::Dumper($node) if not defined $ruleID;
            my ( $lhs, @rhs ) = $grammar->rule_expand( $node->{ruleID} );
            my $lhsName = $grammar->symbol_name($lhs);

            # say STDERR join " ", __FILE__, __LINE__, "lhsName=$lhsName";
            if ( $lhsName eq 'optGay4i' ) {
                my $start  = $node->{start};
                my $length = $node->{length};
                $gapToPieces->( $start, $length );
                last NODE;
            }

            my $children   = $node->{children};
            my $childCount = scalar @{$children};
            last NODE if $childCount <= 0;
            if ( $childCount == 1 ) {
                applyTestStyle( $baseIndent, $depth + 1, $children->[0] );
                last NODE;
            }

            my $firstChildIndent = column( $children->[0]->{start} ) - 1;

# say STDERR join " ", "$lhsName: column=$firstChildIndent", "baseIndent=$baseIndent";
            $baseIndent = $firstChildIndent if $firstChildIndent > $baseIndent;

            # say STDERR join " ", "$lhsName: baseIndent=$baseIndent";

            if ( $lhsName eq 'wisp5d' ) {

                # special case for battery
                for my $child ( @{$children} ) {
                    applyTestStyle( $baseIndent, $depth + 1, $child );
                }
                last NODE;
            }

            if ( $lhsName eq 'lusLusCell' ) {

                # special case for battery
                for my $child ( @{$children} ) {
                    applyTestStyle( $baseIndent + 2, $depth + 1, $child );
                }
                last NODE;
            }

            my $gapiness = $ruleDB[$ruleID]->{gapiness} // 0;
            if ( $gapiness < 0 ) {    # sequence
                for my $child (@$children) {
                    applyTestStyle( $baseIndent, $depth + 1, $child );
                }
                last NODE;
            }
            if ( $gapiness == 0 ) {    # wide node
                for my $child (@$children) {
                    applyTestStyle( $baseIndent, $depth + 1, $child );
                }
                last NODE;
            }

            # tall node
            my $vertical_gaps = 0;
            my @isVerticalGap;
            my @isGap;
          CHILD: for my $childIX ( 0 .. $#$children ) {
                my $child = $children->[$childIX];
                next CHILD if $child->{type} ne 'lexeme';
                my $name  = $child->{symbol};
                my $isGap = $symbolReverseDB{$name}->{gap};
                $isGap[$childIX] = $isGap;
                next CHILD if not $isGap;
                my $start  = $child->{start};
                my $length = $child->{length};

                if ( literal( $start, $length ) =~ /\n/ ) {
                    $vertical_gaps++;
                    $isVerticalGap[$childIX]++;
                }
            }

            # Do we use alignment, or just backdenting?
            my $useAlignment = 1;
            SET_ALIGNMENT: {
                # Use alignment if first gap is non-vertical and
                # the rest are vertical
                my $gapCount = 0;
              CHILD: for my $childIX ( 0 .. $#$children ) {
                    next CHILD if not $isGap[$childIX];
                    $gapCount++;
                    if ( $isVerticalGap[$childIX] ) {
                        if ( $gapCount == 1 ) {
                            $useAlignment = 0;
                            last SET_ALIGNMENT;
                        }
                        next CHILD;
                    }
                    if ( $gapCount > 1 ) {
                        $useAlignment = 0;
                        last SET_ALIGNMENT;
                    }
                }
            }
            # if ($useAlignment) {
            if (0) {
                # say STDERR "Using alignment!!!";
                my $alignedIndent = $baseIndent;
              CHILD: for my $childIX ( 0 .. $#$children ) {
                    my $child = $children->[$childIX];
                    if ( $isGap[$childIX] and not $isVerticalGap[$childIX] ) {
                        $alignedIndent =
                          applyTestStyle( $alignedIndent, $depth + 1, $child );
                        next CHILD;
                    }
                    applyTestStyle( $alignedIndent, $depth + 1, $child );
                }
                last NODE;
            }
            # If here, use backdenting
            my $currentIndent = $baseIndent + $vertical_gaps * 2;
          CHILD: for my $childIX ( 0 .. $#$children ) {
                $currentIndent -= 2 if $isVerticalGap[$childIX];
                my $child = $children->[$childIX];
                applyTestStyle( $currentIndent, $depth + 1, $child );
            }
        }
      PIECE: for my $piece (@pieces) {
            push @currentLine, $piece;
        }

        return $baseIndent;
    }

    applyTestStyle(0, 0, $astValue);
    $grammar = undef;    # free up memory
    $recce = undef;    # free up memory
    $printLine->();
    print join q{}, @output;
}

# vim: expandtab shiftwidth=4:
