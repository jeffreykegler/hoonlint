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
            old    => [ 'null', $lhs ]
        };
    }
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
                my $child = $children[$childIX];
                my @childData = @{ $child->{old} };
                my $childType = $child->{type};
                $childIX++;
              ITEM: {
                    if ( $childType eq 'node' ) {
                        push @results, { type=>'node',
                            start => $childData[2],
                            length => $childData[3],
                            old=>[@childData]};
                        if (defined $lastSeparator) {
                           my $lastSeparatorData = $lastSeparator->{old};
                            my $length =
                              $child->{start} - $lastSeparator->{start};
                           $lastSeparatorData->[3] = $length;
                           $lastSeparator->{length} = $length;
                        }
                        $lastLocation = $childData[2] + $childData[3];
                        last ITEM;
                    }
                    if ( $childType eq 'null' ) {
                        push @results,
                          {
                            type   => 'null',
                            symbol => $childData[1],
                            start  => $lastLocation,
                            length => 0,
                            old => [ 'null', $childData[1], $lastLocation, 0 ]
                          };
                        if ( defined $lastSeparator ) {
                            my $lastSeparatorData = $lastSeparator->{old};
                            my $length =
                              $child->{start} - $lastSeparator->{start};
                           $lastSeparatorData->[3] = $length;
                           $lastSeparator->{length} = $length;
                        }

                        # say STDERR join "NULL !", __FILE__, __LINE__,
                        # @{ $results[$#results] };
                        last ITEM;
                    }
                    if (defined $lastSeparator) {
                       my $lastSeparatorData = $lastSeparator->{old};
                       my $length = $childData[0]-$lastSeparator->{start};
                       $lastSeparatorData->[3] = $length;
                       $lastSeparator->{length} = $length;
                    }
                    my ($lexemeStart, $lexemeLength, $lexemeName) = @childData;
                    push @results, { type=>'lexeme',
                        symbol => $lexemeName,
                        start => $lexemeStart,
                        length => $lexemeLength,
                        old=>['lexeme', $lexemeName, $lexemeStart, $lexemeLength]
                    };
                }
                last RESULT if $childIX > $#children;
                my $separator = $separator{$lhs};
                next CHILD unless $separator;
                $lastSeparator = { type=>'separator',
                    symbol => $separator,
                    start => $lastLocation,
                    # length supplied later
                    old => ['separator', $separator, $lastLocation, 0]};
                push @results, $lastSeparator;
            }
            last RESULT;
        }
      # All other rules
      CHILD: for my $childIX ( 0 .. $#children ) {
            # say STDERR Data::Dumper::Dumper( $children[$childIX] );
            my $childRef = $children[$childIX];
            my $refType = ref $childRef;
            if ($refType eq 'ARRAY') {
                my ( $lexemeStart, $lexemeLength, $lexemeName ) = @{$childRef};
                push @results, { type=>'lexeme',
                    start => $lexemeStart,
                    length => $lexemeLength,
                    symbol => $lexemeName,
                    old => [ 'lexeme', $lexemeName, $lexemeStart, $lexemeLength ] };
                next CHILD;
            }
            my @childData         = @{ $children[$childIX]->{old} };
            my $dataType = $childData[0];
            if ( $dataType eq 'node' ) {
                push @results, { type=>'node',
                    start => $childData[2],
                    length => $childData[3],
                    old=> [@childData] };
                next CHILD;
            }
            if ( $dataType eq 'null' ) {
                push @results, { type=>'null',
                    start => $lastLocation,
                    length => 0,
                    old=> [@childData, $lastLocation, 0] };
                next CHILD;
            }
        }
        last RESULT;
    }
    return { type=>'node',
        ruleID => $ruleID,
        start => $lhsStart,
        length => $lhsLength,
        children => \@results,
    old => [ 'node', $ruleID, $lhsStart, $lhsLength, @results ] };
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
        my $nodeRef = ref $node;
        my $children = $node->{children};
        if ( not $children ) {
            my $start = $node->{start};
            my $length = $node->{length};
            print $recce->literal( $start, $length );
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

    my @currentLine = ();
        my $printLine = sub {
            # say STDERR "=== called printLine";
            my @lineSoFar = ();
            PIECE: for my $piece (@currentLine) {
               if (not ref $piece) {
                   # say STDERR qq{processing piece, piece="$piece"};
                   push @lineSoFar, $piece;
                   next PIECE;
               }
               my ($command, $indent) = @{$piece};
               if ($command eq 'nl') {
                   # say STDERR "processing nl, indent=$indent";
                   push @lineSoFar, "\n";
                   push @lineSoFar, (q{ } x ($indent));
                   next PIECE;
               }
               if ($command eq 'tab') { # indent is desired 0-based tab location
                   # say STDERR "processing tab, indent=$indent";
                   my $line = join q{}, @lineSoFar;
                   my $lastNlPos = rindex $line, "\n";
                   my $currentColumn; # 0-based
                   if ($lastNlPos < 0) {
                       $currentColumn = length $line;
                   } else {
                       $currentColumn = (length $line) - ($lastNlPos + 1);
                   }
                   # say STDERR qq{lastNlPos=$lastNlPos; currentColumn=$currentColumn; line="$line"};
                   my $spaces = $indent - $currentColumn;
                   if ($spaces < 1 and $currentColumn > 0) {
                       # Always leave at least one space between a comment and preceeding text.
                       $spaces = 1;
                   }
                   # say STDERR qq{spaces=$spaces};
                   @lineSoFar = ($line);
                   push @lineSoFar, (q{ } x $spaces) if $spaces > 1;
                   next PIECE;
               }
               die qq{Command "$command" not implemented};
            }
            # say STDERR "=== printing line: ", join q{}, @lineSoFar;
            print join q{}, @lineSoFar;
        };

    sub applyTestStyle {
        no warnings 'recursion';
        my ($indent, $depth, @nodes) = @_;
        my @pieces = ();

        my $gapToPieces = sub {
            my ( $start, $length ) = @_;
            my $literal = $recce->literal( $start, $length );
            my $currentNL = index $literal, "\n";
            if ($currentNL < 0) {
                   # say STDERR +(join " ", __FILE__, __LINE__, ''), qq{pushing piece: "$literal"};
                push @pieces, $literal;
                return;
            }
            my $lastNL = -1;
            # Convert initialColumn to 0-based
            my $initialColumn = $recce->line_column($start)-1;
          LEADING_LINES: for ( ; ; ) {
                pos $literal = $lastNL + 1;
                my ($spaces) = ( $literal =~ m/\G([ ]*)/ );
                die if not defined $spaces; # TODO: is this necessary?
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
            push @pieces, [ 'nl', $indent ];
            return;
        };

      NODE: for my $node (@nodes) {
            die Data::Dumper::Dumper($node) if ref $node ne 'HASH'; # TODO: delete after development
            my ( undef, $key, $start, $length, @children ) = @{$node->{old}};
            my $type = $node->{type};
            # say STDERR "= $type $key\n";
            if ( not defined $start ) {
                die join "Problem node: ", @{$node};
            }
            if ( $type eq 'null' ) {
                next NODE;
            }
            if ($type eq 'lexeme') {
                my $symbol = $node->{symbol};
                die Data::Dumper::Dumper($node) if not defined $symbol;
                if ($symbol eq 'GAP') {
                     $gapToPieces->($start, $length);
                     next NODE;
                }
                if ($symbol =~ m/^[B-Z][AEOIU][B-Z][B-Z][AEIOU][B-Z]GAP$/) {
                  push @pieces, $recce->literal( $start, 2 );
                  $gapToPieces->($start+2, $length-2);
                  next NODE;
                }
                   # say STDERR +(join " ", __FILE__, __LINE__, ''), qq{pushing piece: "}, $recce->literal( $start, $length ), q{"};
                push @pieces, $recce->literal( $start, $length );
                next NODE;
            }
            if ( $type eq 'separator' ) {
                if ( $key eq 'GAP' ) {
                    # say STDERR +( join " ", __FILE__, __LINE__, '' ),
                      # qq{pushing piece: "}, $recce->literal( $start, $length ),
                      # q{"};
                    $gapToPieces->( $start, $length );
                    next NODE;
                }
                push @pieces, $recce->literal( $start, $length );
                next NODE;
            }
            my ( $lhs, @rhs ) = $grammar->rule_expand($key);
            my $lhsName = $grammar->symbol_name($lhs);
            # say STDERR join " ", "lhsName=$lhsName";

            if ($lhsName eq 'wisp5d') {
                # special case for battery
                for my $child (@children) {
                    applyTestStyle($indent, $depth+1, $child);
                }
                next NODE;
            }

            if ($lhsName eq 'lusLusCell') {
                # special case for battery
                for my $child (@children) {
                    applyTestStyle($indent+2, $depth+1, $child);
                }
                next NODE;
            }

            my $childCount = scalar @children;
            next NODE if $childCount <= 0;
            if ($childCount == 1) {
                applyTestStyle($indent, $depth+1, $children[0]);
                next NODE;
            }
            my $gapiness = $ruleDB[$key]->{gapiness} // 0;
            if ($gapiness < 0) { # sequence
                for my $child (@children) {
            if (ref $child ne 'HASH') {
                my $ruleID = $child->[1];
                die join " ", map { $grammar->symbol_display_form($_) } $grammar->rule_expand($ruleID);
                $node = { 'old' => $child };
            }
                    applyTestStyle($indent, $depth+1, $child);
                }
                next NODE;
            }
            if ($gapiness == 0) { # wide node
                for my $child (@children) {
                    my $wrappedChild = ((ref $child) eq 'HASH') ? $child :
                        { 'old' => $child };
                    applyTestStyle($indent, $depth+1, $wrappedChild);
                }
                next NODE;
            }
            # tall node
            my $vertical_gaps = 0;
            my @isVerticalChild;
            CHILD: for my $childIX (0 .. $#children) {
                my $child = $children[$childIX];
                next CHILD if $child->{type} ne 'lexeme';
                my $name = $child->{symbol};
                next CHILD if not $symbolReverseDB{$name}->{gap};
                my $start = $child->{start};
                my $length = $child->{length};
                next CHILD unless $recce->literal($start, $length) =~ /\n/;
                $vertical_gaps++;
                $isVerticalChild[$childIX]++;
            }
            my $currentIndent = $indent + $vertical_gaps*2;
            CHILD: for my $childIX (0 .. $#children) {
                my $child = $children[$childIX];
                if ($isVerticalChild[$childIX]) {
                    $currentIndent -= 2;
                    applyTestStyle($currentIndent, $depth+1, $child);
                    next CHILD;
                }
                applyTestStyle($currentIndent, $depth+1, $child);
            }
        }
        PIECE: for my $piece (@pieces) {
           if (not ref $piece) {
               push @currentLine, $piece;
               next PIECE;
           }
           my ($command, $indent) = @{$piece};
           if ($command eq 'nl') {
                   $printLine->();
                   @currentLine = ($piece);
                   next PIECE;
               }
               push @currentLine, $piece;
            };
        }

    # $grammar = undef;    # free up memory
    applyTestStyle(0, 0, $astValue);
    $printLine->() if @currentLine;
}

# vim: expandtab shiftwidth=4:
