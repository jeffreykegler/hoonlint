# Hoon whitespace "test" policy

package MarpaX::YAHC::Lint::Policy::Test::Whitespace;

use 5.010;
use strict;
use warnings;
no warnings 'recursion';

use Data::Dumper;
use English qw( -no_match_vars );
use Scalar::Util qw(looks_like_number weaken);

# say STDERR join " ", __FILE__, __LINE__, "hi";

# TODO: delete ancestors, indents in favor of tree traversal

sub new {
    my ( $class, $lintInstance ) = @_;
    my $policy = {};
    $policy->{lint} = $lintInstance;
    Scalar::Util::weaken( $policy->{lint} );
    return bless $policy, $class;
}

sub calcGapIndents {
    my ( $policy, $node ) = @_;
    my $instance        = $policy->{lint};
    my $symbolReverseDB = $instance->{symbolReverseDB};
    my $recce           = $instance->{recce};
    my $children        = $node->{children};
    my @gapIndents      = ();
    my $child           = $children->[0];
    my $childStart      = $child->{start};
    my ( $childLine, $childColumn ) = $recce->line_column($childStart);
    push @gapIndents, [ $childLine, $childColumn - 1 ];

    for my $childIX ( 0 .. ( $#$children - 1 ) ) {
        my $child  = $children->[$childIX];
        my $symbol = $child->{symbol};
        if ( defined $symbol
            and $symbolReverseDB->{$symbol}->{gap} )
        {
            my $nextChild = $children->[ $childIX + 1 ];
            my $nextStart = $nextChild->{start};
            my ( $nextLine, $nextColumn ) = $recce->line_column($nextStart);
            push @gapIndents, [ $nextLine, $nextColumn - 1 ];
        }
    }
    return \@gapIndents;
}

sub is_0Jogging {
    my ( $policy, $runeLine, $runeColumn, $gapIndents ) = @_;
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};
    my @mistakes  = ();
    die "Jogging-0-style rule with only $gapIndents gap indents"
      if $#$gapIndents < 2;

    # Second child must be on rune line, or
    # at ruleColumn+2
    my ( $firstChildLine, $firstChildColumn ) =
      @{ $gapIndents->[1] };

    if (    $firstChildLine != $runeLine
        and $firstChildColumn != $runeColumn + 2 )
    {
        my $msg = sprintf "Jogging-0-style child #%d @%d:%d; %s", 2,
          $firstChildLine,
          $firstChildColumn + 1,
          describeMisindent( $firstChildColumn, $runeColumn + 2 );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $runeLine,
	    parentColumn => $runeColumn,
            line           => $firstChildLine,
            column         => $firstChildColumn,
            child          => 2,
            expectedColumn => $runeColumn + 2,
          };
    }

    my ( $tistisLine, $tistisColumn ) = @{ $gapIndents->[2] };
    if ( $tistisLine == $runeLine ) {
        my $msg = sprintf
          "Jogging-0-style line %d; TISTIS is on rune line %d; should not be",
          $runeLine, $tistisLine;
        push @mistakes,
          {
            desc         => $msg,
	    parentLine => $runeLine,
	    parentColumn => $runeColumn,
            line         => $tistisLine,
            column       => $tistisColumn,
            child        => 3,
            expectedLine => $runeLine,
          };
    }

    my $tistisIsMisaligned = $tistisColumn != $runeColumn;

    if ($tistisIsMisaligned) {
        my $tistisPos = $lineToPos->[$tistisLine] + $tistisColumn;
        my $tistis = $instance->literal( $tistisPos, 2 );
        $tistisIsMisaligned = $tistis ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf "Jogging-0-style; TISTIS @%d:%d; %s",
          $tistisLine, $tistisColumn + 1,
          describeMisindent( $tistisColumn, $runeColumn );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $runeLine,
	    parentColumn => $runeColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            child          => 3,
            expectedColumn => $runeColumn,
          };
    }
    return \@mistakes;
}

sub isLuslusStyle {
    my ( $policy, $node, $indents ) = @_;
    my $instance = $policy->{lint};
    my ( $parentLine, $parentColumn ) =
      $instance->line_column( $node->{start} );
    my @mistakes = ();
    my ( $baseLine, $baseColumn ) = @{ $indents->[0] };

    my $indentCount = scalar @{$indents};
    my $indentIX    = 1;
  INDENT: while ( $indentIX < $indentCount ) {
        my ( $thisLine, $thisColumn ) = @{ $indents->[$indentIX] };
        last INDENT if $thisLine != $baseLine;
        $indentIX++;
    }
  INDENT: while ( $indentIX < $indentCount ) {
        my ( $thisLine, $thisColumn ) = @{ $indents->[$indentIX] };
        if ( $thisColumn != $baseColumn + 2 ) {
            my $msg = sprintf
              "Child #%d @ line %d; backdent is %d; should be %d",
              $indentIX, $thisLine, $thisColumn, $baseColumn + 2;
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $thisLine,
                column         => $thisColumn,
                child          => $indentIX,
                expectedColumn => $baseColumn + 2
              };
        }
        $indentIX++;
    }
    return \@mistakes;
}

# Format line and 0-based column as string
sub describeLC {
    my ( $line, $column ) = @_;
    return '@' . $line . ':' . ( $column + 1 );
}

sub describeMisindent {
    my ( $got, $sought ) = @_;
    if ( $got > $sought ) {
        return "overindented by " . ( $got - $sought );
    }
    if ( $got < $sought ) {
        return "underindented by " . ( $sought - $got );
    }
    return "correctly indented";
}

sub joggingSide {
    my ( $policy, $node, $runeColumn ) = @_;
    my $instance        = $policy->{lint};
    my $symbolReverseDB = $instance->{symbolReverseDB};
    my $children        = $node->{children};
    my %sideCount       = ();
    my $firstSide;
    my %bodyColumnCount = ();
    my $kingsideCount   = 0;
    my $queensideCount  = 0;
  CHILD: for my $childIX ( 0 .. $#$children ) {
        my $jog    = $children->[$childIX];
        my $symbol = $jog->{symbol};
        next CHILD if defined $symbol and $symbolReverseDB->{$symbol}->{gap};
        my $head = $jog->{children}->[0];
        my ( undef, $column1 ) = $instance->line_column( $head->{start} );

        # say " $column1 - $runeColumn >= 4 ";
        if ( $column1 - $runeColumn >= 4 ) {
            $queensideCount++;
            next CHILD;
        }
        $kingsideCount++;
    }
    return $kingsideCount > $queensideCount
      ? 'kingside'
      : 'queenside';
}

sub joggingBodyAlignment {
    my ( $policy, $node, $runeColumn ) = @_;
    my $instance = $policy->{lint};
    my $children = $node->{children};
    my $firstBodyColumn;
    my %firstLine       = ();
    my %bodyColumnCount = ();

    # Traverse first to last to make it easy to record
    # first line of occurrence of each body column
  CHILD:
    for ( my $childIX = $#$children ; $childIX >= 0 ; $childIX-- ) {
        my $jog         = $children->[$childIX];
        my $jogChildren = $jog->{children};
        my $head        = $jogChildren->[1];
        my $gap         = $jogChildren->[1];
        my $body        = $jogChildren->[2];
        my ( $bodyLine, $bodyColumn ) =
          $instance->line_column( $body->{start} );
        my ( $headLine, $headColumn ) =
          $instance->line_column( $head->{start} );
        my $gapLength = $gap->{length};
        $firstBodyColumn = $bodyColumn
          if not defined $firstBodyColumn;
        next CHILD unless $headLine == $bodyLine;
        next CHILD unless $gap > 2;
        $bodyColumnCount{$bodyColumn} = $bodyColumnCount{$bodyColumn}++;
        $firstLine{$bodyColumn}       = $bodyLine;
    }
    my @bodyColumns = keys %bodyColumnCount;

    # If no aligned columns, simply return first
    return $firstBodyColumn if not @bodyColumns;

    my @sortedBodyColumns =
      sort {
             $bodyColumnCount{$a} <=> $bodyColumnCount{$b}
          or $firstLine{$b} <=> $firstLine{$a}
      }
      keys %bodyColumnCount;
    my $topBodyColumn = $sortedBodyColumns[$#sortedBodyColumns];
    return $topBodyColumn;
}

sub censusJoggingHoon {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my ( undef, $runeColumn ) = $instance->line_column( $node->{start} );
    my $children = $node->{children};
  CHILD: for my $childIX ( 0 .. $#$children ) {
        my $child  = $children->[$childIX];
        my $symbol = $instance->symbol($child);
        next CHILD if $symbol ne 'rick5d' and $symbol ne 'ruck5d';
        my $side = $policy->joggingSide( $child, $runeColumn );
        my $bodyAlignment =
          $policy->joggingBodyAlignment( $child, $runeColumn );
        return $side, $bodyAlignment;
    }
    die "No jogging found for ", symbol($node);
}

sub is_1Jogging {
    my ( $policy, $context, $node, $gapIndents ) = @_;
    my $instance  = $policy->{lint};
    my ($parentLine, $parentColumn) = $instance->line_column( $node->{start} );
    my $lineToPos = $instance->{lineToPos};
    my $start     = $node->{start};
    my ( $runeLine,  $runeColumn )    = $instance->line_column($start);
    my ( $chessSide, $jogBodyColumn ) = $policy->censusJoggingHoon($node);
    $context->{chessSide} = $chessSide;

    $context->{jogRuneColumn} = $runeColumn;

    $context->{jogBodyColumn} = $jogBodyColumn
      if defined $jogBodyColumn;
    $instance->internalError("Chess side undefined") unless $chessSide;

    my @mistakes = ();
    die "1-jogging rule with only $gapIndents gap indents"
      if $#$gapIndents < 3;
    my ( $firstChildLine, $firstChildColumn ) =
      @{ $gapIndents->[1] };
    if ( $firstChildLine != $runeLine ) {
		say STDERR join " ", __FILE__, __LINE__;
        my $msg = sprintf
          "1-jogging %s head %s; should be on rune line %d",
          $chessSide,
          describeLC( $firstChildLine, $firstChildColumn ),
          $runeLine;
        push @mistakes,
          {
            desc         => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line         => $firstChildLine,
            column       => $firstChildColumn,
            child        => 1,
            expectedLine => $runeLine,
          };
    }

    my $expectedColumn = $runeColumn + ( $chessSide eq 'kingside' ? 4 : 6 );
    if ( $firstChildColumn != $expectedColumn ) {
        my $msg = sprintf
          "1-jogging %s head %s; %s",
          $chessSide,
          describeLC( $firstChildLine, $firstChildColumn ),
          describeMisindent( $firstChildColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line           => $firstChildLine,
            column         => $firstChildColumn,
            child          => 1,
            expectedColumn => $expectedColumn,
          };
    }

    my ( $tistisLine, $tistisColumn ) = @{ $gapIndents->[3] };
    if ( $tistisLine == $runeLine ) {
        my $msg = sprintf
          "1-jogging TISTIS %s; should not be on rune line",
          $chessSide,
          describeLC( $tistisLine, $tistisColumn );
        push @mistakes,
          {
            desc         => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line         => $tistisLine,
            column       => $tistisColumn,
            child        => 3,
            expectedLine => $runeLine,
          };
    }

    my $tistisIsMisaligned = $tistisColumn != $runeColumn;

    if ($tistisIsMisaligned) {
        my $tistisPos = $lineToPos->[$tistisLine] + $tistisColumn;
        my $tistis = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistis ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf "1-jogging TISTIS %s; %s",
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent( $tistisColumn, $runeColumn );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            child          => 3,
            expectedColumn => $runeColumn,
          };
    }
    return \@mistakes;
}

sub is_2Jogging {
    my ( $policy, $context, $node, $gapIndents ) = @_;
    my $instance  = $policy->{lint};
    my ($parentLine, $parentColumn) = $instance->line_column( $node->{start} );
    my $lineToPos = $instance->{lineToPos};

    my $start = $node->{start};
    my ( $runeLine,  $runeColumn )    = $instance->line_column($start);
    my ( $chessSide, $jogBodyColumn ) = $policy->censusJoggingHoon($node);
    $context->{chessSide} = $chessSide;

    $context->{jogRuneColumn} = $runeColumn;
    $context->{jogBodyColumn} = $jogBodyColumn if $jogBodyColumn;
    $instance->internalError("Chess side undefined") unless $chessSide;

    # say join " ", "=== jog census:", $side, ($flatJogColumn // 'na');
    my @mistakes = ();
    my ( $firstChildLine, $firstChildColumn ) =
      @{ $gapIndents->[1] };
    if ( $firstChildLine != $runeLine ) {
        my $msg = sprintf
"Jogging-2-style child #%d @ line %d; first child is on line %d; should be on rune line",
          1, $runeLine, $firstChildLine;
        push @mistakes,
          {
            desc         => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line         => $firstChildLine,
            column       => $firstChildColumn,
            child        => 1,
            expectedLine => $runeLine,
          };
    }

    my $expectedColumn = $runeColumn + ( $chessSide eq 'kingside' ? 6 : 8 );
    if ( $firstChildColumn != $expectedColumn ) {
        my $msg = sprintf
          "Jogging-2-style %s child #%d @%d:%d; %s",
          $chessSide, 1, $runeLine,
          $firstChildColumn + 1,
          describeMisindent( $firstChildColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line           => $firstChildLine,
            column         => $firstChildColumn,
            child          => 1,
            expectedColumn => $expectedColumn,
          };
    }

    # Second child must be on rune line, or
    # at chess-side-dependent column
    $expectedColumn = $runeColumn + ( $chessSide eq 'kingside' ? 4 : 6 );
    my ( $secondChildLine, $secondChildColumn ) =
      @{ $gapIndents->[2] };

    if (    $secondChildLine != $runeLine
        and $secondChildColumn != $expectedColumn )
    {
        my $msg = sprintf
          "Jogging-2-style %s child #%d @%d:%d; %s",
          $chessSide, 2, $secondChildLine,
          $secondChildColumn + 1,
          describeMisindent( $secondChildColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line           => $secondChildLine,
            column         => $secondChildColumn,
            child          => 2,
            expectedColumn => $expectedColumn,
          };
    }

    my ( $tistisLine, $tistisColumn ) = @{ $gapIndents->[4] };
    if ( $tistisLine == $runeLine ) {
        my $msg = sprintf
          "Jogging-2-style line %d; TISTIS is on rune line %d; should not be",
          $runeLine, $tistisLine;
        push @mistakes,
          {
            desc         => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line         => $tistisLine,
            column       => $tistisColumn,
            child        => 3,
            expectedLine => $runeLine,
          };
    }

    my $tistisIsMisaligned = $tistisColumn != $runeColumn;

    if ($tistisIsMisaligned) {
        my $tistisPos = $lineToPos->[$tistisLine] + $tistisColumn;
        my $tistis = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistis ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf "Jogging-2-style; TISTIS @%d:%d; %s",
          $tistisLine, $tistisColumn + 1,
          describeMisindent( $tistisColumn, $runeColumn );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            child          => 3,
            expectedColumn => $runeColumn,
          };
    }
    return \@mistakes;
}

sub is_Jogging1 {
    my ( $policy, $context, $node, $gapIndents ) = @_;
    my $instance  = $policy->{lint};
    my ($parentLine, $parentColumn) = $instance->line_column( $node->{start} );
    my $lineToPos = $instance->{lineToPos};

    my $start = $node->{start};
    my ( $runeLine,  $runeColumn )    = $instance->line_column($start);
    my ( $chessSide, $jogBodyColumn ) = $policy->censusJoggingHoon($node);
    $context->{chessSide} = $chessSide;

    $context->{jogRuneColumn} = $runeColumn;
    $context->{jogBodyColumn} = $jogBodyColumn if defined $jogBodyColumn;
    $instance->internalError("Chess side undefined") unless $chessSide;

    # say join " ", "=== jog census:", $side, ($flatJogColumn // 'na');
    my @mistakes = ();
    die "Jogging-prefix rule with only $gapIndents gap indents"
      if $#$gapIndents < 3;

    my ( $tistisLine, $tistisColumn ) = @{ $gapIndents->[2] };
    if ( $tistisLine == $runeLine ) {
        my $msg = sprintf
          "Jogging-prefix line %d; TISTIS is on rune line %d; should not be",
          $runeLine, $tistisLine;
        push @mistakes,
          {
            desc         => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line         => $tistisLine,
            column       => $tistisColumn,
            child        => 3,
            expectedLine => $runeLine,
          };
    }

    my $expectedColumn     = $runeColumn + 2;
    my $tistisIsMisaligned = $tistisColumn != $expectedColumn;

    if ($tistisIsMisaligned) {
        my $tistisPos = $lineToPos->[$tistisLine] + $tistisColumn;
        my $tistis = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistis ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf "Jogging-prefix; TISTIS @%d:%d; %s",
          $tistisLine, $tistisColumn + 1,
          describeMisindent( $tistisColumn, $runeColumn );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            child          => 3,
            expectedColumn => $expectedColumn,
          };
    }

    my ( $thirdChildLine, $thirdChildColumn ) =
      @{ $gapIndents->[3] };

    # TODO: No examples of "jogging prefix" queenside in arvo/ corpus
    $expectedColumn = $runeColumn;
    if ( $thirdChildColumn != $expectedColumn ) {
        my $msg = sprintf
          "Jogging-prefix %s child #%d @%d:%d; %s",
          $chessSide, 1, $runeLine,
          $thirdChildColumn + 1,
          describeMisindent( $thirdChildColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line           => $thirdChildLine,
            column         => $thirdChildColumn,
            child          => 1,
            expectedColumn => $expectedColumn,
          };
    }

    return \@mistakes;
}

sub checkKingsideJog {
    my ( $policy, $node, $context ) = @_;
    my $instance = $policy->{lint};
    my $fileName = $instance->{fileName};
    my $grammar  = $instance->{grammar};
    my $ruleID   = $node->{ruleID};
    my ($parentLine, $parentColumn) = $instance->line_column( $node->{start} );
    say STDERR Data::Dumper::Dumper(
        [
            $context->{hoonName},
            $fileName,
            $parentLine, $parentColumn,
            map { $grammar->symbol_display_form($_) }
              $grammar->rule_expand($ruleID)
        ]
    ) unless $parentLine;    # TODO: Delete after development

    my $chessSide = $context->{chessSide};
    say STDERR Data::Dumper::Dumper(
        [
            $context->{hoonName},
            $fileName,
            $parentLine, $parentColumn,
            map { $grammar->symbol_display_form($_) }
              $grammar->rule_expand($ruleID)
        ]
    ) unless $chessSide;    # TODO: Delete after development
    $instance->internalError("Chess side undefined") unless $chessSide;

    my @mistakes = ();

    my $runeColumn = $context->{jogRuneColumn};
    say STDERR Data::Dumper::Dumper(
        [
            $context->{hoonName},
            $fileName,
            $parentLine, $parentColumn,
            map { $grammar->symbol_display_form($_) }
              $grammar->rule_expand($ruleID)
        ]
    ) unless defined $runeColumn;    # TODO: Delete after development
    $instance->internalError("Rune column undefined") unless defined $runeColumn;
    my $jogBodyColumn = $context->{jogBodyColumn};


    # do not pass these attributes on to child nodes
    delete $context->{jogRuneColumn};
    delete $context->{jogBodyColumn};
    delete $context->{chessSide};

    # Replace inherited attribute rune LC with brick LC
    my ( $brickLine, $brickColumn ) = $instance->brickLC($node);

    my $children = $node->{children};
    my $head     = $children->[0];
    my $gap      = $children->[1];
    my $body     = $children->[2];
    my ( $headLine, $headColumn ) =
      $instance->line_column( $head->{start} );
    my ( $bodyLine, $bodyColumn ) =
      $instance->line_column( $body->{start} );
    my $sideDesc = 'kingside';

    my $expectedHeadColumn = $runeColumn + 2;
    if ( $headColumn != $expectedHeadColumn ) {
        my $msg = sprintf 'Jog %s head %s; %s',
          $sideDesc,
          describeLC( $headLine, $headColumn ),
          describeMisindent( $headColumn, $expectedHeadColumn );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line           => $headLine,
            column         => $headColumn,
            child          => 1,
            expectedColumn => $expectedHeadColumn,
            topicLines     => [$brickLine],
          };
    }

    if ( $headLine != $bodyLine ) {

        my $expectedBodyColumn = $runeColumn + 4;
        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg = sprintf 'Jog %s body %s; %s',
              $sideDesc, describeLC( $bodyLine, $bodyColumn ),
              describeMisindent( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                child          => 2,
                expectedColumn => $expectedBodyColumn,
                topicLines     => [$brickLine],
              };
        }
        return \@mistakes;
    }

    # Check for flat kingside misalignments
    my $gapLength = $gap->{length};
    if ( $gapLength != 2 and $bodyColumn != $jogBodyColumn ) {
        my $msg = sprintf 'Jog %s body %s; %s',
          $sideDesc,
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent( $bodyColumn, $jogBodyColumn );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line           => $bodyLine,
            column         => $bodyColumn,
            child          => 2,
            expectedColumn => $jogBodyColumn,
            topicLines     => [$brickLine],
          };
    }
    return \@mistakes;
}

sub checkQueensideJog {
    my ( $policy, $node, $context ) = @_;
    my $instance = $policy->{lint};
    my ($parentLine, $parentColumn) = $instance->line_column( $node->{start} );
    my $ruleID   = $node->{ruleID};
    my $fileName = $instance->{fileName};
    my $grammar  = $instance->{grammar};

    my $chessSide = $context->{chessSide};
    die Data::Dumper::Dumper(
        [
            $fileName,
            ( $instance->line_column( $node->{start} ) ),
            map { $grammar->symbol_display_form($_) }
              $grammar->rule_expand($ruleID)
        ]
    ) unless $chessSide;    # TODO: Delete after development
    $instance->internalError("Chess side undefined") unless $chessSide;

    my @mistakes = ();

    my $runeColumn    = $context->{jogRuneColumn};
    my $jogBodyColumn = $context->{jogBodyColumn};

    # do not pass these attributes on to child nodes
    delete $context->{jogRuneColumn};
    delete $context->{jogBodyColumn};
    delete $context->{chessSide};

    # Replace inherited attribute rune LC with brick LC
    my ( $brickLine, $brickColumn ) = $instance->brickLC($node);

    my $children = $node->{children};
    my $head     = $children->[0];
    my $gap      = $children->[1];
    my $body     = $children->[2];
    my ( $headLine, $headColumn ) =
      $instance->line_column( $head->{start} );
    my ( $bodyLine, $bodyColumn ) =
      $instance->line_column( $body->{start} );
    my $sideDesc = 'queenside';

    my $expectedHeadColumn = $runeColumn + 4;
    if ( $headColumn != $expectedHeadColumn ) {
        my $msg = sprintf 'Jog %s head %s; %s',
          $sideDesc,
          describeLC( $headLine, $headColumn ),
          describeMisindent( $headColumn, $expectedHeadColumn );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line           => $headLine,
            column         => $headColumn,
            child          => 1,
            expectedColumn => $expectedHeadColumn,
            topicLines     => [$brickLine],
          };
    }

    my $expectedBodyColumn = $runeColumn + 2;
    if (    $headLine != $bodyLine
        and $bodyColumn != $expectedBodyColumn )
    {

        my $msg = sprintf 'Jog %s body %s; %s',
          $sideDesc,
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent( $bodyColumn, $expectedBodyColumn );
        push @mistakes,
          {
            desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
            line           => $bodyLine,
            column         => $bodyColumn,
            child          => 2,
            expectedColumn => $expectedBodyColumn,
            topicLines     => [$brickLine],
          };
    }

    # Check for flat queenside misalignments
    if ( $headLine == $bodyLine ) {
        $expectedBodyColumn = $jogBodyColumn;
        my $gapLength = $gap->{length};
        if ( $gapLength != 2 and $bodyColumn != $jogBodyColumn ) {
            my $msg = sprintf 'Jog %s body %s; %s',
              $sideDesc,
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent( $bodyColumn, $jogBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                child          => 2,
                expectedColumn => $jogBodyColumn,
                topicLines     => [$brickLine],
              };
        }
    }
    return \@mistakes;
}

# TODO: Add a check (optional?) for queenside joggings with no
# split jogs.
sub isJog {
    my ( $policy, $node, $context ) = @_;
    my $instance = $policy->{lint};

    my $chessSide = $context->{chessSide};
    return $policy->checkQueensideJog( $node, $context )
      if $chessSide eq 'queenside';
    return $policy->checkKingsideJog( $node, $context );
}

sub isBackdented {
    my ( $policy, $node, $indents, $baseIndent ) = @_;
    my $instance = $policy->{lint};
    my ($parentLine, $parentColumn) = $instance->line_column( $node->{start} );
    my @mistakes = ();

    # say Data::Dumper::Dumper($indents);
    my ( $baseLine, $baseColumn ) = @{ $indents->[0] };
    $baseIndent //= $baseColumn;
    my $currentIndent = $baseIndent + $#$indents * 2;
    my $lastLine      = $baseLine;
  INDENT: for my $ix ( 1 .. $#$indents ) {
        my $indent = $indents->[$ix];
        my ( $thisLine, $thisColumn ) = @{$indent};
        $currentIndent -= 2;

        # say "$currentIndent vs. $thisColumn";
        next INDENT if $thisLine == $lastLine;
        if ( $currentIndent != $thisColumn ) {
            my $msg = sprintf
              "Child #%d @ line %d; backdent is %d; should be %d",
              $ix, $thisLine, $thisColumn, $currentIndent;
            push @mistakes,
              {
                desc           => $msg,
	    parentLine => $parentLine,
	    parentColumn => $parentColumn,
                line           => $thisLine,
                column         => $thisColumn,
                child          => $ix,
                backdentColumn => $currentIndent,
              };
        }
        $lastLine = $thisLine;
    }
    return \@mistakes;
}

sub isFlat {
    my ($indents)   = @_;
    my ($firstLine) = @{ $indents->[0] };
    my ($lastLine)  = @{ $indents->[$#$indents] };
    return $firstLine == $lastLine;
}

sub validate {
  my ($policy, $node, $context) = @_;
  my $instance = $policy->{lint};

  my $parentContext = $policy->validate_node($node, $context);
  return if $node->{type} ne 'node';
  my $children = $node->{children};
  CHILD: for my $childIX ( 0 .. $#$children ) {
          # say STDERR join " ", __FILE__, __LINE__, "child $childIX of ", (scalar @{$children});
        my $child = $children->[$childIX];
        $policy->validate( $child, $parentContext );
    }
}

sub displayMistakes {
    my ( $policy, $mistakes, $hoonDesc ) = @_;
    my $instance = $policy->{lint};
    my $fileName = $instance->{fileName};

    my @pieces = ();
  MISTAKE: for my $mistake ( @{$mistakes} ) {

        my $type = $mistake->{type};
        my $parentLine = $mistake->{parentLine};
        my $parentColumn = $mistake->{parentColumn};
        my $desc              = $mistake->{desc};
        my $mistakeLine       = $mistake->{line};
        $mistake->{reportLine}       = $parentLine;
        $mistake->{reportColumn}       = $parentColumn;
        my $mistakeTopicLines = $mistake->{topicLines};
        my @topicLines        = ($parentLine);
        push @topicLines, @{$mistakeTopicLines} if $mistakeTopicLines;

        $instance->reportItem( $mistake, "$hoonDesc $desc", \@topicLines, $mistakeLine, );
    }
}

sub validate_node {
    my ( $policy, $node, $argContext ) = @_;
		      # say STDERR join " ", __FILE__, __LINE__, "validate(), hoon =", $argContext->{hoonName};

    my $instance  = $policy->{lint};
    my $fileName  = $instance->{fileName};
    my $grammar   = $instance->{grammar};
    my $recce     = $instance->{recce};
    my $mortarLHS = $instance->{mortarLHS};

    my $tallRuneRule      = $instance->{tallRuneRule};
    my $tallJogRule       = $instance->{tallJogRule};
    my $tallNoteRule      = $instance->{tallNoteRule};
    my $tallLuslusRule    = $instance->{tallLuslusRule};
    my $tall_0JoggingRule = $instance->{tall_0JoggingRule};
    my $tall_1JoggingRule = $instance->{tall_1JoggingRule};
    my $tall_2JoggingRule = $instance->{tall_2JoggingRule};
    my $tall_Jogging1Rule = $instance->{tallJogging1_Rule};

    my $ruleDB             = $instance->{ruleDB};
    my $lineToPos          = $instance->{lineToPos};
    my $symbolReverseDB    = $instance->{symbolReverseDB};
    my $censusWhitespace   = $instance->{censusWhitespace};

    my $parentSymbol = $node->{symbol};
    my $parentStart  = $node->{start};
    my $parentLength = $node->{length};
    my $parentRuleID = $node->{ruleID};

    # $Data::Dumper::Maxdepth = 3;
    # say Data::Dumper::Dumper($node);

    my ( $parentLine, $parentColumn ) = $instance->line_column($parentStart);
    my $parentLC = join ':', $parentLine, $parentColumn + 1;

    my @parentIndents = @{ $argContext->{indents} };

    # TODO: Delete "ancestors" in favor of tree traversal
    my @ancestors = @{ $argContext->{ancestors} };
    shift @ancestors if scalar @ancestors >= 5;    # no more than 5
    push @ancestors, { ruleID => $parentRuleID, start => $parentStart };

    my $argLine = $argContext->{line};
    if ( $argLine != $parentLine ) {
        @parentIndents = ($parentColumn);

        # say "line $parentLine: new indents: ", (join " ", @parentIndents);
    }
    elsif ( $parentColumn != $parentIndents[$#parentIndents] ) {
        push @parentIndents, $parentColumn;

        # say "line $parentLine: indents: ", (join " ", @parentIndents);
    }

    my $argBodyIndent     = $argContext->{bodyIndent};
    my $argTallRuneIndent = $argContext->{tallRuneIndent};
    my $parentBodyIndent;
    $parentBodyIndent = $argBodyIndent if $argLine == $parentLine;
    my $parentTallRuneIndent;
    $parentTallRuneIndent = $argTallRuneIndent if $argLine == $parentLine;
    my $parentContext = {
        ancestors => \@ancestors,
        line      => $parentLine,
        indents   => [@parentIndents],
    };
    $parentContext->{bodyIndent} = $parentBodyIndent
      if defined $parentBodyIndent;
    $parentContext->{tallRuneIndent} = $parentTallRuneIndent
      if defined $parentTallRuneIndent;

    # notes align with body indent from ancestor, if there is one;
    # otherwise, with the parent tall rune (if one exists);
    # otherwise with the parent.
    my $noteIndent = ( $parentBodyIndent // $parentTallRuneIndent )
      // $parentColumn;

    my $parentChessSide = $argContext->{chessSide};
    $parentContext->{chessSide} = $parentChessSide
      if defined $parentChessSide;

    my $parentJogRuneColumn = $argContext->{jogRuneColumn};
    $parentContext->{jogRuneColumn} = $parentJogRuneColumn
      if defined $parentJogRuneColumn;

    my $parentJogBodyColumn = $argContext->{jogBodyColumn};
    $parentContext->{jogBodyColumn} = $parentJogBodyColumn
      if defined $parentJogBodyColumn;

    my $parentHoonName = $argContext->{hoonName};
    my $parentHoonLine = $argContext->{hoonLine};
    my $parentHoonColumn = $argContext->{hoonColumn};

    # say STDERR "setting hoonName = $parentHoonName";
    $parentContext->{hoonName} = $parentHoonName;
    $parentContext->{hoonLine} = $parentHoonLine;
    $parentContext->{hoonColumn} = $parentHoonColumn;

    my $children = $node->{children};

    my $nodeType = $node->{type};
    return if $nodeType ne 'node';

    my $ruleID = $node->{ruleID};
    my ( $lhs, @rhs ) = $grammar->rule_expand( $node->{ruleID} );
    my $lhsName = $grammar->symbol_name($lhs);

    if ( not $mortarLHS->{$lhsName} ) {
        $parentHoonName = $lhsName;

        # say STDERR "resetting hoonName = $parentHoonName";
        $parentContext->{hoonName} = $parentHoonName;
        $parentContext->{hoonLine} = $parentLine;
        $parentContext->{hoonColumn} = $parentColumn;
    }

    $parentContext->{bodyIndent} = $parentColumn
      if $instance->{tallBodyRule}->{$lhsName};

    $parentContext->{tallRuneIndent} = $parentColumn
      if $tallRuneRule->{$lhsName};

    if ( $lhsName eq 'optGay4i' ) {
        return $parentContext;
    }

    my $childCount = scalar @{$children};
    if ( $childCount <= 1 ) {
        return $parentContext;
    }

    my $firstChildIndent = $instance->column( $children->[0]->{start} ) - 1;

    my $gapiness = $ruleDB->[$ruleID]->{gapiness} // 0;

    my $reportType = $gapiness < 0 ? 'sequence' : 'indent';

    # TODO: In another policy, warn on tall children of wide nodes
    if ( $gapiness == 0 ) {    # wide node
        return $parentContext;
    }

    # tall node

    if ( $gapiness < 0 ) {     # sequence
        my ( $parentLine, $parentColumn ) = $recce->line_column($parentStart);
        my $parentLC = join ':', $parentLine, $parentColumn;
        $parentColumn--;       # 0-based
        my $previousLine = $parentLine;
      TYPE_INDENT: {

            # Jogging problems are detected by the individual jogs --
            # we do not run diagnostics on the sequence.
            next TYPE_INDENT if $lhsName eq 'rick5d';
            next TYPE_INDENT if $lhsName eq 'ruck5d';

            if ( $lhsName eq 'tall5dSeq' ) {
                my $argAncestors = $argContext->{ancestors};

                # say Data::Dumper::Dumper($argAncestors);
                my $ancestorCount   = scalar @{$argAncestors};
                my $grandParentName = "";
                my $grandParentLC;
                my ( $grandParentLine, $grandParentColumn );
                if ( scalar @{$argAncestors} >= 1 ) {
                    my $grandParent       = $argAncestors->[-1];
                    my $grandParentRuleID = $grandParent->{ruleID};
                    my $grandParentStart  = $grandParent->{start};
                    ( $grandParentLine, $grandParentColumn ) =
                      $recce->line_column($grandParentStart);
                    $grandParentLC = join ':', $grandParentLine,
                      $grandParentColumn;
                    $grandParentColumn--;    # 0-based
                    my ($lhs) = $grammar->rule_expand($grandParentRuleID);
                    $grandParentName = $grammar->symbol_display_form($lhs);
                }
                if ( $grandParentName eq 'tallSemsig' ) {

                    $previousLine = $grandParentLine;
                  CHILD: for my $childIX ( 0 .. $#$children ) {
                        my $isProblem  = 0;
                        my $child      = $children->[$childIX];
                        my $childStart = $child->{start};
                        my $symbol     = $child->{symbol};
                        next CHILD
                          if defined $symbol
                          and $symbolReverseDB->{$symbol}->{gap};
                        my ( $childLine, $childColumn ) =
                          $recce->line_column($childStart);
                        my $childLC = join ':', $childLine, $childColumn;
                        $childColumn--;    # 0-based

                        my $indentDesc = 'RUN';
                      SET_INDENT_DESC: {
                            if (    $childLine != $previousLine
                                and $childColumn != $grandParentColumn + 2 )
                            {
                                $isProblem = 1;
                                $indentDesc = join " ", $grandParentLC,
                                  $childLC;
                            }
                        }
                            $instance->reportItem(
			    { type=>'sequence', reportLine=>$childLine, reportColumn=>$childColumn },
"$lhsName $indentDesc",
                                $parentHoonLine,
                                $childLine
                            ) if $censusWhitespace or $isProblem;
                        $previousLine = $childLine;
                    }

                    last TYPE_INDENT;
                }
            }

          CHILD: for my $childIX ( 0 .. $#$children ) {
                my $isProblem  = 0;
                my $child      = $children->[$childIX];
                my $childStart = $child->{start};
                my $symbol     = $child->{symbol};
                next CHILD
                  if defined $symbol
                  and $symbolReverseDB->{$symbol}->{gap};
                my ( $childLine, $childColumn ) =
                  $recce->line_column($childStart);
                my $childLC = join ':', $childLine, $childColumn;
                $childColumn--;    # 0-based

                my $indentDesc = 'REGULAR';
              SET_INDENT_DESC: {
                    if (    $childLine != $previousLine
                        and $childColumn != $parentColumn )
                    {
                        $isProblem = 1;
                        $indentDesc = join " ", $parentLC, $childLC;
                    }
                }
                $instance->reportItem(
                    (
                        {
                            type         => 'sequence',
                            reportLine   => $childLine,
                            reportColumn => $childColumn
                        },
                        sprintf "%s $indentDesc",
                        $instance->diagName(
                            $node, $parentContext->{hoonName}
                        )
                    ),
                    $parentHoonLine,
                    $childLine,
                ) if $censusWhitespace or $isProblem;
                $previousLine = $childLine;
            }
        }
        return $parentContext;
    }

    # say STDERR __LINE__, " parentIndents: ", (join " ", @parentIndents);
    # if here, gapiness > 0
    {
        my $mistakes = [];
        my $start    = $node->{start};

        my $indentDesc = '???';

        my @gapIndents = @{ $policy->calcGapIndents($node) };

      TYPE_INDENT: {

            if ( $tallJogRule->{$lhsName} ) {
                $mistakes = $policy->isJog( $node, $parentContext );
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'JOG-STYLE';
                last TYPE_INDENT;
            }

            # if ( isFlat( \@gapIndents ) ) {
            # $indentDesc = 'FLAT';
            # last TYPE_INDENT;
            # }

            if ( $tall_0JoggingRule->{$lhsName} ) {
                $mistakes =
                  $policy->is_0Jogging( $parentLine, $parentColumn, \@gapIndents );
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'JOGGING-0-STYLE';
                last TYPE_INDENT;
            }

            if ( $tall_1JoggingRule->{$lhsName} ) {
                $mistakes =
                  $policy->is_1Jogging( $parentContext, $node, \@gapIndents );
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'JOGGING-1-STYLE';
                last TYPE_INDENT;
            }

            if ( $tall_2JoggingRule->{$lhsName} ) {
                $mistakes =
                  $policy->is_2Jogging( $parentContext, $node, \@gapIndents );
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'JOGGING-2-STYLE';
                last TYPE_INDENT;
            }

            if ( $tall_Jogging1Rule->{$lhsName} ) {
                $mistakes =
                  $policy->is_Jogging1( $parentContext, $node, \@gapIndents );
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'JOGGING-1-STYLE';
                last TYPE_INDENT;
            }

            if ( $tallNoteRule->{$lhsName} ) {
                $mistakes = $policy->isBackdented( $node, \@gapIndents, $noteIndent );
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'CAST-STYLE';
                last TYPE_INDENT;
            }

            if ( $tallLuslusRule->{$lhsName} ) {
                $mistakes = $policy->isLuslusStyle( $node, \@gapIndents );
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'LUSLUS-STYLE';
                last TYPE_INDENT;
            }

            # By default, treat as backdented
            $mistakes = $policy->isBackdented( $node, \@gapIndents );
            if ( not @{$mistakes} ) {
                $indentDesc = 'BACKDENTED';
                last TYPE_INDENT;
            }

        }

      PRINT: {
            if ( @{$mistakes} ) {
                $_->{type} = 'indent' for @{$mistakes};
                $policy->displayMistakes( $mistakes,
                    $instance->diagName( $node, $parentContext->{hoonName} ) );
                last PRINT;
            }

            if ($censusWhitespace) {
		my ($reportLine, $reportColumn ) = $instance->line_column($start);
		my $mistake = { type=>'indent', reportLine=>$reportLine, reportColumn=>$reportColumn };
                $instance->reportItem(
                    (
			$mistake,
                        sprintf "%s %s",
                        $instance->diagName( $node, $parentContext->{hoonName} ),
                        $indentDesc
                    ),
                    $parentLine,
                    $parentLine
                );
            }
        }
    }

    return $parentContext;
}

1;
