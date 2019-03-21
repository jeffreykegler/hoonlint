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

# TODO: delete indents in favor of tree traversal

sub new {
    my ( $class, $lintInstance ) = @_;
    my $policy = {};
    $policy->{lint} = $lintInstance;
    Scalar::Util::weaken( $policy->{lint} );
    return bless $policy, $class;
}

# A "gapSeq" is an ordered subset of a node's children.
# It consists of the first child, followed by zero or more
# pairs of nodes, where each pair is a gap and it post-gap
# symbol.  It is assumed that the first child is not a gap,
# and no post-gap child is a gap.  The sequence will always
# be of odd length.
#
# Intuitively, this is usually the subset of the children with
# information useful for parsing.
sub gapSeq {
    my ( $policy, $node ) = @_;
    my $instance        = $policy->{lint};
    my $symbolReverseDB = $instance->{symbolReverseDB};
    my $children        = $node->{children};
    my $child           = $children->[0];
    my @gapSeq      = ($child);

    my $childIX = 1;
    CHILD: while ($childIX < $#$children ) {
        $child  = $children->[$childIX];
        my $symbol = $child->{symbol};
        if ( not defined $symbol
            or not $symbolReverseDB->{$symbol}->{gap} )
        {
	  $childIX++;
	  next CHILD;
	}
	my $nextChild = $children->[ $childIX + 1 ];
	push @gapSeq, $child, $nextChild;
	$childIX += 2;
    }
    return \@gapSeq;
}

# A variant of "gapSeq" which relaxes the assumption that
# the first child is not a gap, and which returns an
# alternating sequence of gap and post-gap.  It assumes
# that a gap does not follow another gap.
sub gapSeq0 {
    my ( $policy, $node ) = @_;
    my $instance        = $policy->{lint};
    my $symbolReverseDB = $instance->{symbolReverseDB};
    my $children        = $node->{children};
    my @gapSeq      = ();

    my $childIX = 0;
    CHILD: while ($childIX < $#$children ) {
        my $child  = $children->[$childIX];
        my $symbol = $child->{symbol};
        if ( not defined $symbol
            or not $symbolReverseDB->{$symbol}->{gap} )
        {
	  $childIX++;
	  next CHILD;
	}
	my $nextChild = $children->[ $childIX + 1 ];
	push @gapSeq, $child, $nextChild;
	$childIX += 2;
    }
    return \@gapSeq;
}

# Checks a gap to see if it is OK as a pseudo-join.
# If so, returns the column at which code may resume.
# Otherwise returns -1;

sub pseudoJoinColumn {
    my ( $policy, $gap ) = @_;
    my $instance   = $policy->{lint};
    my $gapLiteral = $instance->literalNode($gap);
    my $gapStart   = $gap->{start};
    my $gapEnd     = $gap->{start} + $gap->{length};

    my ( $startLine, $startColumn ) = $instance->line_column($gapStart);
    my ( $endLine,   $endColumn )   = $instance->line_column($gapEnd);

    my $commentColumn; 
    # first partial line (must exist)
    my $firstNewline = index $gapLiteral, "\n";
    return if $firstNewline < 0;
    my $firstColon = index $gapLiteral, ':';
    if ( $firstColon >= 0 and $firstColon < $firstNewline ) {
        ( undef, $commentColumn ) =
          $instance->line_column( $gapStart + $firstColon );
    }

    # say STDERR join " ", __FILE__, __LINE__, "commentColumn", $commentColumn;
    return -1 if not $commentColumn;

    # If the last line of the gap does not end in a newline,
    # it **cannot** contain a comment, because the parser would
    # recognize the whole comment as part of the gap.
    # So we only look for properly aligned comments in full
    # (that is, newline-terminated) lines.

    my $lastFullLine =
      ( substr $gapLiteral, -1, 1 ) eq "\n" ? $endLine : $endLine - 1;
    for my $lineNum ( $startLine + 1 .. $lastFullLine ) {
        my $literalLine = $instance->literalLine($lineNum);
        my $commentOffset = index $literalLine, ':';

        # say STDERR "comment offset $commentOffset vs. $commentColumn";
        return -1 if $commentOffset < 0;
        return -1 if $commentOffset != $commentColumn;
    }
    return $commentColumn;
}

# Is this a one-line gap, or its equivalent?
sub isOneLineGap {
    my ( $policy, $gap, $expectedColumn ) = @_;
    my $instance = $policy->{lint};
    my $start  = $gap->{start};
    my $length = $gap->{length};
    return i_isOneLineGap( $policy, $start + 2, $length - 2, $expectedColumn )
      if $instance->runeGapNode($gap);
    return i_isOneLineGap( $policy, $start, $length, $expectedColumn );
}

# Internal version of isOneLineGap()
sub i_isOneLineGap {
    my ( $policy, $start, $length, $expectedColumn ) = @_;
    my @mistakes = ();
    my $instance = $policy->{lint};
    my $end      = $start + $length;
    my ( $startLine, $startColumn ) = $instance->line_column($start);
    my ( $endLine,   $endColumn )   = $instance->line_column($end);
    $expectedColumn //= -1;    # -1 will never match

    # Criss-cross TISTIS lines are a special case
    if (    $startLine == $endLine
        and $instance->literal( $start - 2, 2 ) ne '=='
        and $instance->literal( $start - 2, 2 ) ne '--' )
    {
        return [
            {
                msg => "missing newline "
                  . describeLC( $startLine, $startColumn ),
                line   => $startLine,
                column => $startColumn,
            }
        ];
    }
  LINE: for my $lineNum ( $startLine + 1 .. $endLine - 1 ) {
        my $literalLine = $instance->literalLine($lineNum);
        if ( $literalLine =~ m/^ [ ]* ([+][|]|[:][:]|[:][<]|[:][>]) /x ) {
            my $commentOffset = $LAST_MATCH_START[1];
            if ( $commentOffset != $expectedColumn ) {
                push @mistakes,
                  {
                    msg => "comment "
                      . describeMisindent( $commentOffset, $expectedColumn ),
                    line   => $lineNum,
                    column => $commentOffset,
                  };
            }
            next LINE;
        }

	# TODO: These are hacks to work around the way
	# triple quoting deals with its trailer -- the parser throws
	# it away before even this whitespace-reading version
	# of the parser gets to see it.  Probably, hoonlint
	# should fork the parser and deal with this situation
	# in a less hack-ish way.
	next LINE if $literalLine =~ m/^ *'''$/; 
	next LINE if $literalLine =~ m/^ *"""$/; 

        push @mistakes,
          {
            msg    => "missing comment on line $lineNum",
            line   => $lineNum,
            column => 0,
          };

    }
    return \@mistakes;
}

sub check_0Running {
    my ( $policy, $node )    = @_;
    my $instance  = $policy->{lint};
    my ( $rune,   undef,    $running ) = @{ $policy->gapSeq($node) };

    my ($runeLine)    = $instance->nodeLC($rune);
    my ($runningLine) = $instance->nodeLC($running);
    return checkSplit_0Running( $policy, $node )
      if $runningLine != $runeLine;
    return checkJoined_0Running( $policy, $node );
}

sub checkBoog5d {
    my ( $policy, $node, $runeNode ) = @_;
    my $gapSeq    = $policy->gapSeq($node);
    my $instance  = $policy->{lint};
    my $censusWhitespace = $instance->{censusWhitespace};

    my @mistakes = ();

    # We deal with the running list here, rather than
    # in its own node

    my $anchorNode = $instance->firstBrickOfLine($node);
    my ( $anchorLine, $anchorColumn ) = $instance->nodeLC($anchorNode);
    my ( $startLine, $startColumn ) = $instance->nodeLC($node);

    # The battery is "joined" iff it starts on the same line as the anchor,
    # but at a different column.  "Different column" to catch the case where
    # the anchor rune *is* the battery rune.
    my $joined = ($anchorLine == $startLine and $anchorColumn != $startColumn);
    my $children = $node->{children};
    my $childIX         = 0;
    my $expectedColumn = $joined ? $startColumn : $anchorColumn;
    my $expectedLine = $joined ? $startLine : $anchorLine+1;
  CHILD: while ( $childIX <= $#$children ) {
        my $cell = $children->[$childIX];
        my ( $cellLine, $cellColumn ) = $instance->nodeLC($cell);

        if ( $cellColumn != $expectedColumn or $censusWhitespace ) {
            my $msg = sprintf
              "cell #%d %s; %s",
              ( $childIX / 2 ) + 1,
              describeLC( $cellLine, $cellColumn ),
              describeMisindent( $cellColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $cellLine,
                parentColumn   => $cellColumn,
                line           => $cellLine,
                column         => $cellColumn,
                expectedColumn => $expectedColumn,
                topicLines     => [ $startLine, $expectedLine ],
              };
        }

        $childIX++;
        last CHILD unless $childIX <= $#$children;
        my $cellGap = $children->[$childIX];
        my ( $cellGapLine, $cellGapColumn ) = $instance->nodeLC($cellGap);
        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $cellGap, $expectedColumn ) } )
        {
            for my $gapMistake (@gapMistakes) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf
                  "cell #%d %s; %s",
                  ( $childIX / 2 ) + 1,
                  describeLC( $gapMistakeLine, $gapMistakeColumn ),
                  $gapMistakeMsg;
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $cellGapLine,
                    parentColumn => $cellGapColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
                    topicLines   => [ $startLine, $cellGapLine ],
                  };
            }
        }

        $childIX++;
    }

    return \@mistakes;

}

sub checkWisp5d {
    my ( $policy, $node ) = @_;
    my @mistakes = ();
    my $instance  = $policy->{lint};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC( $node );
    my $gapSeq    = $policy->gapSeq0($node);
    my ($gap, $hephep) = @{$gapSeq};
    if ( my @gapMistakes = @{ $policy->isOneLineGap( $gap, $parentColumn ) } ) {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf 'battery, %s; %s',
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
              };
        }
    }

    my ( $hephepLine, $hephepColumn ) = $instance->nodeLC( $hephep );
    my $expectedColumn = $parentColumn;
    my $hephepIsMisaligned = $hephepColumn != $expectedColumn;

    if ($hephepIsMisaligned) {
	my $lineToPos        = $instance->{lineToPos};
        my $hephepPos = $lineToPos->[$hephepLine] + $expectedColumn;
        my $hephepLiteral = $instance->literal( $hephepPos, 2 );
        $hephepIsMisaligned = $hephepLiteral ne '--'
    }
    if ($hephepIsMisaligned) {
        my $msg = sprintf
          'battery hephep %s; %s',
          describeLC( $hephepLine, $hephepColumn ),
          describeMisindent( $hephepColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $hephepLine,
            column         => $hephepColumn,
            expectedColumn => $expectedColumn,
          };
    }
    return \@mistakes;
}

sub checkBarcab {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    # BARCAB is special, so we need to find the components using low-level
    # techniques.
    # tallBarcab ::= (- BAR CAB GAP -) till5d (- GAP -) wasp5d wisp5d
    my ( undef, undef, $headGap, $head, $batteryGap, undef, $battery ) =
      @{$node->{children}};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my $anchorNode = $node;
    my ( $anchorLine,  $anchorColumn )  = $instance->nodeLC($anchorNode);
    my ( $headLine, $headColumn ) = $instance->nodeLC($head);
    my ( $batteryLine, $batteryColumn ) = $instance->nodeLC($battery);

    my @mistakes = ();

    my $expectedColumn;

  HEAD_ISSUES: {
        if ( $parentLine != $headLine ) {
            my $pseudoJoinColumn = $policy->pseudoJoinColumn($headGap);
            if ( $pseudoJoinColumn <= 0 ) {
                my $msg = sprintf 'Barcab head %s; must be on rune line',
                  describeLC( $headLine, $headColumn );
                push @mistakes,
                  {
                    desc           => $msg,
                    parentLine     => $parentLine,
                    parentColumn   => $parentColumn,
                    line           => $headLine,
                    column         => $headColumn,
                    expectedColumn => $expectedColumn,
                  };
                last HEAD_ISSUES;
            }
            my $expectedHeadColumn = $pseudoJoinColumn;
            if ( $headColumn != $expectedHeadColumn ) {
                my $msg =
                  sprintf
'Pseudo-joined BARCEN head; head/comment mismatch; head is %s',
                  describeLC( $headLine, $headColumn ),
                  describeMisindent( $headColumn, $expectedHeadColumn );
                push @mistakes,
                  {
                    desc           => $msg,
                    parentLine     => $parentLine,
                    parentColumn   => $parentColumn,
                    line           => $headLine,
                    column         => $headColumn,
                    expectedColumn => $expectedHeadColumn,
                  };
            }
            last HEAD_ISSUES;
        }

        # If here, headLine == runeLine
        my $gapLiteral = $instance->literalNode($headGap);
        my $gapLength  = $headGap->{length};
        last HEAD_ISSUES if $gapLength == 2;
        my ( undef, $headGapColumn ) = $instance->nodeLC($headGap);

        # expected length is the length if the spaces at the end
        # of the gap-equivalent were exactly one stop.
        my $expectedLength = $gapLength + ( 2 - length $gapLiteral );
        $expectedColumn = $headGapColumn + $expectedLength;
        my $msg = sprintf 'Barcab head %s; %s',
          describeLC( $headLine, $headColumn ),
          describeMisindent( $headColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $headLine,
            column         => $headColumn,
            expectedColumn => $expectedColumn,
          };

    }

    $expectedColumn = $anchorColumn;
    if ( my @gapMistakes = @{ $policy->isOneLineGap( $batteryGap, $expectedColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf 'Barceb battery %s; %s',
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $batteryLine ],
              };
        }
    }

    if ( $batteryColumn != $expectedColumn ) {
        my $msg = sprintf 'Barcab battery %s; %s',
          describeLC( $batteryLine, $batteryColumn ),
          describeMisindent( $batteryColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $batteryLine,
            column         => $batteryColumn,
            expectedColumn => $expectedColumn,
          };
        return \@mistakes;
    }

    return \@mistakes;
}

sub checkBarcen {
    my ( $policy, $node ) = @_;
    my ( $gap,         $battery )       = @{$policy->gapSeq0($node)};
    my $instance = $policy->{lint};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my $anchorNode = $instance->firstBrickOfLineExc($node, $instance->{barcenAnchorExceptions});
    my ( $anchorLine, $anchorColumn ) = $instance->nodeLC($anchorNode);
    my ( $batteryLine, $batteryColumn ) = $instance->nodeLC($battery);

    my @mistakes = ();

    my $gapLiteral = $instance->literalNode($gap);
    my $expectedColumn;

    if ( $anchorLine == $batteryLine) {
        return [] if length $gapLiteral == 2;
        my $gapLength = $gap->{length};
        my ( undef, $gapColumn ) = $instance->nodeLC($gap);

        # expected length is the length if the spaces at the end
        # of the gap-equivalent were exactly one stop.
        my $expectedLength = $gapLength + ( 2 - length $gapLiteral );

        $expectedColumn = $gapColumn + $expectedLength;

        if ( $expectedColumn != $batteryColumn ) {
            my $msg = sprintf 'joined Barcen battery %s; %s',
              describeLC( $batteryLine, $batteryColumn ),
              describeMisindent( $batteryColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $batteryLine,
                column         => $batteryColumn,
                expectedColumn => $expectedColumn,
              };
        }
        return \@mistakes;
    }

    # If here head line != battery line
    $expectedColumn = $anchorColumn;
    if ( my @gapMistakes = @{ $policy->isOneLineGap( $gap, $expectedColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf 'split Barcen battery %s; %s',
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $batteryLine ],
              };
        }
    }

    if ( $batteryColumn != $expectedColumn ) {
        my $msg = sprintf 'split Barcen battery %s; %s',
          describeLC( $batteryLine, $batteryColumn ),
          describeMisindent( $batteryColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $batteryLine,
            column         => $batteryColumn,
            expectedColumn => $expectedColumn,
          };
        return \@mistakes;
    }

    return \@mistakes;
}

sub checkBarket {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    my ( $headGap, $head, $batteryGap, $battery ) = @{$policy->gapSeq0($node)};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my $anchorNode = $node;
    my ( $anchorLine,  $anchorColumn )  = $instance->nodeLC($anchorNode);
    my ( $headLine, $headColumn ) = $instance->nodeLC($head);
    my ( $batteryLine, $batteryColumn ) = $instance->nodeLC($battery);

    my @mistakes = ();

    my $expectedColumn;

  HEAD_ISSUES: {
        if ( $parentLine != $headLine ) {
            my $pseudoJoinColumn = $policy->pseudoJoinColumn($headGap);
            if ( $pseudoJoinColumn <= 0 ) {
                my $msg = sprintf 'Barket head %s; must be on rune line',
                  describeLC( $headLine, $headColumn );
                push @mistakes,
                  {
                    desc           => $msg,
                    parentLine     => $parentLine,
                    parentColumn   => $parentColumn,
                    line           => $headLine,
                    column         => $headColumn,
                    expectedColumn => $expectedColumn,
                  };
                last HEAD_ISSUES;
            }
            my $expectedHeadColumn = $pseudoJoinColumn;
            if ( $headColumn != $expectedHeadColumn ) {
                my $msg =
                  sprintf
'Pseudo-joined Barket head; head/comment mismatch; head is %s',
                  describeLC( $headLine, $headColumn ),
                  describeMisindent( $headColumn, $expectedHeadColumn );
                push @mistakes,
                  {
                    desc           => $msg,
                    parentLine     => $parentLine,
                    parentColumn   => $parentColumn,
                    line           => $headLine,
                    column         => $headColumn,
                    expectedColumn => $expectedHeadColumn,
                  };
            }
            last HEAD_ISSUES;
        }

        # If here, headLine == runeLine
        my $gapLiteral = $instance->literalNode($headGap);
        my $gapLength  = $headGap->{length};
        last HEAD_ISSUES if $gapLength == 2;
        my ( undef, $headGapColumn ) = $instance->nodeLC($headGap);

        # expected length is the length if the spaces at the end
        # of the gap-equivalent were exactly one stop.
        my $expectedLength = $gapLength + ( 2 - length $gapLiteral );
        $expectedColumn = $headGapColumn + $expectedLength;
        my $msg = sprintf 'Barket head %s; %s',
          describeLC( $headLine, $headColumn ),
          describeMisindent( $headColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $headLine,
            column         => $headColumn,
            expectedColumn => $expectedColumn,
          };

    }

    $expectedColumn = $anchorColumn;
    if ( my @gapMistakes = @{ $policy->isOneLineGap( $batteryGap, $expectedColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf 'Barket battery %s; %s',
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $batteryLine ],
              };
        }
    }

    if ( $batteryColumn != $expectedColumn ) {
        my $msg = sprintf 'Barket battery %s; %s',
          describeLC( $batteryLine, $batteryColumn ),
          describeMisindent( $batteryColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $batteryLine,
            column         => $batteryColumn,
            expectedColumn => $expectedColumn,
          };
        return \@mistakes;
    }

    return \@mistakes;
}

sub checkFashep {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    # FASWUT is very similar to FASHEP.  Combine them?

    # FASHEP is special, so we need to find the components using low-level
    # techniques.
    # optFordFashep ::= (- FAS HEP GAP -) fordHoofSeq (- GAP -)
    my ( undef, undef, $leaderGap, $body, $trailerGap ) =
      @{ $node->{children} };

    # TODO: Should we require that parent column be 0?
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);

    my @mistakes = ();

    my $expectedColumn;

  BODY_ISSUES: {
        if ( $parentLine != $bodyLine ) {
            my $msg = sprintf 'Fashep body %s; must be on rune line',
              describeLC( $bodyLine, $bodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedColumn,
              };
            last BODY_ISSUES;
        }
        my $expectedBodyColumn = $parentColumn + 6;
        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg =
              sprintf 'Fashep body %s is %s',
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedBodyColumn,
              };
        }
        last BODY_ISSUES;

    }

    $expectedColumn = $parentColumn;
    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $trailerGap, $expectedColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf 'Fashep suffix %s; %s',
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
              };
        }
    }

    return \@mistakes;
}

sub checkFaslus {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    # FASWUT is very similar to FASLUS.  Combine them?

    # FASWUT is special, so we need to find the components using low-level
    # techniques.
    # optFordFaslus ::= (- FAS LUS GAP -) fordHoofSeq (- GAP -)
    my ( undef, undef, $leaderGap, $body, $trailerGap ) =
      @{ $node->{children} };

    # TODO: Should we require that parent column be 0?
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);

    my @mistakes = ();

    my $expectedColumn;

  BODY_ISSUES: {
        if ( $parentLine != $bodyLine ) {
            my $msg = sprintf 'Faslus body %s; must be on rune line',
              describeLC( $bodyLine, $bodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedColumn,
              };
            last BODY_ISSUES;
        }
        my $expectedBodyColumn = $parentColumn + 6;
        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg =
              sprintf 'Faslus body %s is %s',
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedBodyColumn,
              };
        }
        last BODY_ISSUES;

    }

    $expectedColumn = $parentColumn;
    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $trailerGap, $expectedColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf 'Faslus suffix %s; %s',
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
              };
        }
    }

    return \@mistakes;
}

sub checkFaswut {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    # FASWUT is special, so we need to find the components using low-level
    # techniques.
    # fordFaswut ::= (- FAS WUT GAP -) DIT4K_SEQ (- GAP -)
    my ( undef, undef, $leaderGap, $body, $trailerGap ) =
      @{ $node->{children} };

    # TODO: Should we require that parent column be 0?
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);

    my @mistakes = ();

    my $expectedColumn;

  BODY_ISSUES: {
        if ( $parentLine != $bodyLine ) {
            my $msg = sprintf 'Faswut body %s; must be on rune line',
              describeLC( $bodyLine, $bodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedColumn,
              };
            last BODY_ISSUES;
        }
        my $expectedBodyColumn = $parentColumn + 6;
        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg =
              sprintf 'FASWUT; body %s is %s',
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedBodyColumn,
              };
        }
        last BODY_ISSUES;

    }

    $expectedColumn = $parentColumn;
    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $trailerGap, $expectedColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf 'FASWUT suffix %s; %s',
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
              };
        }
    }

    return \@mistakes;
}

sub checkSplit_0Running {
    my ( $policy, $node ) = @_;
    my $gapSeq    = $policy->gapSeq($node);
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};
    my $minimumRunsteps = $instance->{minSplit_0RunningSteps} // 0;

    my ( $rune, $runningGap, $running, $tistisGap, $tistis ) =
      @{ $policy->gapSeq($node) };

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $runningLine, $runningColumn ) = $instance->nodeLC($running);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);

    my ( $anchorLine,  $anchorColumn ) = ($runeLine, $runeColumn);
    my $lhsName = $instance->symbol($node);
    if ( $lhsName eq 'tallColsig' ) {
        # say join " ", __FILE__, __LINE__, $runeLine, $runeColumn;
        my $anchor =
          $instance->nearestBrickOfLineInc( $node, { 'tallCenhep' => 1 } );
        ( $anchorLine, $anchorColumn ) = $instance->nodeLC($anchor);
        # say join " ", __FILE__, __LINE__, $anchorLine, $anchorColumn;
    }

    my @mistakes = ();

    # We deal with the running list here, rather than
    # in its own node

    my $runningChildren = $running->{children};
    my $childIX         = 0;
    my $expectedColumn = $anchorColumn + 2;
    my $expectedLine = $runeLine + 1;
    my $lastGap      = $runningGap;

    my $runStepCount = (scalar @{$runningChildren}+1)/2;
    if ( $runStepCount < $minimumRunsteps ) {

        # Untested

        my $msg =
          sprintf
          "joined 0-running %s; too many runsteps; has %d, minimum is %d",
          describeLC( $runningLine, $runningColumn ),
          $runStepCount, $minimumRunsteps;
        push @mistakes,
          {
            desc         => $msg,
            parentLine   => $runeLine,
            parentColumn => $runeColumn,
            line         => $runningLine,
            column       => $runningColumn,
          };
    }

    # Initial runsteps may be on a single line,
    # separated by one stop
  RUN_STEP: while ( $childIX <= $#$runningChildren ) {
        my $runStep = $runningChildren->[$childIX];
        my ( $runStepLine, $runStepColumn ) =
          $instance->line_column( $runStep->{start} );
        if ( $runStepLine != $runeLine ) {
            last RUN_STEP;
        }
        if ( $lastGap->{length} != 2 ) {
            my ( $lastGapLine, $lastGapColumn ) = $instance->nodeLC($lastGap);
            $expectedColumn = $lastGapColumn + 2;
            my $msg            = sprintf
              "split 0-running runstep #%d %s; %s",
              ( $childIX / 2 ) + 1,
              describeLC( $lastGapLine, $lastGapColumn ),
              describeMisindent( $lastGapColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $runeLine,
                parentColumn   => $runeColumn,
                line           => $runStepLine,
                column         => $runStepColumn,
                expectedColumn => $expectedColumn,
              };
        }
        $lastGap = $runningChildren->[ $childIX + 1 ];
        $childIX += 2;
    }

    while ( $childIX <= $#$runningChildren ) {
        my $runStep = $runningChildren->[$childIX];
        my ( $runStepLine, $runStepColumn ) =
          $instance->line_column( $runStep->{start} );
        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $lastGap, $anchorColumn ) } )
        {
            for my $gapMistake (@gapMistakes) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf
                  "split 0-running runstep #%d %s; %s",
                  ( $childIX / 2 ) + 1,
                  describeLC( $gapMistakeLine, $gapMistakeColumn ),
                  $gapMistakeMsg;
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $runStepLine,
                    parentColumn => $runStepColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
                    topicLines   => [ $runStepLine, $runeLine ],
                  };
            }
        }
        if ( $runStepColumn != $expectedColumn ) {
            my $msg = sprintf
              "split 0-running runstep #%d %s; %s",
              ( $childIX / 2 ) + 1,
              describeLC( $runStepLine, $runStepColumn ),
              describeMisindent( $runStepColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $runStepLine,
                parentColumn   => $runStepColumn,
                line           => $runStepLine,
                column         => $runStepColumn,
                expectedColumn => $expectedColumn,
                topicLines     => [ $runeLine, $expectedLine ],
              };
        }
        $lastGap      = $runningChildren->[ $childIX + 1 ];
        $expectedLine = $runStepLine + 1;
        $childIX += 2;
    }

    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $tistisGap, $anchorColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "split 0-running TISTIS %s; $gapMistakeMsg",
              describeLC( $tistisLine, $tistisColumn );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $anchorLine, $tistisLine ],
              };
        }
    }

    $expectedColumn = $anchorColumn;
    my $tistisIsMisaligned = $tistisColumn != $expectedColumn;

    if ($tistisIsMisaligned) {
        my $tistisPos = $lineToPos->[$tistisLine] + $expectedColumn;
        my $tistisLiteral = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistisLiteral ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf "split 0-running TISTIS %s; %s",
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent( $tistisColumn, $anchorColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            child          => 3,
            expectedColumn => $anchorColumn,
          };
    }
    return \@mistakes;
}

sub checkJoined_0Running {
    my ( $policy, $node ) = @_;
    my $gapSeq    = $policy->gapSeq($node);
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};
    my $maximumRunsteps = $instance->{maxJoined_0RunningSteps};

    my ( $rune, $runningGap, $running, $tistisGap, $tistis ) =
      @{ $policy->gapSeq($node) };

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $runningLine, $runningColumn ) = $instance->nodeLC($running);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);

    my @mistakes = ();

    # We deal with the running list here, rather than
    # in its own node

    my $runningChildren = $running->{children};
    my $childIX         = 0;
    my $expectedColumn = $runeColumn + 4;
    my $expectedLine = $runeLine + 1;
    my $lastGap = $runningGap;;

    my $runStepCount = ( scalar @{$runningChildren} + 1 ) / 2;
    if ( defined $maximumRunsteps and $runStepCount > $maximumRunsteps ) {

        # Untested
        my $msg = sprintf
          "joined 0-running %s; too many runsteps; has %d, maximum is %d",
          describeLC( $runningLine, $runningColumn ),
          $runStepCount, $maximumRunsteps;
        push @mistakes,
          {
            desc         => $msg,
            parentLine   => $runeLine,
            parentColumn => $runeColumn,
            line         => $runningLine,
            column       => $runningColumn,
          };
    }

    # Initial runsteps are on the rune line,
    # separated by one stop
  RUN_STEP: while ( $childIX <= $#$runningChildren ) {
        my $runStep = $runningChildren->[$childIX];
        my ( $runStepLine, $runStepColumn ) =
          $instance->line_column( $runStep->{start} );
        if ( $runStepLine != $runeLine ) {
            last RUN_STEP;
        }
        if ( $lastGap->{length} != 2 ) {
            my ( $lastGapLine, $lastGapColumn ) = $instance->nodeLC($lastGap);
            $expectedColumn = $lastGapColumn + 2;
            my $msg            = sprintf
              "joined 0-running runstep #%d %s; %s",
              ( $childIX / 2 ) + 1,
              describeLC( $lastGapLine, $lastGapColumn ),
              describeMisindent( $lastGapColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $runeLine,
                parentColumn   => $runeColumn,
                line           => $runStepLine,
                column         => $runStepColumn,
                expectedColumn => $expectedColumn,
              };
        }
        $lastGap = $runningChildren->[ $childIX + 1 ];
        $childIX += 2;
    }

    while ( $childIX <= $#$runningChildren ) {
        my $runStep = $runningChildren->[$childIX];
        my ( $runStepLine, $runStepColumn ) =
          $instance->line_column( $runStep->{start} );
        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $lastGap, $runeColumn ) } )
        {
            for my $gapMistake (@gapMistakes) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf
                  "joined 0-running runstep #%d %s; %s",
                  ( $childIX / 2 ) + 1,
                  describeLC( $gapMistakeLine, $gapMistakeColumn ),
                  $gapMistakeMsg;
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $runStepLine,
                    parentColumn => $runStepColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
                    topicLines   => [ $runStepLine, $runeLine ],
                  };
            }
        }
        if ( $runStepColumn != $expectedColumn ) {
            my $msg = sprintf
              "joined 0-running runstep #%d %s; %s",
              ( $childIX / 2 ) + 1,
              describeLC( $runStepLine, $runStepColumn ),
              describeMisindent( $runStepColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $runStepLine,
                parentColumn   => $runStepColumn,
                line           => $runStepLine,
                column         => $runStepColumn,
                expectedColumn => $expectedColumn,
                topicLines     => [ $runeLine, $expectedLine ],
              };
        }
        $lastGap      = $runningChildren->[ $childIX + 1 ];
        $expectedLine = $runStepLine + 1;
        $childIX += 2;
    }

    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $tistisGap, $runeColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "joined 0-running TISTIS %s; $gapMistakeMsg",
              describeLC( $tistisLine, $tistisColumn );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $runeLine, $tistisLine ],
              };
        }
    }

    $expectedColumn = $runeColumn;
    my $tistisIsMisaligned = $tistisColumn != $expectedColumn;

    if ($tistisIsMisaligned) {
        my $tistisPos = $lineToPos->[$tistisLine] + $expectedColumn;
        my $tistisLiteral = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistisLiteral ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf "joined 0-running TISTIS %s; %s",
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent( $tistisColumn, $runeColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            child          => 3,
            expectedColumn => $runeColumn,
          };
    }
    return \@mistakes;
}

sub is_1Running {
    my ( $policy, $context, $node ) = @_;
    my $gapSeq   = $policy->gapSeq($node);
    my $instance = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my (
        $rune,       $headGap, $head,
        $runningGap, $running, $tistisGap, $tistis
    ) = @{ $policy->gapSeq($node) };

    my ( $runeLine, $runeColumn ) = $instance->nodeLC( $rune );
    my ( $headLine, $headColumn ) = $instance->nodeLC( $head );
    my ( $runningLine, $runningColumn ) = $instance->nodeLC( $running );
    my ( $tistisLine, $tistisColumn ) = $instance->nodeLC( $tistis );

    my @mistakes = ();
    if ( $headLine != $runeLine ) {
        my $msg = sprintf
          "1-running s head %s; should be on rune line %d",
          describeLC( $headLine, $headColumn ),
          $runeLine;
        push @mistakes,
          {
            desc         => $msg,
            parentLine   => $runeLine,
            parentColumn => $runeColumn,
            line         => $headLine,
            column       => $headColumn,
            expectedLine => $runeLine,
          };
    }

    my $expectedColumn = $runeColumn + 4;
    if ( $headColumn != $expectedColumn ) {
        my $msg = sprintf
          "1-running head %s; %s",
          describeLC( $headLine, $headColumn ),
          describeMisindent( $headColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $headLine,
            column         => $headColumn,
            expectedColumn => $expectedColumn,
          };
    }

    # We deal with the running list here, rather that
    # in its own node

    my $runningChildren = $running->{children};
    my $childIX         = 0;
    $expectedColumn  = $runeColumn + 2;
    my $expectedLine    = $runeLine + 1;
    my $lastGap         = $runningGap;

    # Initial runsteps may be on a single line,
    # separated by one stop
    RUN_STEP: while ( $childIX <= $#$runningChildren ) {
        my $runStep = $runningChildren->[$childIX];
        my ( $runStepLine, $runStepColumn ) =
          $instance->line_column( $runStep->{start} );
	if ($runStepLine != $runeLine) {
	  last RUN_STEP;
	}
        if ( $lastGap->{length} != 2 ) {
            my ( $lastGapLine, $lastGapColumn ) = $instance->nodeLC($lastGap);
            $expectedColumn = $lastGapColumn + 2;
            my $msg            = sprintf
              "1-running runstep #%d %s; %s",
	      ( $childIX / 2 ) + 1,
              describeLC( $lastGapLine, $lastGapColumn ),
              describeMisindent( $lastGapColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $runeLine,
                parentColumn   => $runeColumn,
                line           => $headLine,
                column         => $headColumn,
                expectedColumn => $expectedColumn,
              };
        }
        $lastGap      = $runningChildren->[ $childIX + 1 ];
        $childIX += 2;
    }

    while ( $childIX <= $#$runningChildren ) {
        my $runStep = $runningChildren->[$childIX];
        my ( $runStepLine, $runStepColumn ) =
          $instance->line_column( $runStep->{start} );
        if ( my @gapMistakes = @{ $policy->isOneLineGap( $lastGap, $runeColumn )} )
        {
            for my $gapMistake ( @gapMistakes ) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf
                  "1-running runstep #%d %s; %s",
                  ( $childIX / 2 ) + 1,
                  describeLC( $gapMistakeLine, $gapMistakeColumn ),
                  $gapMistakeMsg;
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $runStepLine,
                    parentColumn => $runStepColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
                    topicLines   => [ $runStepLine, $runeLine ],
                  };
            }
        }
        if ( $runStepColumn != $expectedColumn ) {
            my $msg = sprintf
              "1-running runstep #%d %s; %s",
              ( $childIX / 2 ) + 1,
              describeLC( $runStepLine, $runStepColumn ),
              describeMisindent( $runStepColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $runStepLine,
                parentColumn   => $runStepColumn,
                line           => $runStepLine,
                column         => $runStepColumn,
                expectedColumn => $expectedColumn,
		topicLines     => [$runeLine, $expectedLine],
              };
        }
        $lastGap      = $runningChildren->[ $childIX + 1 ];
        $expectedLine = $runStepLine + 1;
        $childIX += 2;
    }

    if ( my @gapMistakes = @{ $policy->isOneLineGap( $tistisGap, $runeColumn )} )
        {
            for my $gapMistake ( @gapMistakes ) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf
                  "1-running TISTIS %s; $gapMistakeMsg",
                  describeLC( $tistisLine, $tistisColumn );
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $runeLine,
                    parentColumn => $runeColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
                    topicLines   => [ $runeLine, $tistisLine ],
                  };
            }
        }

    $expectedColumn = $runeColumn;
    my $tistisIsMisaligned = $tistisColumn != $expectedColumn;

    if ($tistisIsMisaligned) {
        my $tistisPos = $lineToPos->[$tistisLine] + $expectedColumn;
        my $tistisLiteral = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistisLiteral ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf "1-running TISTIS %s; %s",
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent( $tistisColumn, $runeColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            child          => 3,
            expectedColumn => $runeColumn,
          };
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
    my ( $policy, $node ) = @_;
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

sub censusJogging1_Hoon {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $children = $node->{children};
  CHILD: for my $childIX ( 0 .. $#$children ) {
        my $child  = $children->[$childIX];
        my $symbol = $instance->symbol($child);
        next CHILD if $symbol ne 'rick5d' and $symbol ne 'ruck5d';
        my $bodyAlignment =
          $policy->joggingBodyAlignment( $child );
        return $bodyAlignment;
    }
    die "No jogging found for ", symbol($node);
}

sub censusJoggingHoon {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my ( undef, $baseColumn ) = $instance->nodeLC( $node );
    my $children = $node->{children};
  CHILD: for my $childIX ( 0 .. $#$children ) {
        my $child  = $children->[$childIX];
        my $symbol = $instance->symbol($child);
        next CHILD if $symbol ne 'rick5d' and $symbol ne 'ruck5d';
        my $side = $policy->joggingSide( $child, $baseColumn );
        my $bodyAlignment =
          $policy->joggingBodyAlignment( $child );
        return $side, $bodyAlignment;
    }
    die "No jogging found for ", symbol($node);
}

sub is_1Jogging {
    my ( $policy, $context, $node ) = @_;
    my $instance   = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my (
        $rune,       $headGap, $head,
        $joggingGap, $jogging, $tistisGap, $tistis
    ) = @{ $policy->gapSeq($node) };

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $headLine,    $headColumn )    = $instance->nodeLC($head);
    my ( $joggingLine, $joggingColumn ) = $instance->nodeLC($jogging);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);

    my ( $chessSide, $jogBodyColumn ) = $policy->censusJoggingHoon($node);
    $context->{chessSide} = $chessSide;
    $context->{jogBodyColumn} = $jogBodyColumn
      if defined $jogBodyColumn;

    my @mistakes = ();
    if ( $headLine != $runeLine ) {
        # say STDERR join " ", __FILE__, __LINE__;
        my $msg = sprintf
          "1-jogging %s head %s; should be on rune line %d",
          $chessSide,
          describeLC( $headLine, $headColumn ),
          $runeLine;
        push @mistakes,
          {
            desc         => $msg,
            parentLine   => $runeLine,
            parentColumn => $runeColumn,
            line         => $headLine,
            column       => $headColumn,
            expectedLine => $runeLine,
          };
    }

    my $expectedColumn = $runeColumn + ( $chessSide eq 'kingside' ? 4 : 6 );
    if ( $headColumn != $expectedColumn ) {
        my $msg = sprintf
          "1-jogging %s head %s; %s",
          $chessSide,
          describeLC( $headLine, $headColumn ),
          describeMisindent( $headColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $headLine,
            column         => $headColumn,
            expectedColumn => $expectedColumn,
          };
    }

    if ( my @gapMistakes = @{$policy->isOneLineGap( $joggingGap, $runeColumn )} )
    {
        for my $gapMistake ( @gapMistakes ) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "1-jogging %s jogging %s; %s",
              $chessSide,
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            my ($joggingGapLine) = $instance->nodeLC($joggingGap);
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [$joggingLine],
              };
        }
    }

    if ( my @gapMistakes = @{$policy->isOneLineGap( $tistisGap, $runeColumn )} )
    {
        for my $gapMistake ( @gapMistakes ) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "1-jogging %s TISTIS %s; %s",
              $chessSide,
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [$tistisLine],
              };
        }
    }

    $expectedColumn = $runeColumn;
    my $tistisIsMisaligned = $tistisColumn != $expectedColumn;

    if ($tistisIsMisaligned) {
        my $tistisPos = $lineToPos->[$tistisLine] + $expectedColumn;
        my $tistisLiteral = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistisLiteral ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf "1-jogging TISTIS %s; %s",
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent( $tistisColumn, $runeColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            expectedColumn => $runeColumn,
          };
    }
    return \@mistakes;
}

sub is_2Jogging {
    my ( $policy, $context, $node ) = @_;
    my $instance   = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my (
        $rune,       $headGap, $head,      $subheadGap, $subhead,
        $joggingGap, $jogging, $tistisGap, $tistis
    ) = @{ $policy->gapSeq($node) };

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $headLine,    $headColumn )    = $instance->nodeLC($head);
    my ( $subheadLine,    $subheadColumn )    = $instance->nodeLC($subhead);
    my ( $joggingLine, $joggingColumn ) = $instance->nodeLC($jogging);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);

    my ( $chessSide, $jogBodyColumn ) = $policy->censusJoggingHoon($node);
    $context->{chessSide} = $chessSide;
    $context->{jogBodyColumn} = $jogBodyColumn
      if defined $jogBodyColumn;

    my @mistakes = ();
    if ( $headLine != $runeLine ) {
        # say STDERR join " ", __FILE__, __LINE__;
        my $msg = sprintf
          "2-jogging %s head %s; should be on rune line %d",
          $chessSide,
          describeLC( $headLine, $headColumn ),
          $runeLine;
        push @mistakes,
          {
            desc         => $msg,
            parentLine   => $runeLine,
            parentColumn => $runeColumn,
            line         => $headLine,
            column       => $headColumn,
            expectedLine => $runeLine,
          };
    }

    if ( $headLine == $subheadLine ) {
        my $expectedColumn = $runeColumn + ( $chessSide eq 'kingside' ? 4 : 6 );
        if ( $headColumn != $expectedColumn ) {
            my $msg = sprintf
              "2-jogging %s head %s; %s",
              $chessSide,
              describeLC( $headLine, $headColumn ),
              describeMisindent( $headColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $runeLine,
                parentColumn   => $runeColumn,
                line           => $headLine,
                column         => $headColumn,
                expectedColumn => $expectedColumn,
              };
        }

        if ( $subheadGap->{length} != 2 ) {
            my ( undef, $subheadGapColumn ) = $instance->nodeLC($subheadGap);
            $expectedColumn = $subheadGapColumn + 2;
            my $msg            = sprintf
              "2-jogging %s subhead %s; %s",
              $chessSide,
              describeLC( $subheadLine, $subheadColumn ),
              describeMisindent( $subheadColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $runeLine,
                parentColumn   => $runeColumn,
                line           => $headLine,
                column         => $headColumn,
                expectedColumn => $expectedColumn,
              };
        }
    }

    if ( $headLine != $subheadLine ) {

        my $expectedColumn = $runeColumn + 4;
        if ( $headColumn != $expectedColumn ) {
            my $msg = sprintf
              "2-jogging split head %s; %s",
              describeLC( $headLine, $headColumn ),
              describeMisindent( $headColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $runeLine,
                parentColumn   => $runeColumn,
                line           => $headLine,
                column         => $headColumn,
                expectedColumn => $expectedColumn,
              };
        }

        # If here, we have "split heads", which should follow the "pseudo-jog"
        # format
        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $subheadGap, $runeColumn ) } )
        {
            for my $gapMistake (@gapMistakes) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf
                  "2-jogging %s subhead %s; %s",
                  $chessSide,
                  describeLC( $gapMistakeLine, $gapMistakeColumn ),
                  $gapMistakeMsg;
                my ($subheadGapLine) = $instance->nodeLC($subheadGap);
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $runeLine,
                    parentColumn => $runeColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
                    topicLines   => [$subheadLine],
                  };
            }
        }

        $expectedColumn = $headColumn - 2;
        if ( $subheadColumn != $expectedColumn ) {
            my $msg = sprintf
              "2-jogging %s subhead %s; %s",
              $chessSide,
              describeLC( $subheadLine, $subheadColumn ),
              describeMisindent( $subheadColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $runeLine,
                parentColumn   => $runeColumn,
                line           => $subheadLine,
                column         => $subheadColumn,
                expectedColumn => $expectedColumn,
              };
        }
    }

    if ( my @gapMistakes = @{$policy->isOneLineGap( $joggingGap, $runeColumn )} )
    {
        for my $gapMistake ( @gapMistakes ) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "2-jogging %s jogging %s; %s",
              $chessSide,
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            my ($joggingGapLine) = $instance->nodeLC($joggingGap);
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [$joggingLine],
              };
        }
    }

    if ( my @gapMistakes = @{$policy->isOneLineGap( $tistisGap, $runeColumn )} )
    {
        for my $gapMistake ( @gapMistakes ) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "2-jogging %s TISTIS %s; %s",
              $chessSide,
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [$tistisLine],
              };
        }
    }

    my $expectedColumn = $runeColumn;
    my $tistisIsMisaligned = $tistisColumn != $expectedColumn;

    if ($tistisIsMisaligned) {
        my $tistisPos = $lineToPos->[$tistisLine] + $expectedColumn;
        my $tistisLiteral = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistisLiteral ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf "2-jogging TISTIS %s; %s",
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent( $tistisColumn, $runeColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            expectedColumn => $runeColumn,
          };
    }
    return \@mistakes;
}

sub is_Jogging1 {
    my ( $policy, $context, $node ) = @_;
    my $instance   = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my (
        $rune,
        $joggingGap, $jogging, $tistisGap, $tistis,
        $tailGap, $tail
    ) = @{ $policy->gapSeq($node) };

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $joggingLine, $joggingColumn ) = $instance->nodeLC($jogging);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);
    my ( $tailLine,  $tailColumn )  = $instance->nodeLC($tail);

    my $jogBodyColumn = $policy->censusJogging1_Hoon( $node );

    # say STDERR "Setting jog body column $jogBodyColumn";

    $context->{chessSide} = 'kingside';
    $context->{jogBodyColumn} = $jogBodyColumn
      if defined $jogBodyColumn;

    my @mistakes = ();

    if ( $joggingLine != $runeLine ) {
        # say STDERR join " ", __FILE__, __LINE__;
        my $msg = sprintf
          "1-jogging jogging %s; should be on rune line %d",
          describeLC( $joggingLine, $joggingColumn ),
          $runeLine;
        push @mistakes,
          {
            desc         => $msg,
            parentLine   => $runeLine,
            parentColumn => $runeColumn,
            line         => $joggingLine,
            column       => $joggingColumn,
            expectedLine => $runeLine,
          };
    }

    my $expectedColumn = $runeColumn + 4;
    if ( $joggingColumn != $expectedColumn ) {
        my $msg = sprintf
          "1-jogging jogging %s; %s",
          describeLC( $joggingLine, $joggingColumn ),
          describeMisindent( $joggingColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $joggingLine,
            column         => $joggingColumn,
            expectedColumn => $expectedColumn,
          };
    }

    if ( my @gapMistakes = @{$policy->isOneLineGap( $tistisGap, $runeColumn )} )
    {
        for my $gapMistake ( @gapMistakes ) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "jogging-1 TISTIS %s; %s",
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [$tistisLine],
              };
        }
    }

    $expectedColumn = $runeColumn+2;
    my $tistisIsMisaligned = $tistisColumn != $expectedColumn;

    if ($tistisIsMisaligned) {
        my $tistisPos = $lineToPos->[$tistisLine] + $expectedColumn;
        my $tistisLiteral = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistisLiteral ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf "jogging-1 TISTIS %s; %s",
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent( $tistisColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            expectedColumn => $expectedColumn,
          };
    }

    if ( my @gapMistakes = @{$policy->isOneLineGap( $tailGap, $runeColumn )} )
    {
        for my $gapMistake ( @gapMistakes ) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "jogging-1 tail %s; %s",
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [$tailLine],
              };
        }
    }

    $expectedColumn = $runeColumn;
    if ( $tailColumn != $expectedColumn ) {
        my $msg = sprintf
          "1-jogging tail %s; %s",
          describeLC( $tailLine, $tailColumn ),
          describeMisindent( $tailColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $tailLine,
            column         => $tailColumn,
            expectedColumn => $expectedColumn,
          };
    }

    return \@mistakes;
}

sub checkKingsideJog {
    my ( $policy, $node, $context ) = @_;
    my $instance = $policy->{lint};
    my $tall_Jogging1Rule = $instance->{tallJogging1_Rule};
    my $fileName = $instance->{fileName};
    my $grammar  = $instance->{grammar};
    my $ruleID   = $node->{ruleID};
    my ( $parentLine, $parentColumn ) =
      $instance->line_column( $node->{start} );

    my $chessSide     = $context->{chessSide};
    my $jogBodyColumn = $context->{jogBodyColumn};

    # do not pass these attributes on to child nodes
    delete $context->{jogBodyColumn};
    delete $context->{chessSide};

    my @mistakes = ();

    # Replace inherited attribute rune LC with brick LC
    my $brickNode = $instance->brickNode($node);
    my ( $brickLine, $brickColumn ) = $instance->nodeLC($brickNode);
    my $brickName = $instance->brickName($brickNode);
    my $baseColumn =
      $tall_Jogging1Rule->{$brickName} ? $brickColumn + 4 : $brickColumn + 2;

      # say STDERR "$brickName $baseColumn";

    my $children = $node->{children};
    my $head     = $children->[0];
    my $gap      = $children->[1];
    my $body     = $children->[2];
    my ( $headLine, $headColumn ) =
      $instance->line_column( $head->{start} );
    my ( $bodyLine, $bodyColumn ) =
      $instance->line_column( $body->{start} );
    my $sideDesc = 'kingside';

    my $expectedHeadColumn = $baseColumn;
    if ( $headColumn != $expectedHeadColumn ) {
        my $msg = sprintf 'Jog %s head %s; %s',
          $sideDesc,
          describeLC( $headLine, $headColumn ),
          describeMisindent( $headColumn, $expectedHeadColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $headLine,
            column         => $headColumn,
            expectedColumn => $expectedHeadColumn,
            topicLines     => [$brickLine],
          };
    }

    if ( $headLine == $bodyLine ) {
        my $gapLength = $gap->{length};

	# say STDERR "$gapLength $bodyColumn $jogBodyColumn";

        if ( $gapLength != 2 and $bodyColumn != $jogBodyColumn ) {
            my $msg = sprintf 'Jog %s body %s; %s',
              $sideDesc,
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent( $bodyColumn, $jogBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $jogBodyColumn,
                topicLines     => [$brickLine],
              };
        }
        return \@mistakes;
    }

    # If here head line != body line
    my $pseudoJoinColumn = $policy->pseudoJoinColumn($gap);
    # say STDERR join " ", __FILE__, __LINE__, "pseudo join column", $pseudoJoinColumn;
    if ( $pseudoJoinColumn >= 0 ) {
        my $expectedBodyColumn = $pseudoJoinColumn;
	# say STDERR join " ", __FILE__, __LINE__, "body column", $bodyColumn;
        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg =
              sprintf
              'Pseudo-joined %s Jog %s; body/comment mismatch; body is %s',
              $sideDesc,
              describeLC( $parentLine, $parentColumn ),
              describeMisindent( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                child          => 2,
                expectedColumn => $expectedBodyColumn,
                topicLines     => [$brickLine],
              };
        }
        my $headLength = $head->{length};

        # Treat the jogging body alignment as the "expected one"
        my $expectedColumn = $jogBodyColumn;
        my $raggedColumn   = $headColumn + $headLength + 2;
        if ( $bodyColumn != $raggedColumn and $bodyColumn != $expectedColumn ) {
            my $msg = sprintf 'Pseudo-joined %s Jog %s; body %s',
              $sideDesc, describeLC( $parentLine, $parentColumn ),
              describeMisindent( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedColumn,
                topicLines     => [$brickLine],
              };
        }
	return \@mistakes;
    }

    # If here, this is (or should be) a split jog
    my $expectedBodyColumn = $baseColumn + 2;

    if ( $bodyColumn != $expectedBodyColumn ) {
        my $msg = sprintf 'Jog %s body %s; %s',
          $sideDesc,
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent( $bodyColumn, $expectedBodyColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $bodyLine,
            column         => $bodyColumn,
            expectedColumn => $expectedBodyColumn,
            topicLines     => [$brickLine],
          };
        return \@mistakes;
    }

    if ( my @gapMistakes = @{ $policy->isOneLineGap( $gap, $expectedBodyColumn )} )
    {
        for my $gapMistake ( @gapMistakes ) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf 'Jog %s split body %s; %s',
              $sideDesc, describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $bodyLine, $brickLine ],
              };
        }
    }
    return \@mistakes;
}

sub checkQueensideJog {
    my ( $policy, $node, $context ) = @_;
    my $instance = $policy->{lint};
    my ( $parentLine, $parentColumn ) =
      $instance->line_column( $node->{start} );
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

    my $jogBodyColumn = $context->{jogBodyColumn};

    # do not pass these attributes on to child nodes
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

    my $expectedHeadColumn = $brickColumn + 4;
    if ( $headColumn != $expectedHeadColumn ) {
        my $msg = sprintf 'Jog %s head %s; %s',
          $sideDesc,
          describeLC( $headLine, $headColumn ),
          describeMisindent( $headColumn, $expectedHeadColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $headLine,
            column         => $headColumn,
            child          => 1,
            expectedColumn => $expectedHeadColumn,
            topicLines     => [$brickLine],
          };
    }

    # Check for flat queenside misalignments
    my $expectedBodyColumn = $jogBodyColumn;
    if ( $headLine == $bodyLine ) {
        my $gapLength = $gap->{length};
        if ( $gapLength != 2 and $bodyColumn != $jogBodyColumn ) {
            my $msg = sprintf 'Jog %s body %s; %s',
              $sideDesc,
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent( $bodyColumn, $jogBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                child          => 2,
                expectedColumn => $jogBodyColumn,
                topicLines     => [$brickLine],
              };
        }
        return \@mistakes;
    }

    # If here, this is a split jog
    $expectedBodyColumn = $brickColumn + 2;
    if ( $bodyColumn != $expectedBodyColumn ) {

        my $msg = sprintf 'Jog %s body %s; %s',
          $sideDesc,
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent( $bodyColumn, $expectedBodyColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $bodyLine,
            column         => $bodyColumn,
            child          => 2,
            expectedColumn => $expectedBodyColumn,
            topicLines     => [$brickLine],
          };
        return \@mistakes;
    }

    if ( my @gapMistakes = @{ $policy->isOneLineGap( $gap, $expectedBodyColumn )} )
    {
        for my $gapMistake ( @gapMistakes ) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf 'Jog %s split body %s; %s',
              $sideDesc, describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $bodyLine, $brickLine ],
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

# not yet implemented
sub isNYI {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my ( $parentLine, $parentColumn ) =
      $instance->line_column( $node->{start} );
    my @mistakes = ();

    my $msg = join q{ }, 'NYI', '[' . $instance->symbol($node) . ']',
      $instance->describeNodeRange($node);
    push @mistakes,
      {
        desc         => $msg,
        parentLine   => $parentLine,
        parentColumn => $parentColumn,
        line         => $parentLine,
        column       => $parentColumn,
      };
    return \@mistakes;
}

sub isBackdented {
    my ( $policy, $node ) = @_;
    my @gapSeq       = @{ $policy->gapSeq0($node) };
    my $elementCount = ( scalar @gapSeq ) / 2;
    my $instance     = $policy->{lint};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my @mistakes = ();

  ENFORCE_ELEMENT1_JOINEDNESS: {
        my $firstGap = $gapSeq[0];
        my ($gapLine) = $instance->nodeLC($firstGap);
        last ENFORCE_ELEMENT1_JOINEDNESS if $gapLine == $parentLine;
        my $gapLiteral = $instance->literalNode($firstGap);
        $gapLiteral = substr( $gapLiteral, 2 )
          if $instance->runeGapNode($firstGap);

        # Only enforce if 1st line is spaces --
        # comments, etc., are caught by the logic to follow
        last ENFORCE_ELEMENT1_JOINEDNESS unless $gapLiteral =~ /^[ ]*\n/;
        my $element = $gapSeq[1];
        my ( $elementLine, $elementColumn ) = $instance->nodeLC($element);
        my $msg = sprintf
          '%d-element backdent must be joined %s',
          $elementCount,
          describeLC( $elementLine, $elementColumn );
        push @mistakes,
          {
            desc         => $msg,
            parentLine   => $parentLine,
            parentColumn => $parentColumn,
            line         => $elementLine,
            column       => $elementColumn,
          };
    }

    my $anchorNode = $instance->anchorNode($node);
    my ( $anchorLine, $anchorColumn ) = $instance->nodeLC($anchorNode);
  ELEMENT:
    for (
        my $elementNumber = 1 ;
        $elementNumber <= $elementCount ;
        $elementNumber++
      )
    {

        my $element = $gapSeq[ $elementNumber * 2 - 1 ];
        my ( $elementLine, $elementColumn ) = $instance->nodeLC($element);
        my $gap = $gapSeq[ $elementNumber * 2 - 2 ];
        my $expectedColumn =
          $anchorColumn + ( $elementCount - $elementNumber ) * 2;

        if ( $elementLine == $parentLine ) {
            my $gapLiteral = $instance->literalNode($gap);
	    # say STDERR join " ", __LINE__, '[' . $gapLiteral . ']';
	    # Remove the rune, if present
	    $gapLiteral = substr($gapLiteral, 2) if $instance->runeGapNode($gap);
	    # say STDERR join " ", __LINE__, '[' . $gapLiteral . ']';

	    # OK if final space are exactly one stop
            next ELEMENT if length $gapLiteral == 2;
	    # OK if at proper alignment for backdent
	    next ELEMENT if $expectedColumn == $elementColumn;

            my $gapLength = $gap->{length};
	    my ( undef, $gapColumn ) = $instance->nodeLC($gap);

            # expected length is the length if the spaces at the end
            # of the gap-equivalent were exactly one stop.
            my $expectedLength = $gapLength + ( 2 - length $gapLiteral );

            $expectedColumn = $gapColumn + $expectedLength;
            my $msg = sprintf
              "joined backdent element #%d %s; %s",
              $elementNumber,
              describeLC( $elementLine, $elementColumn ),
              describeMisindent( $elementColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $elementLine,
                column         => $elementColumn,
                expectedColumn => $expectedColumn,
              };
            next ELEMENT;
        }

        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $gap, $anchorColumn ) } )
        {
            for my $gapMistake (@gapMistakes) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf 'backdented element #%d, %s; %s',
                  $elementNumber,
                  describeLC( $gapMistakeLine, $gapMistakeColumn ),
                  $gapMistakeMsg;
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $parentLine,
                    parentColumn => $parentColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
                  };
            }
        }


# say STDERR join " ", __FILE__, __LINE__, "element $elementNumber", '[' . $instance->literalNode($element) . ']';

        if ( $expectedColumn != $elementColumn ) {
            my $msg = sprintf
              'backdented element #%d %s; %s',
              $elementNumber,
              describeLC( $elementLine, $elementColumn ),
              describeMisindent( $elementColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $elementLine,
                column         => $elementColumn,
                expectedColumn => $expectedColumn,
              };
        }
    }
    return \@mistakes;
}

sub checkLuslusStyle {
    my ( $policy, $node ) = @_;
    my ( $symbolGap, $symbol, $bodyGap, $body ) = @{ $policy->gapSeq0($node) };
    my $instance     = $policy->{lint};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $symbolLine, $symbolColumn ) = $instance->nodeLC($symbol);
    my ( $bodyLine, $bodyColumn ) = $instance->nodeLC($body);
    my @mistakes = ();

    if ( $symbolLine != $parentLine ) {
	my $msg = sprintf
	  'cell symbol must be joined %s',
	  describeLC( $symbolLine, $symbolColumn );
	push @mistakes,
	  {
	    desc         => $msg,
	    parentLine   => $parentLine,
	    parentColumn => $parentColumn,
	    line         => $symbolLine,
	    column       => $symbolColumn,
	  };
    }

    my $expectedColumn = $parentColumn + 4;
    if ( $expectedColumn != $symbolColumn ) {
        my $msg = sprintf
          "cell symbol %s; %s",
          describeLC( $symbolLine, $symbolColumn ),
          describeMisindent( $symbolColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $symbolLine,
            column         => $symbolColumn,
            expectedColumn => $expectedColumn,
          };
    }

  CHECK_BODY: {
        if ( $symbolLine == $bodyLine ) {
            my ( undef, $bodyGapColumn ) = $instance->nodeLC($bodyGap);
            $expectedColumn = $bodyGapColumn + 2;
            if ( $expectedColumn != $bodyColumn ) {
                my $msg = sprintf
                  "joined cell body %s; %s",
                  describeLC( $symbolLine, $symbolColumn ),
                  describeMisindent( $bodyColumn, $expectedColumn );
                push @mistakes,
                  {
                    desc           => $msg,
                    parentLine     => $parentLine,
                    parentColumn   => $parentColumn,
                    line           => $bodyLine,
                    column         => $bodyColumn,
                    expectedColumn => $expectedColumn,
                  };
            }
            last CHECK_BODY;
        }

        # If here, body is split
        $expectedColumn = $parentColumn + 2;
        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $bodyGap, $expectedColumn ) } )
        {
            for my $gapMistake (@gapMistakes) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf 'cell body %s; %s',
                  describeLC( $gapMistakeLine, $gapMistakeColumn ),
                  $gapMistakeMsg;
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $parentLine,
                    parentColumn => $parentColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
                  };
            }
        }
        if ( $expectedColumn != $bodyColumn ) {
            my $msg = sprintf
              "split cell body %s; %s",
              describeLC( $symbolLine, $symbolColumn ),
              describeMisindent( $bodyColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedColumn,
              };
        }
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
    return;
}

sub displayMistakes {
    my ( $policy, $mistakes ) = @_;
    my $instance = $policy->{lint};
    my $fileName = $instance->{fileName};

    my @pieces = ();
  MISTAKE: for my $mistake ( @{$mistakes} ) {

        my $parentLine = $mistake->{parentLine};
        my $parentColumn = $mistake->{parentColumn};
        my $desc              = $mistake->{desc};
        my $mistakeLine       = $mistake->{line};
        $mistake->{reportLine}       = $parentLine;
        $mistake->{reportColumn}       = $parentColumn;
        my $mistakeTopicLines = $mistake->{topicLines};
        my @topicLines        = ($parentLine);
        push @topicLines, @{$mistakeTopicLines} if $mistakeTopicLines;

        $instance->reportItem( $mistake, $desc, \@topicLines, $mistakeLine, );
    }
    return;
}

sub validate_node {
    my ( $policy, $node, $argContext ) = @_;

    my $policyShortName = $policy->{shortName};
    my $instance        = $policy->{lint};
    my $fileName        = $instance->{fileName};
    my $grammar         = $instance->{grammar};
    my $recce           = $instance->{recce};

    my $NYI_Rule          = $instance->{NYI_Rule};
    my $backdentedRule    = $instance->{backdentedRule};
    my $tallRuneRule      = $instance->{tallRuneRule};
    my $tallJogRule       = $instance->{tallJogRule};
    my $tallNoteRule      = $instance->{tallNoteRule};
    my $tallLuslusRule    = $instance->{tallLuslusRule};
    my $tall_0RunningRule = $instance->{tall_0RunningRule};
    my $tall_1RunningRule = $instance->{tall_1RunningRule};
    my $tall_1JoggingRule = $instance->{tall_1JoggingRule};
    my $tall_2JoggingRule = $instance->{tall_2JoggingRule};
    my $tall_Jogging1Rule = $instance->{tallJogging1_Rule};

    my $ruleDB           = $instance->{ruleDB};
    my $lineToPos        = $instance->{lineToPos};
    my $symbolReverseDB  = $instance->{symbolReverseDB};
    my $censusWhitespace = $instance->{censusWhitespace};

    my $parentSymbol = $node->{symbol};
    my $parentStart  = $node->{start};
    my $parentLength = $node->{length};
    my $parentRuleID = $node->{ruleID};

    # $Data::Dumper::Maxdepth = 3;
    # say Data::Dumper::Dumper($node);

    my ( $parentLine, $parentColumn ) = $instance->line_column($parentStart);
    my $parentLC = join ':', $parentLine, $parentColumn + 1;

    my $parentContext = {};

    my $parentChessSide = $argContext->{chessSide};
    $parentContext->{chessSide} = $parentChessSide
      if defined $parentChessSide;

    my $parentJogBodyColumn = $argContext->{jogBodyColumn};
    $parentContext->{jogBodyColumn} = $parentJogBodyColumn
      if defined $parentJogBodyColumn;

    my $children = $node->{children};

    my $nodeType = $node->{type};
    return if $nodeType ne 'node';

    my $ruleID = $node->{ruleID};
    my ( $lhs, @rhs ) = $grammar->rule_expand( $node->{ruleID} );
    my $lhsName = $grammar->symbol_name($lhs);

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

    my $mistakes = [];
    my $start = $node->{start};
    my $indentDesc = '???';

  GATHER_MISTAKES: {
        if ( $gapiness < 0 ) {    # sequence
            my $previousLine = $parentLine;
          TYPE_INDENT: {

                # Jogging problems are detected by the individual jogs --
                # we do not run diagnostics on the sequence.
                if ( $lhsName eq 'rick5d' ) {
                    $indentDesc = 'JOGGING';
                    last TYPE_INDENT;
                }
                if ( $lhsName eq 'ruck5d' ) {
                    $indentDesc = 'JOGGING';
                    last TYPE_INDENT;
                }

                my $grandParent = $instance->ancestor( $node, 1 );
                my $grandParentName = $instance->brickName($grandParent);
                if ( $lhsName eq 'tall5dSeq' or $lhsName eq 'till5dSeq' ) {
                    if ( $tall_1RunningRule->{$grandParentName} ) {
                        $indentDesc = '1-RUNNING';
                        last TYPE_INDENT;
                    }
                    if ( $tall_0RunningRule->{$grandParentName} ) {
                        $indentDesc = '0-RUNNING';
                        last TYPE_INDENT;
                    }
                }

                if ( $lhsName eq 'whap5d' ) {
                    my $greatGrandParent =
                      $instance->ancestor( $grandParent, 1 );
                    my $greatGrandParentName =
                      $instance->brickName($greatGrandParent);

                    # TODO: remove after development?
                    die
                      unless $greatGrandParentName eq 'tallBarcab'
                      or $greatGrandParentName eq 'tallBarcen'
                      or $greatGrandParentName eq 'tallBarket';
                    $mistakes =
                      $policy->checkBoog5d( $node, $greatGrandParent );
                    last TYPE_INDENT if @{$mistakes};
                    $indentDesc = 'CELL';
                    last TYPE_INDENT;
                }

                # By default, treat as not yet implemented
                $mistakes = $policy->isNYI($node);
                last TYPE_INDENT if @{$mistakes};

                # should never reach here
                die "NYI";
            }

            last GATHER_MISTAKES;
        }

        # if here, gapiness > 0

      TYPE_INDENT: {

            if ( $lhsName eq "tallBarcab" ) {
                $mistakes = $policy->checkBarcab($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'BARCAB';
                last TYPE_INDENT;
	    }

            if ( $lhsName eq "tallBarcen" ) {
                $mistakes = $policy->checkBarcen($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'BARCEN';
                last TYPE_INDENT;
	    }

            if ( $lhsName eq "tallBarket" ) {
                $mistakes = $policy->checkBarket($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'BARKET';
                last TYPE_INDENT;
	    }

            if ( $lhsName eq "optFordFashep" ) {
                $mistakes = $policy->checkFashep($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'FASHEP';
                last TYPE_INDENT;
	    }

            if ( $lhsName eq "optFordFaslus" ) {
                $mistakes = $policy->checkFaslus($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'FASLUS';
                last TYPE_INDENT;
	    }

            if ( $lhsName eq "fordFaswut" ) {
                $mistakes = $policy->checkFaswut($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'FASWUT';
                last TYPE_INDENT;
	    }

            if ( $lhsName eq "wisp5d" ) {
                $mistakes = $policy->checkWisp5d($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'WISP5D';
                last TYPE_INDENT;
	    }

            if ( $NYI_Rule->{$lhsName} ) {
                $mistakes = $policy->isNYI($node);
                last TYPE_INDENT if @{$mistakes};

                # should never reach here
                die 'NYI failure';
            }

            if ( $tallJogRule->{$lhsName} ) {
                $mistakes = $policy->isJog( $node, $parentContext );
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'JOG-STYLE';
                last TYPE_INDENT;
            }

            if ( $tall_0RunningRule->{$lhsName} ) {
                $mistakes = $policy->check_0Running($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'RUNNING-0-STYLE';
                last TYPE_INDENT;
            }

            if ( $tall_1RunningRule->{$lhsName} ) {
                $mistakes = $policy->is_1Running( $parentContext, $node );
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'RUNNING-1-STYLE';
                last TYPE_INDENT;
            }

            if ( $tall_1JoggingRule->{$lhsName} ) {
                $mistakes = $policy->is_1Jogging( $parentContext, $node );
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = '1-JOGGING-STYLE';
                last TYPE_INDENT;
            }

            if ( $tall_2JoggingRule->{$lhsName} ) {
                $mistakes = $policy->is_2Jogging( $parentContext, $node );
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = '2-JOGGING-STYLE';
                last TYPE_INDENT;
            }

            if ( $tall_Jogging1Rule->{$lhsName} ) {
                $mistakes = $policy->is_Jogging1( $parentContext, $node );
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'JOGGING-1-STYLE';
                last TYPE_INDENT;
            }

            if ( $tallNoteRule->{$lhsName} ) {
                $mistakes = $policy->isBackdented($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'CAST-STYLE';
                last TYPE_INDENT;
            }

            if ( $tallLuslusRule->{$lhsName} ) {
                $mistakes = $policy->checkLuslusStyle($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'LUSLUS-STYLE';
                last TYPE_INDENT;
            }

            if ( $backdentedRule->{$lhsName} ) {
                $mistakes = $policy->isBackdented($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'BACKDENTED';
                last TYPE_INDENT;
            }

            # By default, treat as not yet implemented
            {
                $mistakes = $policy->isNYI($node);
                last TYPE_INDENT if @{$mistakes};

                # should never reach here
                die 'NYI failure';
            }

        }
    }

  PRINT: {
        if ( @{$mistakes} ) {
            $_->{policy} = $policyShortName for @{$mistakes};
            $_->{subpolicy} = $instance->diagName($node) for @{$mistakes};
            $policy->displayMistakes( $mistakes );
            last PRINT;
        }

        if ($censusWhitespace) {
            my ( $reportLine, $reportColumn ) = $instance->line_column($start);
            my $mistake = {
                policy       => $policyShortName,
                subpolicy    => $instance->diagName($node),
                reportLine   => $reportLine,
                reportColumn => $reportColumn
            };
            $instance->reportItem(
		$mistake,
		$indentDesc,
                $parentLine,
                $parentLine
            );
        }
    }

    return $parentContext;
}

1;
