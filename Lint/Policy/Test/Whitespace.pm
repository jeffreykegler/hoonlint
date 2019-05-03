# Hoon whitespace "test" policy

package MarpaX::YAHC::Lint::Policy::Test::Whitespace;

use 5.010;
use strict;
use warnings;
no warnings 'recursion';

# use Carp::Always;
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

# Return the node tag for the subpolicy field.
# Archetypally, this is the 6-character form of
# rune for the node's brick.
sub nodeSubpolicy {
    my ( $policy, $node ) = @_;
    my $instance        = $policy->{lint};
    my $name = $instance->brickName($node);
    if ( my ($tag) = $name =~ /^tall([B-Z][aeoiu][b-z][b-z][aeiou][b-z])$/ ) {
        return lc $tag;
    }
    if ( my ($tag) = $name =~ /^tall([B-Z][aeoiu][b-z][b-z][aeiou][b-z])Mold$/ )
    {
        return lc $tag;
    }
    return lc $name;
}

# return standard anchor "detail" line
sub anchorDetailsBasic {
    my ( $policy, $rune, $anchorColumn ) = @_;
    my $instance        = $policy->{lint};
    my ( $runeLine, $runeColumn ) = $instance->nodeLC($rune);
    my $anchorLiteral = $instance->literalLine($runeLine);
    my $anchorLexeme = substr $anchorLiteral, $anchorColumn;
    $anchorLexeme =~ s/[\s].*\z//xms;
    my $typeVerb = ($anchorColumn == $runeColumn) ? "anchor" : "re-anchor";
    return [qq{$typeVerb column is } . describeLC($runeLine, $anchorColumn) . qq{ "$anchorLexeme"}];
}

sub anchorDetails {
    my ( $policy, $rune, $anchorData ) = @_;
    my @desc = ();
    my $instance        = $policy->{lint};
    my $brick = $anchorData->{brick};

    my ( $runeLine, $runeColumn ) = $instance->nodeLC($rune);
    my ( $brickLine, $brickColumn ) = $instance->nodeLC($brick);
    my $anchorColumn = $anchorData->{column};
    my $offset = $anchorData->{offset};
    my $runeLineLiteral = $instance->literalLine($runeLine);
    $runeLineLiteral =~ s/\n\z//xms;

    $DB::single = 1 if not defined $anchorColumn;
    if ($anchorColumn == $runeColumn) {
      my $brickLiteral = $instance->literalLine($runeLine);
      my $brickLexeme = substr $brickLiteral, $brickColumn;
      $brickLexeme =~ s/[\s].*\z//xms;
      return [ qq{rune/anchor column is } . describeLC($runeLine, $anchorColumn) . qq{ "$brickLexeme"} ];
    }
    push @desc,
      sprintf
're-anchor column (%d) = anchor brick column (%d) + re-anchor offset (%d)',
      $anchorColumn + 1, $brickColumn + 1, $offset;
    my $maxNumWidth = $instance->maxNumWidth();
    my $pointersPrefix = (' ' x $maxNumWidth);
    my $prefixLength   = length $pointersPrefix;
    push @desc, sprintf '%s%s', $pointersPrefix, $runeLineLiteral;
    my $pointerLine = ( ' ' x ( $runeColumn + $prefixLength ) ) . q{^};
    substr( $pointerLine, ( $brickColumn + $prefixLength ),  1 ) = q{^};
    substr( $pointerLine, ( $anchorColumn + $prefixLength ), 1 ) = q{!};
    push @desc, $pointerLine;
    return \@desc;
}

# first brick node in $node's line,
# by inclusion list.
# $node if there is no prior included brick node
sub reanchorInc {
    my ( $policy, $node, $inclusions ) = @_;
    my $instance = $policy->{lint};

    my ($currentLine)  = $instance->nodeLC($node);
    my $thisNode       = $node;
    my $firstBrickNode = $node;
    my @nodes          = ();

    # Accumulate a list of the nodes on the same line as
    # the argument node
  NODE: while ($thisNode) {
        my ($thisLine) = $instance->nodeLC($thisNode);
        last NODE if $thisLine != $currentLine;
        push @nodes, $thisNode;
        $thisNode = $thisNode->{PARENT};
    }
    my $topNodeIX;
    my $brick          = $node;
    my $reanchorOffset = 0;
  SET_DATA: {
      PICK_NODE: for ( my $nodeIX = $#nodes ; $nodeIX >= 0 ; $nodeIX-- ) {
            my $thisNode  = $nodes[$nodeIX];
            my $brickName = $instance->brickName($thisNode);
            if ( defined $brickName and $inclusions->{$brickName} ) {
                $topNodeIX = $nodeIX;
                last PICK_NODE;
            }
        }
        last SET_DATA if not defined $topNodeIX;
        for (
            my $nodeIX = 1 ;    # do not include first node
            $nodeIX <= $topNodeIX ; $nodeIX++
          )
        {
            my $thisNode = $nodes[$nodeIX];
            my $nodeID   = $thisNode->{IX};
            my $thisReanchorOffset =
              $policy->{perNode}->{$nodeID}->{reanchorOffset} // 0;
            $reanchorOffset += $thisReanchorOffset;
        }
        $brick = $nodes[$topNodeIX];
    }
    my ( $brickLine, $brickColumn ) = $instance->nodeLC($brick);
    my $column  = $brickColumn + $reanchorOffset;
    my %results = (
        brick  => $brick,
        offset => $reanchorOffset,
        column => $column,
        line   => $brickLine
    );
    return $column, \%results;
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

        return -1 if $commentOffset < 0;
        return -1 if $commentOffset != $commentColumn;
    }
    return $commentColumn;
}

# Is this a valid join gap?
# Return undef if not.
# Return -1 if flat join.
# Return pseudo-join column if pseudo-join.
sub checkJoinGap {
    my ( $policy, $gap ) = @_;
    my $instance = $policy->{lint};
    my $gapLiteral = $instance->literalNode($gap);
    # say join ' ', __FILE__, __LINE__;
    # say join q{}, '[', $gapLiteral, ']';
    return -1 if $gapLiteral =~ m/^ *$/;
    # say join ' ', __FILE__, __LINE__;
    my $column = $policy->pseudoJoinColumn($gap);
    return $column if defined $column and $column >= 0;
    return;
}


sub deComment {
   my ($policy, $string) = @_;
   $string =~ s/ ([+][|]|[:][:]|[:][<]|[:][>]) .* \z//xms;
   return $string;
}

# Is this a one-line gap, or its equivalent?
sub isOneLineGap {
    my ( $policy, $gap, $options, $expectedColumn, $expectedColumn2 ) = @_;
    my $instance = $policy->{lint};
    my $start  = $gap->{start};
    my $length = $gap->{length};
    return i_isOneLineGap( $policy, $options, $start + 2, $length - 2, $expectedColumn, $expectedColumn2 )
      if $instance->runeGapNode($gap);
    return i_isOneLineGap( $policy, $options, $start, $length, $expectedColumn, $expectedColumn2 );
}

# Internal version of isOneLineGap()
sub i_isOneLineGap {
    my ( $policy, $options, $start, $length, $expectedColumn, $expectedColumn2 ) = @_;
    my $tag = $options->{tag};
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
		subpolicy => 'missing-newline',
                line   => $startLine,
                column => $startColumn,
            }
        ];
    }

    my $stairTread;    # boolean, initially FALSE
    my $commentOffset;

    my $lineNum = $startLine;
  COMMENT1: while ( 1 ) {
	$lineNum++;
	last COMMENT1 if $lineNum >= $endLine;
        my $literalLine = $instance->literalLine($lineNum);

        if ( $literalLine =~ /^ [ ]* $/xms ) {
            push @mistakes,
              {
                msg    => "empty line in comment",
                line   => $lineNum,
                column => 0,
              };
            next COMMENT1;
        }


      CHECK_FOR_STAIRCASE: {
            last CHECK_FOR_STAIRCASE if $stairTread;
            last CHECK_FOR_STAIRCASE
              unless $literalLine =~ m/^ [ ]* ([:][:][:][:]) /x;
            $commentOffset = $LAST_MATCH_START[1];
            last CHECK_FOR_STAIRCASE if $commentOffset != $expectedColumn;
            if ( length $literalLine > $commentOffset + 4 ) {
                my $nextChar = substr $literalLine, $commentOffset + 4, 1;
                last CHECK_FOR_STAIRCASE if $nextChar !~ m/[ \n]/xms;
            }

            # Peek ahead to make sure this really is a staircase
            my $nextLiteralLine = $instance->literalLine( $lineNum + 1 );
            my $wantedPrefix = ( q{ } x ( $commentOffset + 2 ) ) . q{::};
            my $actualPrefix = substr $nextLiteralLine, 0, length $wantedPrefix;
            last CHECK_FOR_STAIRCASE if $wantedPrefix ne $actualPrefix;
            $stairTread = $lineNum;
            $expectedColumn += 2;
            next COMMENT1;
        }

        if ( $literalLine =~ m/^ [ ]* ([+][|]|[:][:]|[:][<]|[:][>]) /x ) {
            $commentOffset = $LAST_MATCH_START[1];
            next COMMENT1 if $commentOffset == $expectedColumn;
            last COMMENT1
              if defined $expectedColumn2
              and $commentOffset >= $expectedColumn2;
        }

        if ( defined $commentOffset and $commentOffset != $expectedColumn ) {
            my $desc = $stairTread ? "staircase comment" : "comment";
            push @mistakes,
              {
                msg => "$desc "
                  . describeMisindent2( $commentOffset, $expectedColumn ),
                line   => $lineNum,
                column => $commentOffset,
              };
            next COMMENT1;
        }

        # TODO: These are hacks to work around the way
        # triple quoting deals with its trailer -- the parser throws
        # it away before even this whitespace-reading version
        # of the parser gets to see it.  Probably, hoonlint
        # should fork the parser and deal with this situation
        # in a less hack-ish way.
        die unless $literalLine =~ m/^ *'''$/ or $literalLine =~ m/^ *"""$/;

    }

    if (    defined $expectedColumn2
        and defined $commentOffset
        and $commentOffset >= $expectedColumn2 )
    {
      COMMENT2: while (1) {
            $lineNum++;
            last COMMENT2 if $lineNum >= $endLine;
            my $literalLine = $instance->literalLine($lineNum);

            if ( $literalLine =~ /^ [ ]* $/xms ) {
                push @mistakes,
                  {
                    msg    => "empty line ($lineNum) in comment",
                    line   => $lineNum,
                    column => 0,
                  };
                next COMMENT2;
            }

            my $commentOffset;
            if ( $literalLine =~ m/^ [ ]* ([+][|]|[:][:]|[:][<]|[:][>]) /x ) {
                $commentOffset = $LAST_MATCH_START[1];
                next COMMENT2 if $commentOffset == $expectedColumn2;
            }

            if ( defined $commentOffset and $commentOffset != $expectedColumn2 )
            {
                my $desc = "comment";
                push @mistakes,
                  {
                    msg => "$desc "
                      . describeMisindent2( $commentOffset, $expectedColumn2 ),
                    line   => $lineNum,
                    column => $commentOffset,
                  };
                next COMMENT2;
            }

            # TODO: These are hacks to work around the way
            # triple quoting deals with its trailer
            die unless $literalLine =~ m/^ *'''$/ or $literalLine =~ m/^ *"""$/;

        }
    }

    return \@mistakes;
}

sub checkOneLineGap {
    my ( $policy, $gap, $options ) = @_;
    my $instance = $policy->{lint};
    my @mistakes    = ();
    my $tag         = $options->{tag} or die "No tag";
    my $interColumn = $options->{interColumn};
    my $preColumn   = $options->{preColumn};
    my $parent      = $options->{parent} // $gap->{PARENT};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($parent);
    my $topicLines = $options->{topicLines} // [];
    my $details = $options->{details};
    my $subpolicy = $options->{subpolicy};

    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $gap, { tag => $tag}, $interColumn, $preColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "%s %s; %s",
              $tag,
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => $topicLines,
		subpolicy => $subpolicy,
		details => $details,
              };
        }
    }
    return \@mistakes;
}

# Replace all TISTIS logic with this
sub checkTistis {
    my ( $policy, $tistis, $options ) = @_;
    my $expectedColumn = $options->{expectedColumn};
    my $tag = $options->{tag};
    my $subpolicyTag = $options->{subpolicyTag};
    my $instance  = $policy->{lint};
    my $parent = $tistis->{PARENT};
    my @mistakes = ();

    my ( $parentLine,      $parentColumn )      = $instance->nodeLC($parent);
    my ( $tistisLine,      $tistisColumn )      = $instance->nodeLC($tistis);
    my $literalLine = $instance->literalLine($tistisLine);
    $literalLine = $policy->deComment($literalLine);
    $literalLine =~ s/\n//g;
    $literalLine =~ s/==//g;
    if ($literalLine =~ m/[^ ]/) {
        my $msg = sprintf q{TISTIS %s should only share line with other TISTIS's},
          describeLC( $tistisLine, $tistisColumn );
        push @mistakes,
          {
            desc           => $msg,
	    subpolicy => $subpolicyTag . ':tistis-alone',
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
	    details => [ [ $tag ] ],
          };
	  return \@mistakes;
    }

    my $tistisIsMisaligned = $tistisColumn != $expectedColumn;

    if ($tistisIsMisaligned) {
	my $lineToPos = $instance->{lineToPos};
        my $tistisPos = $lineToPos->[$tistisLine] + $expectedColumn;
        my $tistisLiteral = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistisLiteral ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf 'TISTIS %s; %s',
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent2( $tistisColumn, $parentColumn );
        push @mistakes,
          {
            desc           => $msg,
	    subpolicy => $subpolicyTag . ':tistis-indent',
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            expectedColumn => $parentColumn,
	    details => [ [ $tag ] ],
          };
    }
    return \@mistakes;
}

# assumes this is a <tallAttributes> node
sub sailAttributeBodyAlignment {
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
        my $attribute = $children->[$childIX];
        my ( undef, $head, $gap,      $body )       = @{ $policy->gapSeq0($attribute) };
        my ( $headLine, $headColumn ) = $instance->nodeLC($head);
        my ( $bodyLine, $bodyColumn ) = $instance->nodeLC($body);
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

sub checkSailAttribute {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    my ( $headGap, $head, $bodyGap, $body ) = @{ $policy->gapSeq0($node) };

    my ( $headLine, $headColumn ) = $instance->nodeLC($head);
    my ( $bodyLine, $bodyColumn ) = $instance->nodeLC($body);

    my $sailApex = $instance->ancestorByLHS( $node, { sailApex5d => 1 } );
    my ( $sailApexLine, $sailApexColumn ) = $instance->nodeLC($sailApex);
    my $attributes = $instance->ancestorByLHS( $node, { tallAttributes => 1 } );
    my $expectedHeadColumn = $sailApexColumn + 4;
    my $expectedBodyColumn = $policy->sailAttributeBodyAlignment($attributes);

    my @mistakes = ();
    my $tag = 'sail atttribute';

    # We deal with the elements list in its own node

    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $headGap, {tag => $tag}, $expectedHeadColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "Sail attribute head %s; $gapMistakeMsg",
              describeLC( $headLine, $headColumn );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $sailApexLine,
                parentColumn => $sailApexColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [$headLine],
              };
        }
    }

    if ( $headColumn != $expectedHeadColumn ) {
        my $msg = sprintf
          "Sail attribute head %s; %s",
          describeLC( $headLine, $headColumn ),
          describeMisindent2( $headColumn, $expectedHeadColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $sailApexLine,
            parentColumn   => $sailApexColumn,
            line           => $headLine,
            column         => $headColumn,
            expectedColumn => $expectedHeadColumn,
            topicLines     => [$headLine],
          };
    }

  CHECK_BODY: {
        if ( $headLine != $bodyLine ) {
            my $msg = sprintf
              "Sail split attribute NYI %s",
              describeLC( $headLine, $headColumn );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $sailApexLine,
                parentColumn => $sailApexColumn,
                line         => $headLine,
                column       => $headColumn,
                topicLines   => [$headLine],
              };
            last CHECK_BODY;
        }

        my $bodyGapLength = $bodyGap->{length};
        if ( $bodyColumn != $expectedBodyColumn and $bodyGapLength != 2 ) {
            my $msg = sprintf
              "Sail attribute body %s; %s",
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $sailApexLine,
                parentColumn   => $sailApexColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedBodyColumn,
                topicLines     => [$bodyLine],
              };
        }
    }

    return \@mistakes;
}

sub checkTailOfElem {
    my ( $policy, $node ) = @_;
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my ( $tistisGap, $tistis ) = @{ $policy->gapSeq0($node) };

    my $tallTopSail = $instance->ancestor($node, 2);
    my ( $tallTopSailLine, $tallTopSailColumn ) = $instance->nodeLC($tallTopSail);
    my ( $parentLine,      $parentColumn )      = $instance->nodeLC($node);
    my ( $tistisLine,      $tistisColumn )      = $instance->nodeLC($tistis);

    # There is always a SEM before <tallTopSail> and this is our
    # anchor column
    my $expectedColumn = $tallTopSailColumn - 1;

    my @mistakes = ();
    my $tag = 'tail of elem';

    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $tistisGap, { tag => $tag}, $expectedColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "TISTIS %s; $gapMistakeMsg",
              describeLC( $tistisLine, $tistisColumn );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $tistisLine ],
              };
        }
    }

    my $tistisIsMisaligned = $tistisColumn != $expectedColumn;

    if ($tistisIsMisaligned) {
        my $tistisPos = $lineToPos->[$tistisLine] + $expectedColumn;
        my $tistisLiteral = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistisLiteral ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf "TISTIS %s; %s",
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent2( $tistisColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            expectedColumn => $expectedColumn,
          };
    }
    return \@mistakes;
}

sub checkTailOfTop {
    my ( $policy, $node ) = @_;
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my ( $tistisGap, $tistis ) = @{ $policy->gapSeq0($node) };

    my $tallTopSail = $instance->ancestor($node, 2);
    my ( $tallTopSailLine, $tallTopSailColumn ) = $instance->nodeLC($tallTopSail);
    my ( $parentLine,      $parentColumn )      = $instance->nodeLC($node);
    my ( $tistisLine,      $tistisColumn )      = $instance->nodeLC($tistis);

    my $expectedColumn = $tallTopSailColumn;

    my @mistakes = ();
    my $tag = 'tail of top';

    if (
        my @gapMistakes = @{
            $policy->isOneLineGap( $tistisGap, { tag => $tag },
                $expectedColumn )
        }
      )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "TISTIS %s; $gapMistakeMsg",
              describeLC( $tistisLine, $tistisColumn );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [$tistisLine],
              };
        }
    }

    my $tistisIsMisaligned = $tistisColumn != $expectedColumn;

    if ($tistisIsMisaligned) {
        my $tistisPos = $lineToPos->[$tistisLine] + $expectedColumn;
        my $tistisLiteral = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistisLiteral ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf "TISTIS %s; %s",
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent2( $tistisColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            expectedColumn => $expectedColumn,
          };
    }
    return \@mistakes;
}


sub checkBont {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    my ( $gap, $body ) = @{ $policy->gapSeq0($node) };

    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);

    # Bont's are an integral part of SIGGAR/SIGGAL which follow a
    # basically standard backdenting scheme, so this is not really
    # "re-anchoring".
    my $anchor =
      $instance->ancestorByLHS( $node, { tallSiggar => 1, tallSiggal => 1 } );
    my ( $anchorLine,   $anchorColumn )   = $instance->nodeLC($anchor);

    my @mistakes = ();
    my $tag = 'bont';

  BODY_ISSUES: {
        if ( $parentLine == $bodyLine ) {
                my $msg =
                  sprintf 'SIGGAR/SIGGAL element 2 %s; element must not be on rune line',
                  describeLC( $bodyLine, $bodyColumn );
                push @mistakes,
                  {
                    desc           => $msg,
                    parentLine     => $parentLine,
                    parentColumn   => $parentColumn,
                    line           => $bodyLine,
                    column         => $bodyColumn,
		    details => [ [ $tag ] ],
                  };
            last BODY_ISSUES;
        }

        # If here parent line != body line
        my $expectedBodyColumn = $anchorColumn + 2;
        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $gap, { tag => $tag }, $expectedBodyColumn ) } )
        {
            for my $gapMistake (@gapMistakes) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf 'SIGGAL/SIGGAR element 2 %s; %s',
                  describeLC( $gapMistakeLine, $gapMistakeColumn ),
                  $gapMistakeMsg;
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $parentLine,
                    parentColumn => $parentColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
		    details => [ [ $tag ] ],
                  };
            }
        }

        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg =
              sprintf 'SIGGAL/SIGGAR element 2 %s; %s',
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedBodyColumn,
		    details => [ [ $tag ] ],
              };
        }
    }

    return \@mistakes;
}

sub checkBonzElement {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    # bonzElement ::= CEN SYM4K (- GAP -) tall5d
    my ( $bodyGap, $body ) = @{ $policy->gapSeq0($node) };

    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);

    my @mistakes = ();
    my $tag = 'bonz element';

    my $expectedColumn;

  BODY_ISSUES: {
        if ( $parentLine != $bodyLine ) {
            my $msg = sprintf 'Bonz element body %s; must be on rune line',
              describeLC( $bodyLine, $bodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedColumn,
		    details => [ [ $tag ] ],
              };
            last BODY_ISSUES;
        }

        # If here, bodyLine == parentLine
        my $gapLiteral = $instance->literalNode($bodyGap);
        my $gapLength  = $bodyGap->{length};
        last BODY_ISSUES if $gapLength == 2;
        my ( undef, $bodyGapColumn ) = $instance->nodeLC($bodyGap);

        # expected length is the length if the spaces at the end
        # of the gap-equivalent were exactly one stop.
        my $expectedLength = $gapLength + ( 2 - length $gapLiteral );
        $expectedColumn = $bodyGapColumn + $expectedLength;
        my $msg = sprintf 'Bonz element body %s; %s',
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent2( $bodyColumn, $expectedColumn );
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

    return \@mistakes;
}

sub checkTopSail {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $grammar  = $instance->{grammar};
    my $ruleID   = $node->{ruleID};

    my ( $bodyGap, $body ) = @{ $policy->gapSeq0($node) };

    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);

    my @mistakes = ();

    my $expectedColumn;

  BODY_ISSUES: {
        if ( $parentLine != $bodyLine ) {
            last BODY_ISSUES if $instance->symbol($body) eq 'CRAM';
            my $msg = join " ",
              (
                sprintf 'Top sail body %s; must be on rune line',
                describeLC( $bodyLine, $bodyColumn )
              ),
              ( map { $grammar->symbol_display_form($_) }
                  $grammar->rule_expand($ruleID) );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $bodyLine,
                column       => $bodyColumn,
              };
            last BODY_ISSUES;
        }

        # If here, bodyLine == parentLine
        my $gapLiteral = $instance->literalNode($bodyGap);
        my $gapLength  = $bodyGap->{length};
        last BODY_ISSUES if $gapLength == 2;
        my ( undef, $bodyGapColumn ) = $instance->nodeLC($bodyGap);

        # expected length is the length if the spaces at the end
        # of the gap-equivalent were exactly one stop.
        my $expectedLength = $gapLength + ( 2 - length $gapLiteral );
        $expectedColumn = $bodyGapColumn + $expectedLength;
        my $msg = sprintf 'Top Sail body %s; %s',
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent2( $bodyColumn, $expectedColumn );
	  ;
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

    return \@mistakes;
}

sub checkTopKids {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $grammar  = $instance->{grammar};
    my $ruleID   = $node->{ruleID};

    my ( $bodyGap, $body ) = @{ $policy->gapSeq0($node) };

    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);

    my @mistakes = ();

    my $expectedColumn;

  BODY_ISSUES: {
        if ( $parentLine != $bodyLine ) {
            last BODY_ISSUES if $instance->symbol($body) eq 'CRAM';
            my $msg = join " ",
              (
                sprintf 'Sail kids body %s; must be on rune line',
                describeLC( $bodyLine, $bodyColumn )
              ),
              ( map { $grammar->symbol_display_form($_) }
                  $grammar->rule_expand($ruleID) );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $bodyLine,
                column       => $bodyColumn,
              };
            last BODY_ISSUES;
        }

        # If here, bodyLine == parentLine
        my $gapLiteral = $instance->literalNode($bodyGap);
        my $gapLength  = $bodyGap->{length};
        last BODY_ISSUES if $gapLength == 2;
        my ( undef, $bodyGapColumn ) = $instance->nodeLC($bodyGap);

        # expected length is the length if the spaces at the end
        # of the gap-equivalent were exactly one stop.
        my $expectedLength = $gapLength + ( 2 - length $gapLiteral );
        $expectedColumn = $bodyGapColumn + $expectedLength;
        my $msg = sprintf 'Sail kids body %s; %s',
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent2( $bodyColumn, $expectedColumn );
	  ;
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

    return \@mistakes;
}

# Common logic for checking the running element of a hoon.
# returns a (possibly empty) list of mistakes.
#
# TODO: Some of these arguments can (should?) be computed from others.
#
sub checkRunning {
    my ($policy, $options ) = @_;
    my $instance  = $policy->{lint};
    my $runningChildren = $options->{children};
    my $tag = $options->{tag} or die "No tag";
    my $anchorColumn = $options->{anchorColumn};
    my $expectedColumn = $options->{expectedColumn};

    # by default, in fact always at this point, the running can be
    # found as the parent of the last running child, and the parent
    # can be found as the parent
    my $running = $runningChildren->[-1]->{PARENT};
    my $parent = $running->{PARENT};


    my ( $runeLine, $runeColumn ) = $instance->nodeLC($parent);
    my ( $runningLine, $runningColumn ) = $instance->nodeLC($running);

    my $anchorDetails = $options->{anchorDetails}
      // $policy->anchorDetailsBasic( $parent, $anchorColumn );
    # say STDERR Data::Dumper::Dumper($options->{anchorDetails});
    # say STDERR Data::Dumper::Dumper($anchorDetails);

    my $childIX         = 0;
    my $firstSingletonLine;
    my $mistakeSubpolicy = $policy->nodeSubpolicy($parent);
    my @mistakes = ();

    # Do the first run step
    my $runStepCount         = 1;
    my $gap     = $runningChildren->[$childIX];
    my ( $gapLine, $gapColumn ) = $instance->nodeLC($gap);
    my $runStep = $runningChildren->[ $childIX + 1 ];
    my ( $thisRunStepLine, $runStepColumn ) = $instance->nodeLC($runStep);

  CHECK_FIRST_RUNNING: {
        last CHECK_FIRST_RUNNING if $options->{skipFirst};
        last CHECK_FIRST_RUNNING if $runStepColumn == $expectedColumn;
        my $msg = sprintf
          "runstep #%d %s; %s",
          ( $childIX / 2 ) + 1,
          describeLC( $thisRunStepLine, $runStepColumn ),
          describeMisindent2( $runStepColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $thisRunStepLine,
            parentColumn   => $runStepColumn,
            line           => $thisRunStepLine,
            column         => $runStepColumn,
            expectedColumn => $expectedColumn,
            topicLines     => [$runeLine],
	    details => [ [ $tag, @{$anchorDetails} ] ],
          };
    }

    my $workingRunStepLine = $thisRunStepLine;

    # Initial runsteps may be on a single line,
    # separated by one stop
    $childIX = 2;
  RUN_STEP: while ( $childIX < $#$runningChildren ) {

      INLINE_RUN_STEP: while ( 1 ) {
	    if ($childIX >= $#$runningChildren) {
	        last RUN_STEP;
	    }
            $gap     = $runningChildren->[$childIX];
	    ( $gapLine, $gapColumn ) = $instance->nodeLC($gap);
            $runStep = $runningChildren->[ $childIX + 1 ];
            ( $thisRunStepLine, $runStepColumn ) = $instance->nodeLC($runStep);
            if ( $thisRunStepLine != $workingRunStepLine ) {
                last INLINE_RUN_STEP;
            }
            $runStepCount++;
            if ( $gap->{length} != 2 ) {
                my $nextExpectedColumn = $gapColumn + 2;
                my $msg = sprintf
                  'runstep #%d %s; %s',
                  ( $childIX / 2 ) + 1,
                  describeLC( $gapLine, $gapColumn ),
                  describeMisindent2( $runStepColumn, $nextExpectedColumn );
                push @mistakes,
                  {
                    desc           => $msg,
                    parentLine     => $runeLine,
                    parentColumn   => $runeColumn,
                    line           => $thisRunStepLine,
                    column         => $runStepColumn,
                    expectedColumn => $nextExpectedColumn,
		    subpolicy => $mistakeSubpolicy . ':running-hgap',
		    details => [ [ $tag ] ],
                  };
            }
	    $childIX += 2;
        }

	$firstSingletonLine = $workingRunStepLine if $runStepCount <= 1 and not defined $firstSingletonLine;

        if ($runStepCount > 1 and defined $firstSingletonLine ) {
                my $msg = sprintf
                  'runstep %s; multi-step line not allowed after singleton (%d)',
                  describeLC( $gapLine, $gapColumn ),
		  $firstSingletonLine;
                push @mistakes,
                  {
                    desc           => $msg,
                    parentLine     => $runeLine,
                    parentColumn   => $runeColumn,
                    line           => $workingRunStepLine,
                    column         => $runStepColumn,
		    topicLines => [ $firstSingletonLine ],
		    subpolicy => $mistakeSubpolicy . ':running-bad-multistep',
		    details => [ [ $tag ] ],
                  };
	}

	$workingRunStepLine = $thisRunStepLine;
	$runStepCount = 1;

        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $gap,
                {
                    interColumn => $anchorColumn,
                    preColumn   => $runStepColumn,
                    tag => ( sprintf 'runstep #%d', int( 1 + $childIX / 2 ) ),
                    parent     => $runStep,
                    topicLines => [$runeLine],
		    subpolicy => $mistakeSubpolicy . ':comment-indent',
                    details    => [
                        [
                            $tag,
                            'inter-comment indent should be ' . ( $anchorColumn + 1 ),
                            'pre-comment indent should be ' . ( $expectedColumn + 1 ),
			    @{$anchorDetails},
                        ]
                    ],
                }
            )
          };

        if ( $runStepColumn != $expectedColumn ) {
            my $msg = sprintf
              "runstep #%d %s; %s",
              ( $childIX / 2 ) + 1,
              describeLC( $thisRunStepLine, $runStepColumn ),
              describeMisindent2( $runStepColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $thisRunStepLine,
                parentColumn   => $runStepColumn,
                line           => $thisRunStepLine,
                column         => $runStepColumn,
                expectedColumn => $expectedColumn,
                topicLines     => [$runeLine],
		details => [ [ $tag, @{$anchorDetails} ] ],
              };
        }

	$childIX += 2;

    }

    # say join " ", __FILE__, __LINE__, 'childIX', $childIX, $#$runningChildren;

    return \@mistakes;

}

sub check_0Running {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my ( $rune, $runningGap, $running ) = @{ $policy->gapSeq($node) };

    my $column = $policy->checkJoinGap($runningGap);
    return checkSplit_0Running( $policy, $node ) if not defined $column;
    return checkJoined_0Running( $policy, $node, $column ) if $column == -1;
    my ( $runeLine, $runeColumn ) = $instance->nodeLC($rune);
    return checkJoined_0Running( $policy, $node, $column ) if $column == $runeColumn + 4;
    return checkSplit_0Running( $policy, $node );
}

# Find the cell body column, based on alignment within
# a parent hoon.
sub cellBodyColumn {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $nodeIX = $node->{IX};
    my $cellBodyColumn = $policy->{perNode}->{$nodeIX}->{cellBodyColumn};
    return $cellBodyColumn if defined $cellBodyColumn;

  FIND_CELL_BODY_COLUMN: {
	my $instance = $policy->{lint};
        my $lhsName = $instance->lhsName($node);
        if ( $lhsName and $lhsName eq 'whap5d' ) {
            $cellBodyColumn = $policy->whapCellBodyAlignment($node);
	    last FIND_CELL_BODY_COLUMN;
        }
        $cellBodyColumn = $policy->cellBodyColumn( $node->{PARENT} );
    }
    $policy->{perNode}->{$nodeIX}->{cellBodyColumn} = $cellBodyColumn;
    return $cellBodyColumn;
}

# assumes this is a <whap5d> node
sub whapCellBodyAlignment {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $children = $node->{children};
    my $firstBodyColumn;
    my %firstLine       = ();
    my %bodyColumnCount = ();

    # Traverse first to last to make it easy to record
    # first line of occurrence of each body column
  CHILD:
    for ( my $childIX = $#$children ; $childIX >= 0 ; $childIX-=2 ) {
        my $boog = $children->[$childIX];
        my $cell = $boog->{children}->[0];
        my ( undef, $head, $gap,      $body )       = @{ $policy->gapSeq0($cell) };
        my ( $headLine, $headColumn ) = $instance->nodeLC($head);
        my ( $bodyLine, $bodyColumn ) = $instance->nodeLC($body);
        my $gapLength = $gap->{length};
        $firstBodyColumn = $bodyColumn
          if not defined $firstBodyColumn;
        next CHILD unless $headLine == $bodyLine;
        next CHILD unless $gapLength > 2;
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

sub checkWhap5d {
    my ( $policy, $node ) = @_;
    my $gapSeq    = $policy->gapSeq($node);
    my $instance  = $policy->{lint};
    my $censusWhitespace = $instance->{censusWhitespace};

    my @mistakes = ();
    my $tag = 'whap';

    my $anchorNode = $instance->firstBrickOfLine($node);
    my ( $anchorLine, $anchorColumn ) = $instance->nodeLC($anchorNode);
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);

    # The battery is "joined" iff it starts on the same line as the anchor,
    # but at a different column.  "Different column" to catch the case where
    # the anchor rune *is* the battery rune.
    my $joined = ($anchorLine == $parentLine and $anchorColumn != $parentColumn);
    my $children = $node->{children};
    my $childIX         = 0;
    my $expectedColumn = $joined ? $parentColumn : $anchorColumn;
    my $expectedLine = $joined ? $parentLine : $anchorLine+1;

  CHILD: while ( $childIX <= $#$children ) {
        my $boog = $children->[$childIX];
        my ( $boogLine, $boogColumn ) = $instance->nodeLC($boog);

        if ( $boogColumn != $expectedColumn or $censusWhitespace ) {
            my $msg = sprintf
              "cell #%d %s; %s",
              ( $childIX / 2 ) + 1,
              describeLC( $boogLine, $boogColumn ),
              describeMisindent2( $boogColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $boogLine,
                parentColumn   => $boogColumn,
                line           => $boogLine,
                column         => $boogColumn,
                expectedColumn => $expectedColumn,
                topicLines     => [ $parentLine, $expectedLine ],
		details => [ [ $tag ] ],
              };
        }

        $childIX++;
        last CHILD unless $childIX <= $#$children;
        my $boogGap = $children->[$childIX];
        my ( $boogGapLine, $boogGapColumn ) = $instance->nodeLC($boogGap);
        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $boogGap, { tag => $tag}, $expectedColumn ) } )
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
                    parentLine   => $boogGapLine,
                    parentColumn => $boogGapColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
                    topicLines   => [ $parentLine, $boogGapLine ],
		    details => [ [ $tag ] ],
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
    my $tag = 'wisp';
    my $instance  = $policy->{lint};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC( $node );

    my $battery =
      $instance->ancestorByLHS( $node, { tallBarcab => 1, tallBarcen => 1, tallBarket => 1 } );
    my ( $batteryLine, $batteryColumn ) = $instance->nodeLC( $battery );
    my $batteryNodeIX = $battery->{IX};
    my $anchorColumn = $policy->{perNode}->{$batteryNodeIX}->{anchorColumn};
    $anchorColumn //= $batteryColumn;

    my $gapSeq    = $policy->gapSeq0($node);
    my ($gap, $hephep) = @{$gapSeq};
    if ( my @gapMistakes = @{ $policy->isOneLineGap( $gap, { tag => $tag}, $parentColumn ) } ) {
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
                topicLines   => [ $batteryLine ],
		details => [ [ $tag ] ],
              };
        }
    }

    my ( $hephepLine, $hephepColumn ) = $instance->nodeLC( $hephep );
    my $expectedColumn = $anchorColumn;
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
          describeMisindent2( $hephepColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $hephepLine,
            column         => $hephepColumn,
            expectedColumn => $expectedColumn,
                topicLines   => [ $batteryLine ],
		details => [ [ $tag ] ],
          };
    }
    return \@mistakes;
}

sub checkSplitFascom {
    my ( $policy, $node ) = @_;
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my ( $bodyGap, $body, $tistisGap, $tistis ) =
      @{ $policy->gapSeq0($node) };

    my ( $runeLine,   $runeColumn )   = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);
    my ( $tistisLine, $tistisColumn ) = $instance->nodeLC($tistis);

    my ( $anchorLine, $anchorColumn ) = ( $runeLine, $runeColumn );

    my @mistakes = ();
    my $tag = 'fascom';

    # We deal with the elements list itself,
    # in its own node

    my $expectedColumn = $anchorColumn + 2;
    my $expectedLine   = $runeLine + 1;

    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $bodyGap, { tag => $tag}, $anchorColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "split Fascom %s; %s",
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $bodyLine ],
		details => [ [ $tag ] ],
              };
        }
    }

    if ( $bodyColumn != $expectedColumn ) {
        my $msg = sprintf
          "split Fascom %s; %s",
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent2( $bodyColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $bodyLine,
            column         => $bodyColumn,
            expectedColumn => $expectedColumn,
            topicLines     => [ $runeLine, $expectedLine ],
	    details => [ [ $tag ] ],
          };
    }

    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $tistisGap, { tag => $tag}, $anchorColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "split Fascom TISTIS %s; $gapMistakeMsg",
              describeLC( $tistisLine, $tistisColumn );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $anchorLine, $tistisLine ],
		details => [ [ $tag ] ],
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
        my $msg = sprintf "split Fascom TISTIS %s; %s",
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent2( $tistisColumn, $anchorColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            expectedColumn => $anchorColumn,
          };
    }
    return \@mistakes;
}

sub checkJoinedFascom {
    my ( $policy, $node ) = @_;
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my ( $bodyGap, $body, $tistisGap, $tistis ) = @{ $policy->gapSeq0($node) };

    my ( $runeLine,   $runeColumn )   = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);
    my ( $tistisLine, $tistisColumn ) = $instance->nodeLC($tistis);

    my @mistakes = ();
    my $tag = 'fascom';

    # We deal with the elements list in its own node

    my $expectedColumn = $runeColumn + 4;
    my $expectedLine   = $runeLine + 1;

    if ( $bodyColumn != $expectedColumn ) {
        my $msg = sprintf
          "joined Fascom %s; %s",
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent2( $bodyColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $bodyLine,
            column         => $bodyColumn,
            expectedColumn => $expectedColumn,
            topicLines     => [ $runeLine, $expectedLine ],
	    details => [ [ $tag ] ],
          };
    }

    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $tistisGap, { tag => $tag }, $runeColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "joined Fascom TISTIS %s; $gapMistakeMsg",
              describeLC( $tistisLine, $tistisColumn );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $runeLine, $tistisLine ],
		details => [ [ $tag ] ],
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
        my $msg = sprintf "joined Fascom TISTIS %s; %s",
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent2( $tistisColumn, $runeColumn );
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

sub checkFascom {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my ( undef, $elements ) = @{ $policy->gapSeq0($node) };

    my ($runeLine)     = $instance->nodeLC($node);
    my ($elementsLine) = $instance->nodeLC($elements);
    return checkSplitFascom( $policy, $node )
      if $elementsLine != $runeLine;
    return checkJoinedFascom( $policy, $node );
}

sub checkFascomElements {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $children = $node->{children};

    my $rune = $instance->ancestorByBrickName( $node, 'fordFascom' );
    my ( $runeLine, $runeColumn ) = $instance->nodeLC($rune);
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);

    my @mistakes = ();
    my $tag = 'fascom elements';

    my $childIX        = 0;
    my $expectedColumn = $parentColumn;
  CHILD: while ( $childIX <= $#$children ) {
        my $element = $children->[$childIX];
        my ( $elementLine, $elementColumn ) = $instance->nodeLC($element);

        if ( $elementColumn != $expectedColumn ) {
            my $msg = sprintf
              "element %d %s; %s",
              ( $childIX / 2 ) + 1,
              describeLC( $elementLine, $elementColumn ),
              describeMisindent2( $elementColumn, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $elementLine,
                column         => $elementColumn,
                expectedColumn => $expectedColumn,
		topicLines => [ $runeLine ],
              };
        }

        $childIX++;
        last CHILD unless $childIX <= $#$children;
        my $elementGap = $children->[$childIX];
        my ( $elementGapLine, $elementGapColumn ) = $instance->nodeLC($elementGap);
        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $elementGap, { tag => $tag }, $runeColumn ) } )
        {
            for my $gapMistake (@gapMistakes) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf
                  "element %d %s; %s",
                  ( $childIX / 2 ) + 1,
                  describeLC( $gapMistakeLine, $gapMistakeColumn ),
                  $gapMistakeMsg;
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $parentLine,
                    parentColumn => $parentColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
		    topicLines => [ $runeLine ],
		    details => [ [ $tag ] ],
                  };
            }
        }

        $childIX++;
    }

    return \@mistakes;
}

# Check "vanilla" sequence
sub checkSeq {
    my ( $policy, $node, $tag ) = @_;
    my $instance = $policy->{lint};
    my $children = $node->{children};

    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);

    my @mistakes = ();

    my $childIX        = 0;
    my $expectedColumn = $parentColumn;
  CHILD: while ( $childIX <= $#$children ) {
        my $element = $children->[$childIX];
        my ( $elementLine, $elementColumn ) = $instance->nodeLC($element);

        if ( $elementColumn != $expectedColumn ) {
            my $msg = sprintf
              '%s %d %s; %s',
	      $tag,
              ( $childIX / 2 ) + 1,
              describeLC( $elementLine, $elementColumn ),
              describeMisindent2( $elementColumn, $expectedColumn );
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

        $childIX++;
        last CHILD unless $childIX <= $#$children;
        my $elementGap = $children->[$childIX];
        my ( $elementGapLine, $elementGapColumn ) = $instance->nodeLC($elementGap);
        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $elementGap, { tag => $tag}, $expectedColumn ) } )
        {
            for my $gapMistake (@gapMistakes) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf
                  '%s %d %s; %s',
		  $tag,
                  ( $childIX / 2 ) + 1,
                  describeLC( $gapMistakeLine, $gapMistakeColumn ),
                  $gapMistakeMsg;
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $parentLine,
                    parentColumn => $parentColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
                    topicLines   => [$elementGapLine],
                  };
            }
        }

        $childIX++;
    }

    return \@mistakes;

}

sub checkBarcab {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    # TODO: reanchoring logic, memoize anchorColumn for checkWisp5d()

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
    my $tag = 'barcen';

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
                  describeMisindent2( $headColumn, $expectedHeadColumn );
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
          describeMisindent2( $headColumn, $expectedColumn );
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
    if ( my @gapMistakes = @{ $policy->isOneLineGap( $batteryGap, { tag => $tag}, $expectedColumn ) } )
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
          describeMisindent2( $batteryColumn, $expectedColumn );
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
    my ( $anchorColumn, $anchorData ) = $policy->reanchorInc(
        $node,
        {
            # LustisCell => 1, # should NOT reanchor at Lustis
            # LushepCell => 1, # should NOT reanchor at Lushep
            # LuslusCell => 1, # should NOT reanchor at Luslus, per experiment
            # tallTisgar => 1, # should NOT reanchor at TISGAR, per experiment
            tallKetbar => 1,
            tallKetwut => 1,
        }
    );
    my $anchorDetails = $policy->anchorDetails($node, $anchorData );

    my $batteryNodeIX = $battery->{IX};
    $policy->{perNode}->{$batteryNodeIX}->{anchorColumn} = $anchorColumn;

    my ( $batteryLine, $batteryColumn ) = $instance->nodeLC($battery);

    my @mistakes = ();
    my $tag = 'barcen';

    my $gapLiteral = $instance->literalNode($gap);
    my $expectedColumn;

    if ( $parentLine == $batteryLine) {
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
              describeMisindent2( $batteryColumn, $expectedColumn );
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
    if ( my @gapMistakes = @{ $policy->isOneLineGap( $gap, { tag => $tag}, $expectedColumn ) } )
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
		anchorDetails => $policy->anchorDetails($node, $anchorData),
                topicLines   => [ $batteryLine ],
              };
        }
    }

    if ( $batteryColumn != $expectedColumn ) {
        my $msg = sprintf 'split Barcen battery %s; %s',
          describeLC( $batteryLine, $batteryColumn ),
          describeMisindent2( $batteryColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $batteryLine,
            column         => $batteryColumn,
	    anchorDetails => $policy->anchorDetails($node, $anchorData),
            expectedColumn => $expectedColumn,
          };
        return \@mistakes;
    }

    return \@mistakes;
}

sub checkBarket {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    # TODO: reanchoring logic, memoize anchorColumn for checkWisp5d()

    my ( $headGap, $head, $batteryGap, $battery ) = @{$policy->gapSeq0($node)};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my $anchorNode = $node;
    my ( $anchorLine,  $anchorColumn )  = $instance->nodeLC($anchorNode);
    my ( $headLine, $headColumn ) = $instance->nodeLC($head);
    my ( $batteryLine, $batteryColumn ) = $instance->nodeLC($battery);

    my @mistakes = ();
    my $tag = 'barket';

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
		    details => [ [ $tag ] ],
                  };
                last HEAD_ISSUES;
            }
            my $expectedHeadColumn = $pseudoJoinColumn;
            if ( $headColumn != $expectedHeadColumn ) {
                my $msg =
                  sprintf
'Pseudo-joined Barket head; head/comment mismatch; head is %s',
                  describeLC( $headLine, $headColumn ),
                  describeMisindent2( $headColumn, $expectedHeadColumn );
                push @mistakes,
                  {
                    desc           => $msg,
                    parentLine     => $parentLine,
                    parentColumn   => $parentColumn,
                    line           => $headLine,
                    column         => $headColumn,
                    expectedColumn => $expectedHeadColumn,
		    details => [ [ $tag ] ],
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
          describeMisindent2( $headColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $headLine,
            column         => $headColumn,
            expectedColumn => $expectedColumn,
	    details => [ [ $tag ] ],
          };

    }

    $expectedColumn = $anchorColumn;
    if (
        my @gapMistakes = @{
            $policy->isOneLineGap( $batteryGap, { tag => $tag },
                $expectedColumn )
        }
      )
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
                topicLines   => [$batteryLine],
              };
        }
    }

    if ( $batteryColumn != $expectedColumn ) {
        my $msg = sprintf 'Barket battery %s; %s',
          describeLC( $batteryLine, $batteryColumn ),
          describeMisindent2( $batteryColumn, $expectedColumn );
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
    my $tag = 'fashep';

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
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
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
        @{ $policy->isOneLineGap( $trailerGap, { tag => $tag }, $expectedColumn ) } )
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
    my $tag = 'faslus';

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
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
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
        @{ $policy->isOneLineGap( $trailerGap, { tag => $tag }, $expectedColumn ) } )
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

sub checkFord_1Gap {
    my ( $policy, $node, $tag ) = @_;
    my $instance = $policy->{lint};

    # fordFassig ::= (- FAS SIG GAP -) tall5d
    my ( $gap, $body ) = @{ $policy->gapSeq0($node) };

    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);

    my @mistakes = ();

    my $expectedBodyColumn;

  BODY_ISSUES: {
        if ( $parentLine == $bodyLine ) {
            my $expectedBodyColumn = $parentColumn + 4;
            if ( $bodyColumn != $expectedBodyColumn ) {
                my $msg =
                  sprintf 'joined %s body %s is %s',
                  $tag,
                  describeLC( $bodyLine, $bodyColumn ),
                  describeMisindent2( $bodyColumn, $expectedBodyColumn );
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

        # If here parent line != body line
        $expectedBodyColumn = $parentColumn;
        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $gap, { tag => $tag }, $expectedBodyColumn ) } )
        {
            for my $gapMistake (@gapMistakes) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf 'split %s body %s; %s',
                  $tag,
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

        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg =
              sprintf 'split %s body %s is %s',
              $tag,
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
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
    my $tag = 'faswut';

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
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
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
        @{ $policy->isOneLineGap( $trailerGap, { tag => $tag }, $expectedColumn ) } )
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

# The only lutes in the arvo/ corpus are one-liners.
# We treat one-line lutes as free-form -- never any errors.
# We report "not yet implemented" for multi-line lutes.
sub checkLute {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    # lutes are special, so we need to find the components using low-level
    # techniques.
    # lute5d ::= (- SEL GAP -) tall5dSeq (- GAP SER -)
    my $children = $node->{children};
    my $sel = $children->[0];
    my $ser = $children->[-1];
    my ( $selLine ) = $instance->nodeLC($sel);
    my ( $serLine ) = $instance->nodeLC($ser);
    
    return $instance->checkNYI($node) if $selLine != $serLine;
    return [];
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
    my $anchorData;
    if ( $lhsName eq 'tallColsig' ) {
        # say join " ", __FILE__, __LINE__, $runeLine, $runeColumn;
	# TODO: Cleanup after development
	($anchorColumn, $anchorData) = $policy->reanchorInc( $node, {
	  'tallCendot' => 1,
	  'tallCenhep' => 1,
	  'tallCenlus' => 1,
	  'tallCollus' => 1,
	  'tallKethep' => 1,
	  'tallTisfas' => 1,
	  } );
    }
    my $anchorDetails;
    $anchorDetails = $policy->anchorDetails($node, $anchorData ) if $anchorData;
    # say join ' ', __FILE__, __LINE__, Data::Dumper::Dumper($anchorDetails);

    my $expectedColumn = $anchorColumn + 2;
    my $tag = 'split 0-running';

    my @mistakes = ();

    # We deal with the running list here, rather than
    # in its own node

    my $runningChildren = [ $runningGap, @{ $running->{children} } ];
    my $runStepCount = (scalar @{$runningChildren})/2;
    if ( $runStepCount < $minimumRunsteps ) {

        # Untested

        my $msg =
          sprintf
          '%s %s; too few runsteps; has %d, minimum is %d',
	  $tag,
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

        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $runningGap,
                {
                    interColumn => $runeColumn,
		    preColumn => $expectedColumn,
                    tag         => $tag,
                }
            )
          };

    push @mistakes,
      @{
        $policy->checkRunning( { children => $runningChildren,
           tag => $tag, anchorColumn => $anchorColumn, expectedColumn => $expectedColumn,
	   anchorDetails => $anchorDetails,
	})
      };

    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $tistisGap, { tag => $tag }, $anchorColumn, $expectedColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              '%s TISTIS %s; %s',
	      $tag,
              describeLC( $tistisLine, $tistisColumn ),
              $gapMistakeMsg;
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

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
		subpolicyTag => $policy->nodeSubpolicy($node),
                tag            => $tag,
                expectedColumn => $anchorColumn,
            }
        )
      };
    return \@mistakes;
}

sub checkJoined_0Running {
    my ( $policy, $node, $joinColumn ) = @_;
    my $gapSeq    = $policy->gapSeq($node);
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};
    my $maximumRunsteps = $instance->{maxJoined_0RunningSteps};

    my ( $rune, $runningGap, $running, $tistisGap, $tistis ) =
      @{ $policy->gapSeq($node) };

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $anchorLine, $anchorColumn ) = ( $runeLine,    $runeColumn );
    my ( $runningLine, $runningColumn ) = $instance->nodeLC($running);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);

    my @mistakes = ();
    my $tag = 'joined 0-running';
    my $expectedColumn = $joinColumn >= 0 ? $joinColumn : $runeColumn + 4;

    # We deal with the running list here, rather than
    # in its own node

    my @runningChildren = ($runningGap, @{$running->{children}});
    my $runStepCount = ( scalar @runningChildren) / 2;
    if ( defined $maximumRunsteps and $runStepCount > $maximumRunsteps ) {

        # Untested
        my $msg = sprintf
          '%s; too many runsteps; has %d, maximum is %d',
          describeLC( $runningLine, $runningColumn ),
          $runStepCount, $maximumRunsteps;
        push @mistakes,
          {
            desc         => $msg,
            parentLine   => $runeLine,
            parentColumn => $runeColumn,
            line         => $runningLine,
            column       => $runningColumn,
	    details => [ [ $tag ] ],
          };
    }

    push @mistakes,
      @{
        $policy->checkRunning( { children => \@runningChildren,
           tag => $tag, anchorColumn => $anchorColumn, expectedColumn => $expectedColumn,
	}
        )
      };

    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $tistisGap, { tag => $tag }, $runeColumn, $expectedColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "TISTIS %s; $gapMistakeMsg",
              describeLC( $tistisLine, $tistisColumn );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $runeLine, $tistisLine ],
	    details => [ [ $tag ] ],
              };
        }
    }

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
		subpolicyTag => $policy->nodeSubpolicy($node),
                tag            => $tag,
                expectedColumn => $runeColumn,
            }
        )
      };
    return \@mistakes;
}

sub check_1Running {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};
    my $tag = '1-running';

    my (
        $rune,       $headGap, $head,
        $runningGap, $running, $tistisGap, $tistis
    ) = @{ $policy->gapSeq($node) };

    my ( $runeLine, $runeColumn ) = $instance->nodeLC( $rune );
    my ( $anchorLine, $anchorColumn ) = ( $runeLine, $runeColumn );
    my ( $headLine, $headColumn ) = $instance->nodeLC( $head );
    my ( $runningLine, $runningColumn ) = $instance->nodeLC( $running );
    my ( $tistisLine, $tistisColumn ) = $instance->nodeLC( $tistis );

    my @mistakes = ();
    if ( $headLine != $runeLine ) {
        my $msg = sprintf
          "$tag head %s; should be on rune line %d",
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
          "$tag head %s; %s",
          describeLC( $headLine, $headColumn ),
          describeMisindent2( $headColumn, $expectedColumn );
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

    $expectedColumn  = $anchorColumn + 2;

    # Note: runnings are never pseudo-joined, at
    # least not in the corpus.
    if ($headLine != $runningLine) {
        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $runningGap,
                {
                    interColumn => $anchorColumn,
		    preColumn => $expectedColumn,
                    tag         => $tag,
                }
            )
          };

	my @runningChildren = ( $runningGap, @{$running->{children}});

        push @mistakes,
          @{
            $policy->checkRunning(
                {
                    children       => \@runningChildren,
                    tag            => $tag,
                    anchorColumn   => $anchorColumn,
                    expectedColumn => $expectedColumn,
                }
            )
          };

    } else {
      # joined, that is, $headLine != $runningLine
	my $gapLength = $runningGap->{length};
	my ( $runningGapLine, $runningGapColumn ) = $instance->nodeLC($runningGap);
	my $nextExpectedColumn = $runningGapColumn + 2;

	if ($nextExpectedColumn != $runningColumn) {
            my $msg            = sprintf
              "1-jogging running 1 %s; %s",
              describeLC( $runningLine, $runningColumn ),
              describeMisindent( $runningColumn - $nextExpectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $runeLine,
                parentColumn   => $runeColumn,
                line           => $runningLine,
                column         => $runningColumn,
                expectedColumn => $nextExpectedColumn,
              };
	}

	my @runningChildren = ( $runningGap, @{$running->{children}});

        push @mistakes,
          @{
            $policy->checkRunning(
                {
                    skipFirst      => 1,
                    children       => \@runningChildren,
                    tag            => $tag,
                    anchorColumn   => $anchorColumn,
                    expectedColumn => $expectedColumn
                }
            )
          };

    }

    # We deal with the running list here, rather that
    # in its own node

    if ( my @gapMistakes = @{ $policy->isOneLineGap( $tistisGap, { tag => $tag }, $runeColumn, $expectedColumn )} )
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
          describeMisindent2( $tistisColumn, $runeColumn );
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

# In the Hoon grammar, some 0 runnings are "punned" as 1-runnings.
# Indentation is *not* the same however, and `hoonlint` must
# treat them separately.
sub check_0_as_1Running {
    my ( $policy, $node ) = @_;
    my $gapSeq   = $policy->gapSeq($node);
    my $instance = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my (
        $rune,       $headGap, $head,
        $runningGap, $running, $tistisGap, $tistis
    ) = @{ $policy->gapSeq($node) };

    my $anchorNode = $instance->firstBrickOfLine($node);
    my ( $anchorLine, $anchorColumn ) = $instance->nodeLC($anchorNode);
    my ( $runeLine, $runeColumn ) = $instance->nodeLC( $rune );
    my ( $headLine, $headColumn ) = $instance->nodeLC( $head );
    my ( $runningLine, $runningColumn ) = $instance->nodeLC( $running );
    my ( $tistisLine, $tistisColumn ) = $instance->nodeLC( $tistis );

    my @mistakes = ();
    my $tag = '0as1-running';
    my $expectedColumn = $anchorColumn;

        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $runningGap,
                {
                    interColumn => $runeColumn,
		    preColumn => $expectedColumn,
                    tag         => $tag,
                }
            )
          };

    # We deal with the running list here, rather that
    # in its own node

    # "De-pun" the 0-running by prepending the
    # fake head to the list of running children
    my @runningChildren = ($headGap, $head, $runningGap);
    push @runningChildren,  @{$running->{children}};

    push @mistakes,
      @{
        $policy->checkRunning(
            {
                children       => \@runningChildren,
                tag            => $tag,
                anchorColumn   => $anchorColumn,
                expectedColumn => $expectedColumn
            }
        )
      };

    if ( my @gapMistakes = @{ $policy->isOneLineGap( $tistisGap, { tag => $tag }, $anchorColumn )} )
        {
            for my $gapMistake ( @gapMistakes ) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $gapSubpolicy = $gapMistake->{subpolicy} // q{};
		my $msg;
		if ($gapSubpolicy eq 'missing-newline') {
                $msg              = sprintf
                  'gap %s; vertical gap must precede TISTIS',
                  describeLC( $gapMistakeLine, $gapMistakeColumn );
		} else {
                $msg              = sprintf
                  "$tag TISTIS %s; $gapMistakeMsg",
                  describeLC( $tistisLine, $tistisColumn );
		  }
		 my $mistakeSubpolicy = $policy->nodeSubpolicy($node);
		 $mistakeSubpolicy .= ':' . $gapSubpolicy if $gapSubpolicy;
                push @mistakes,
                  {
                    desc         => $msg,
		    subpolicy => $mistakeSubpolicy,
                    parentLine   => $runeLine,
                    parentColumn => $runeColumn,
                    line         => $gapMistakeLine,
                    column       => $gapMistakeColumn,
		    details => [ [ $tag ] ],
                    topicLines   => [ $anchorLine, $tistisLine ],
                  };
            }
        }

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
		subpolicyTag => $policy->nodeSubpolicy($node),
                tag            => $tag,
                expectedColumn => $anchorColumn,
            }
        )
      };
    return \@mistakes;
}

# Format line and 0-based column as string
sub describeLC {
    my ( $line, $column ) = @_;
    return '@' . $line . ':' . ( $column + 1 );
}

sub describeMisindent {
    my ( $difference ) = @_;
    if ( $difference > 0 ) {
        return "overindented by $difference";
    }
    if ( $difference < 0 ) {
        return "underindented by " . (-$difference);
    }
    return "correctly indented";
}

sub describeMisindent2 {
    my ( $got, $sought ) = @_;
    return describeMisindent($got-$sought);
}

sub chessSideOfJoggingHoon {
    my ( $policy, $node ) = @_;
    my $nodeIX = $node->{IX};
    my $chessSide = $policy->{perNode}->{$nodeIX}->{chessSide};
    return $chessSide if defined $chessSide;

    my $instance = $policy->{lint};
    my $joggingRule = $instance->{joggingRule};
    my $nodeName = $instance->brickName($node);
    if (not $nodeName or not $joggingRule->{$nodeName}) {
      my $chessSide = $policy->chessSideOfJoggingHoon($node->{PARENT});
      $policy->{perNode}->{$nodeIX}->{chessSide} = $chessSide;
      return $chessSide;
    }

    my ( undef, $baseColumn ) = $instance->nodeLC( $node );
    my $children = $node->{children};
  CHILD: for my $childIX ( 0 .. $#$children ) {
        my $child  = $children->[$childIX];
        my $symbol = $instance->symbol($child);
        next CHILD if $symbol ne 'rick5d' and $symbol ne 'ruck5d';
        my $chessSide = $policy->chessSideOfJogging( $child, $baseColumn );
	$policy->{perNode}->{$nodeIX}->{chessSide} = $chessSide;
	return $chessSide;
    }
    die "No jogging found for ", $instance->symbol($node);
}

sub chessSideOfJogging {
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

# Find the body column, based on alignment within
# a parent hoon.
sub bodyColumn {
    my ( $policy, $node ) = @_;
    my $nodeIX = $node->{IX};
    my $jogBodyColumn = $policy->{perNode}->{$nodeIX}->{jogBodyColumn};
    return $jogBodyColumn if defined $jogBodyColumn;

    my $instance = $policy->{lint};
    my $joggingRules = $instance->{joggingRule};
    my $joggingRule = $instance->{joggingRule};
    my $nodeName = $instance->brickName($node);
    if (not $nodeName or not $joggingRule->{$nodeName}) {
      my $jogBodyColumn = $policy->bodyColumn($node->{PARENT}, $joggingRules);
      $policy->{perNode}->{$nodeIX}->{jogBodyColumn} = $jogBodyColumn;
      return $jogBodyColumn;
    }

    my $children = $node->{children};
  CHILD: for my $childIX ( 0 .. $#$children ) {
        my $child  = $children->[$childIX];
        my $symbol = $instance->symbol($child);
        next CHILD if $symbol ne 'rick5d' and $symbol ne 'ruck5d';
        my $jogBodyColumn = $policy->joggingBodyAlignment($child);
	$policy->{perNode}->{$nodeIX}->{jogBodyColumn} = $jogBodyColumn;
        return $jogBodyColumn;
    }
    die "No jogging found for ", $instance->symbol($node);
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

sub check_1Jogging {
    my ( $policy, $node ) = @_;
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

    my $chessSide = $policy->chessSideOfJoggingHoon($node);
    my $joggingRules = $instance->{joggingRule};
    my $jogBodyColumn = $policy->bodyColumn($node, $joggingRules);

    my @mistakes = ();
    my $tag = '1-jogging';

    if ( $headLine != $runeLine ) {
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
          describeMisindent2( $headColumn, $expectedColumn );
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

        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $joggingGap,
                {
                    interColumn => $runeColumn,
                    tag         => (
                        sprintf '1-jogging %s jogging',
                        $chessSide
                    ),
                    parent     => $rune,
                    topicLines => [$joggingLine],
                }
            )
          };

    if ( my @gapMistakes = @{$policy->isOneLineGap( $tistisGap, { tag => $tag }, $runeColumn )} )
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
          describeMisindent2( $tistisColumn, $runeColumn );
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

sub check_2Jogging {
    my ( $policy, $node ) = @_;
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

    my $chessSide = $policy->chessSideOfJoggingHoon($node);
    my $joggingRules = $instance->{joggingRule};
    my $jogBodyColumn = $policy->bodyColumn($node, $joggingRules);

    my @mistakes = ();
    my $tag = '2-jogging';

    if ( $headLine != $runeLine ) {
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
              describeMisindent2( $headColumn, $expectedColumn );
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
              describeMisindent2( $subheadColumn, $expectedColumn );
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
              describeMisindent2( $headColumn, $expectedColumn );
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
            @{ $policy->isOneLineGap( $subheadGap, { tag => $tag }, $runeColumn ) } )
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
              describeMisindent2( $subheadColumn, $expectedColumn );
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

    if ( my @gapMistakes = @{$policy->isOneLineGap( $joggingGap, { tag => $tag }, $runeColumn )} )
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

    if ( my @gapMistakes = @{$policy->isOneLineGap( $tistisGap, { tag => $tag }, $runeColumn )} )
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
          describeMisindent2( $tistisColumn, $runeColumn );
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

sub check_Jogging1 {
    my ( $policy, $node ) = @_;
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

    my @mistakes = ();
    my $tag = 'jogging-1';

    if ( $joggingLine != $runeLine ) {
        my $msg = sprintf
          "jogging %s; should be on rune line %d",
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
	    details => [ [ $tag ] ],
          };
    }

    my $expectedColumn = $runeColumn + 4;
    if ( $joggingColumn != $expectedColumn ) {
        my $msg = sprintf
          "jogging %s; %s",
          describeLC( $joggingLine, $joggingColumn ),
          describeMisindent2( $joggingColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $joggingLine,
            column         => $joggingColumn,
            expectedColumn => $expectedColumn,
	    details => [ [ $tag ] ],
          };
    }

    if ( my @gapMistakes = @{$policy->isOneLineGap( $tistisGap, { tag => $tag }, $runeColumn )} )
    {
        for my $gapMistake ( @gapMistakes ) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf
              "TISTIS %s; %s",
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
	    details => [ [ $tag ] ],
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
          describeMisindent2( $tistisColumn, $expectedColumn );
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

    if ( my @gapMistakes = @{$policy->isOneLineGap( $tailGap, { tag => $tag }, $runeColumn )} )
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
          describeMisindent2( $tailColumn, $expectedColumn );
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

sub fascomBodyAlignment {
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
        my $jog = $children->[$childIX];
        my ( $gap,      $body )       = @{ $policy->gapSeq0($jog) };
        my ( $headLine, $headColumn ) = $instance->nodeLC($jog);
        my ( $bodyLine, $bodyColumn ) = $instance->nodeLC($body);
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

# Find the body column, based on alignment within
# a parent hoon.
sub fascomBodyColumn {
    my ( $policy, $node ) = @_;
    my $nodeIX           = $node->{IX};
    my $fascomBodyColumn = $policy->{perNode}->{$nodeIX}->{fascomBodyColumn};
    return $fascomBodyColumn if defined $fascomBodyColumn;

    my $instance = $policy->{lint};
    my $nodeName = $instance->brickName($node);
    if ( not $nodeName or not $nodeName eq 'fordFascom' ) {

        my $fascomBodyColumn = $policy->fascomBodyColumn( $node->{PARENT} );
        $policy->{perNode}->{$nodeIX}->{fascomBodyColumn} = $fascomBodyColumn;
        return $fascomBodyColumn;
    }

    my $children = $node->{children};
  CHILD: for my $childIX ( 0 .. $#$children ) {
        my $child  = $children->[$childIX];
        my $symbol = $instance->symbol($child);
        next CHILD if $symbol ne 'fordFascomBody';
        my $children2 = $child->{children};
      CHILD2: for my $childIX2 ( 0 .. $#$children2 ) {
            my $child2 = $children2->[$childIX2];
            my $symbol2 = $instance->symbol($child2);
            next CHILD2 if $symbol2 ne 'fordFascomElements';
            my $fascomBodyColumn = $policy->fascomBodyAlignment($child2);
            $policy->{perNode}->{$nodeIX}->{fascomBodyColumn} =
              $fascomBodyColumn;
            return $fascomBodyColumn;
        }
    }
    die "No jogging found for ", $instance->symbol($node);
}

# TODO: Add a check (optional?) for queenside joggings with no
# split jogs.
sub checkFascomElement {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    my $runeNode = $instance->ancestorByBrickName( $node, 'fordFascom' );
    my ( $runeLine,   $runeColumn )   = $instance->nodeLC($runeNode);
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $headLine,   $headColumn )   = ( $parentLine, $parentColumn );
    my ( $gap,        $body )         = @{ $policy->gapSeq0($node) };
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);

    my $fascomBodyColumn =
      $policy->fascomBodyColumn( $node, { fordFascom => 1 } );

    my @mistakes = ();
    my $tag = 'fascom element';

    my $baseColumn = $runeColumn + 4;

    my $expectedHeadColumn = $baseColumn;
    if ( $headColumn != $expectedHeadColumn ) {
        my $msg = sprintf 'Fascom element head %s; %s',
          describeLC( $headLine, $headColumn ),
          describeMisindent2( $headColumn, $expectedHeadColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $headLine,
            column         => $headColumn,
            expectedColumn => $expectedHeadColumn,
            topicLines     => [$runeLine],
          };
    }

    if ( $headLine == $bodyLine ) {
        my $gapLength = $gap->{length};

        if ( $gapLength != 2 and $bodyColumn != $fascomBodyColumn ) {
            my $msg = sprintf 'Fascom element body %s; %s',
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $bodyColumn, $fascomBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $fascomBodyColumn,
                topicLines     => [$runeLine],
              };
        }
        return \@mistakes;
    }

    # If here head line != body line
    my $pseudoJoinColumn = $policy->pseudoJoinColumn($gap);
    if ( $pseudoJoinColumn >= 0 ) {
        my $expectedBodyColumn = $pseudoJoinColumn;
        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg =
              sprintf
'Pseudo-joined Fascom element %s; body/comment mismatch; body is %s',
              describeLC( $parentLine, $parentColumn ),
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedBodyColumn,
                topicLines     => [$runeLine],
              };
        }

        # Treat the fascom body alignment as the "expected one"
        my $expectedColumn = $fascomBodyColumn;
        if ( $bodyColumn != $expectedColumn ) {
            my $msg = sprintf 'Pseudo-joined Fascom element %s; body %s',
              describeLC( $parentLine, $parentColumn ),
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $expectedColumn,
                topicLines     => [$runeLine],
              };
        }
        return \@mistakes;
    }

    # If here, this is (or should be) a split jog
    my $expectedBodyColumn = $headColumn - 2;

    if ( $bodyColumn != $expectedBodyColumn ) {
        my $msg = sprintf 'Fascom element body %s; %s',
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent2( $bodyColumn, $expectedBodyColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $bodyLine,
            column         => $bodyColumn,
            expectedColumn => $expectedBodyColumn,
            topicLines     => [$runeLine],
          };
        return \@mistakes;
    }

    if ( my @gapMistakes =
        @{ $policy->isOneLineGap( $gap, { tag => $tag }, $expectedBodyColumn ) } )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf 'Fascom element split body %s; %s',
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [$runeLine],
              };
        }
    }
    return \@mistakes;
}

sub checkFastis {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    # fordFastis ::= (- FASTISGAP -) SYM4K (- GAP -) horn
    my ( $headGap, $symbol, $hornGap, $horn ) = @{ $policy->gapSeq0($node) };
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $symbolLine, $symbolColumn ) = $instance->nodeLC($symbol);
    my ( $hornLine,   $hornColumn )   = $instance->nodeLC($horn);

    my @mistakes = ();
    my $tag = 'fastis';

  CHECK_SYMBOL: {
        if ( $symbolLine != $parentLine ) {
            my $msg = sprintf 'Fastis symbol %s; symbol must be on rune line',
              describeLC( $symbolLine, $symbolColumn );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $symbolLine,
                column       => $symbolColumn,
              };
            last CHECK_SYMBOL;
        }

        my $expectedSymbolColumn = $parentColumn + 4;
        if ( $symbolColumn != $expectedSymbolColumn ) {
            my $msg = sprintf 'Fastis symbol %s; %s',
              describeLC( $symbolLine, $symbolColumn ),
              describeMisindent2( $symbolColumn, $expectedSymbolColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $symbolLine,
                column         => $symbolColumn,
                expectedColumn => $expectedSymbolColumn,
              };
        }
    }

  CHECK_HORN: {
        if ( $hornLine == $symbolLine ) {
	    my $symbolLength = $symbol->{length};
	    my $expectedHornColumn = $symbolColumn + $symbolLength + 2;
            if ( $hornColumn != $expectedHornColumn ) {
                my $msg = sprintf 'Fastis horn %s; %s',
                  describeLC( $hornLine, $hornColumn ),
                  describeMisindent2( $hornColumn, $expectedHornColumn );
                push @mistakes,
                  {
                    desc           => $msg,
                    parentLine     => $parentLine,
                    parentColumn   => $parentColumn,
                    line           => $hornLine,
                    column         => $hornColumn,
                    expectedColumn => $expectedHornColumn,
                  };
            }
            last CHECK_HORN;
        }

        # if here, horn Line != symbol line
	my $expectedHornColumn = $parentColumn + 2;
        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $hornGap, { tag => $tag }, $expectedHornColumn ) } )
        {
            for my $gapMistake (@gapMistakes) {
                my $gapMistakeMsg    = $gapMistake->{msg};
                my $gapMistakeLine   = $gapMistake->{line};
                my $gapMistakeColumn = $gapMistake->{column};
                my $msg              = sprintf 'Fastis split horn %s; %s',
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

        if ( $hornColumn != $expectedHornColumn ) {
            my $msg = sprintf 'Fastis split horn %s; %s',
              describeLC( $hornLine, $hornColumn ),
              describeMisindent2( $hornColumn, $expectedHornColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $hornLine,
                column         => $hornColumn,
                expectedColumn => $expectedHornColumn,
              };
        }

    }

    return \@mistakes;
}

sub checkKingsideJog {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $tall_Jogging1Rule = $instance->{tallJogging1_Rule};
    my $fileName = $instance->{fileName};
    my $grammar  = $instance->{grammar};
    my $ruleID   = $node->{ruleID};
    my ( $parentLine, $parentColumn ) =
      $instance->line_column( $node->{start} );

    my $joggingRules = $instance->{joggingRule};
    my $jogBodyColumn = $policy->bodyColumn($node, $joggingRules);

    my @mistakes = ();
    my $tag = 'kingside jog';

    # Replace inherited attribute rune LC with brick LC
    my $brickNode = $instance->brickNode($node);
    my ( $brickLine, $brickColumn ) = $instance->nodeLC($brickNode);
    my $brickName = $instance->brickName($brickNode);
    my $baseColumn =
      $tall_Jogging1Rule->{$brickName} ? $brickColumn + 4 : $brickColumn + 2;

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
          describeMisindent2( $headColumn, $expectedHeadColumn );
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

        if ( $gapLength != 2 and $bodyColumn != $jogBodyColumn ) {
            my $msg = sprintf 'Jog %s body %s; %s',
              $sideDesc,
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $bodyColumn, $jogBodyColumn );
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
    if ( $pseudoJoinColumn >= 0 ) {
        my $expectedBodyColumn = $pseudoJoinColumn;
        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg =
              sprintf
              'Pseudo-joined %s Jog %s; body/comment mismatch; body is %s',
              $sideDesc,
              describeLC( $parentLine, $parentColumn ),
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
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
        }
        my $headLength = $head->{length};

        # Treat the jogging body alignment as the "expected one"
        my $expectedColumn = $jogBodyColumn;
        my $raggedColumn   = $headColumn + $headLength + 2;
        if ( $bodyColumn != $raggedColumn and $bodyColumn != $expectedColumn ) {
            my $msg = sprintf 'Pseudo-joined %s Jog %s; body %s',
              $sideDesc, describeLC( $parentLine, $parentColumn ),
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
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
          describeMisindent2( $bodyColumn, $expectedBodyColumn );
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

    if ( my @gapMistakes = @{ $policy->isOneLineGap( $gap, { tag => $tag }, $expectedBodyColumn )} )
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
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my ( $parentLine, $parentColumn ) =
      $instance->line_column( $node->{start} );
    my $ruleID   = $node->{ruleID};
    my $fileName = $instance->{fileName};
    my $grammar  = $instance->{grammar};

    my @mistakes = ();
    my $tag = 'queenside jog';

    my $joggingRules = $instance->{joggingRule};
    my $jogBodyColumn = $policy->bodyColumn($node, $joggingRules);

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
          describeMisindent2( $headColumn, $expectedHeadColumn );
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

    # Check for flat queenside misalignments
    my $expectedBodyColumn = $jogBodyColumn;
    if ( $headLine == $bodyLine ) {
        my $gapLength = $gap->{length};
        if ( $gapLength != 2 and $bodyColumn != $jogBodyColumn ) {
            my $msg = sprintf 'Jog %s body %s; %s',
              $sideDesc,
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $bodyColumn, $jogBodyColumn );
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

    # If here, this is a split jog
    $expectedBodyColumn = $brickColumn + 2;
    if ( $bodyColumn != $expectedBodyColumn ) {

        my $msg = sprintf 'Jog %s body %s; %s',
          $sideDesc,
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent2( $bodyColumn, $expectedBodyColumn );
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

    if ( my @gapMistakes = @{ $policy->isOneLineGap( $gap, { tag => $tag }, $expectedBodyColumn )} )
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
sub checkJog {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    my $chessSide = $policy->chessSideOfJoggingHoon($node);
    return $policy->checkQueensideJog( $node )
      if $chessSide eq 'queenside';
    return $policy->checkKingsideJog( $node );
}

# not yet implemented
sub checkNYI {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $grammar         = $instance->{grammar};
    my ( $parentLine, $parentColumn ) =
      $instance->line_column( $node->{start} );
    my $ruleID   = $node->{ruleID};
    my @mistakes = ();

    my $msg = join q{ }, 'NYI', '[' . $instance->symbol($node) . ']',
      $instance->describeNodeRange($node),
      ( map { $grammar->symbol_display_form($_) }
          $grammar->rule_expand($ruleID) );
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

sub checkBackdented {
    my ( $policy, $node ) = @_;
    my $nodeIX = $node->{IX};
    my @gapSeq       = @{ $policy->gapSeq0($node) };
    my $elementCount = ( scalar @gapSeq ) / 2;
    my $instance     = $policy->{lint};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my @mistakes = ();
    my $tag = $elementCount . '-backdented';

    my $reanchorOffset; # for re-anchoring logic

  ENFORCE_ELEMENT1_JOINEDNESS: {
	# TODO: Is this right?
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
        my ( $gapLine, $gapColumn ) = $instance->nodeLC($gap);
        my $expectedColumn =
          $anchorColumn + ( $elementCount - $elementNumber ) * 2;

        if ( $elementLine == $parentLine ) {
            my $gapLiteral = $instance->literalNode($gap);
	    # Remove the rune, if present
	    $gapLiteral = substr($gapLiteral, 2) if $instance->runeGapNode($gap);

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
              describeMisindent2( $elementColumn, $expectedColumn );
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

      CHECK_FOR_PSEUDOJOIN: {
            last CHECK_FOR_PSEUDOJOIN if $gapLine != $parentLine;
            my $pseudoJoinColumn = $policy->pseudoJoinColumn($gap);

            last CHECK_FOR_PSEUDOJOIN if $pseudoJoinColumn < 0;

            last CHECK_FOR_PSEUDOJOIN if $pseudoJoinColumn != $expectedColumn
                and $pseudoJoinColumn != $parentColumn + 4;

            if ( $elementColumn != $pseudoJoinColumn ) {
                my $msg =
                  sprintf
'Pseudo-joined backdented element %d; element/comment mismatch; element is %s',
		  $elementNumber,
                  describeLC( $elementLine, $elementColumn ),
                  describeMisindent2( $elementColumn, $pseudoJoinColumn );
                push @mistakes,
                  {
                    desc           => $msg,
                    parentLine     => $parentLine,
                    parentColumn   => $parentColumn,
                    line           => $elementLine,
                    column         => $elementColumn,
                    expectedColumn => $pseudoJoinColumn,
                  };
            }

            next ELEMENT;
        }

	# For the use of re-anchoring logic, determine the additional offset
	# reguired for the next line after the rune line
        if ( not defined $reanchorOffset ) {
            $reanchorOffset = 2 + ( $elementCount - $elementNumber ) * 2;
            $policy->{perNode}->{$nodeIX}->{reanchorOffset} = $reanchorOffset;
        }

        if ( my @gapMistakes =
            @{ $policy->isOneLineGap( $gap, { tag => $tag }, $anchorColumn ) } )
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

        if ( $expectedColumn != $elementColumn ) {
            my $msg = sprintf
              'backdented element #%d %s; %s',
              $elementNumber,
              describeLC( $elementLine, $elementColumn ),
              describeMisindent2( $elementColumn, $expectedColumn );
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

# Ketdot is slightly different form other backdented hoons
sub checkKetdot {
    my ( $policy, $node ) = @_;
    my @gapSeq   = @{ $policy->gapSeq0($node) };
    my $instance = $policy->{lint};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my @mistakes = ();
    my $tag = 'ketdot';

    my $anchorNode =
      $instance->firstBrickOfLineInc( $node, { tallKetdot => 1 } );
    my ( $anchorLine, $anchorColumn ) = $instance->nodeLC($anchorNode);

    my $gap1     = $gapSeq[0];
    my $element1 = $gapSeq[1];
    my ( $element1Line, $element1Column ) = $instance->nodeLC($element1);

  ELEMENT: {    # Element 1

        my $expectedColumn = $parentColumn + 4;

        if ( $element1Line != $parentLine ) {
            my $msg = sprintf
              "Ketdot element 1 %s; element 1 expected to be on rune line",
              describeLC( $element1Line, $element1Column );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $element1Line,
                column       => $element1Column,
              };
            last ELEMENT;
        }

        if ( $expectedColumn != $element1Column ) {
            my $msg = sprintf
              'Ketdot element 1 %s; %s',
              describeLC( $element1Line, $element1Column ),
              describeMisindent2( $element1Column, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $element1Line,
                column         => $element1Column,
                expectedColumn => $expectedColumn,
              };
        }
    }

    my $gap2     = $gapSeq[2];
    my $element2 = $gapSeq[3];
    my ( $element2Line, $element2Column ) = $instance->nodeLC($element2);

  ELEMENT2: {
        if ( $element1Line != $element2Line ) {    # Element 2 split

            my $expectedColumn = $anchorColumn;

            if ( my @gapMistakes =
                @{ $policy->isOneLineGap( $gap2, { tag => $tag }, $anchorColumn ) } )
            {
                for my $gapMistake (@gapMistakes) {
                    my $gapMistakeMsg    = $gapMistake->{msg};
                    my $gapMistakeLine   = $gapMistake->{line};
                    my $gapMistakeColumn = $gapMistake->{column};
                    my $msg              = sprintf 'Ketdot element 2 %s; %s',
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

            if ( $expectedColumn != $element2Column ) {
                my $msg = sprintf
                  'Ketdot element 2 %s; %s',
                  describeLC( $element2Line, $element2Column ),
                  describeMisindent2( $element2Column, $expectedColumn );
                push @mistakes,
                  {
                    desc           => $msg,
                    parentLine     => $parentLine,
                    parentColumn   => $parentColumn,
                    line           => $element2Line,
                    column         => $element2Column,
                    expectedColumn => $expectedColumn,
                  };
            }
            last ELEMENT2;
        }

        # If here, joined element 2

        my $gapLiteral = $instance->literalNode($gap2);
        my $gapLength  = $gap2->{length};
        last ELEMENT2 if $gapLength == 2;
        my ( undef, $gap2Column ) = $instance->nodeLC($gap2);

        # expected length is the length if the spaces at the end
        # of the gap-equivalent were exactly one stop.
        my $expectedLength = $gapLength + ( 2 - length $gapLiteral );
        my $expectedColumn = $gap2Column + $expectedLength;

        if ( $expectedColumn != $element2Column ) {
            my $msg = sprintf
              'Ketdot element 2 %s; %s',
              describeLC( $element2Line, $element2Column ),
              describeMisindent2( $element2Column, $expectedColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $element2Line,
                column         => $element2Column,
                expectedColumn => $expectedColumn,
              };
        }
    }

    return \@mistakes;
}

sub checkLuslus {
    my ( $policy, $node, $cellLHS ) = @_;
    my $instance = $policy->{lint};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC( $node );

    my $battery = $instance->ancestorByLHS( $node, { whap5d => 1 } );
    die "battery not found" if not defined $battery;
    my ( $batteryLine, $batteryColumn ) = $instance->nodeLC($battery);
    my $cellBodyColumn = $policy->cellBodyColumn($battery);

    my @mistakes = ();
    my $tag = 'luslus';

    # LuslusCell ::= (- LUS LUS GAP -) SYM4K (- GAP -) tall5d
    my ( $headGap, $head, $bodyGap, $body)       = @{ $policy->gapSeq0($node) };
    my ( $headLine, $headColumn ) = $instance->nodeLC($head);
    my ( $bodyLine, $bodyColumn ) = $instance->nodeLC($body);

    my $headGapLength = $headGap->{length};
    if ($headGapLength != 2) {
	    my $expectedColumn = $parentColumn+4 ;
            my $msg = sprintf 'Cell head %s; %s',
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $headColumn, $expectedColumn );
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

    if ( $headLine == $bodyLine ) {
        my $bodyGapLength = $bodyGap->{length};

        if ( $bodyGapLength != 2 and $bodyColumn != $cellBodyColumn ) {
            my $msg = sprintf 'Cell body %s; %s',
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $bodyColumn, $cellBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $cellBodyColumn,
                topicLines     => [$batteryLine],
              };
        }
        return \@mistakes;
    }

    # If here head line != body line
  CHECK_FOR_PSEUDOJOIN: {
        my $pseudoJoinColumn = $policy->pseudoJoinColumn($bodyGap);
        last CHECK_FOR_PSEUDOJOIN if $pseudoJoinColumn < 0;
        my $headLength   = $head->{length};
        my $raggedColumn = $headColumn + $headLength + 2;
        last CHECK_FOR_PSEUDOJOIN
          if $pseudoJoinColumn != $raggedColumn
          and $pseudoJoinColumn != $cellBodyColumn;
        if ( $pseudoJoinColumn != $bodyColumn ) {
            my $msg =
              sprintf
              'Pseudo-joined cell %s; body/comment mismatch; body is %s',
              describeLC( $parentLine, $parentColumn ),
              describeMisindent2( $bodyColumn, $pseudoJoinColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                expectedColumn => $pseudoJoinColumn,
                topicLines     => [$batteryLine],
              };
        }
        return \@mistakes;
    }

    # If here, this is (or should be) a split cell
    my $expectedBodyColumn = $parentColumn + 2;

    if ( $bodyColumn != $expectedBodyColumn ) {
        my $msg = sprintf 'cell body %s; %s',
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent2( $bodyColumn, $expectedBodyColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $bodyLine,
            column         => $bodyColumn,
            expectedColumn => $expectedBodyColumn,
            topicLines     => [$batteryLine],
          };
        return \@mistakes;
    }

    if ( my @gapMistakes = @{ $policy->isOneLineGap( $bodyGap, { tag => $tag }, $expectedBodyColumn )} )
    {
        for my $gapMistake ( @gapMistakes ) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $msg              = sprintf 'cell split body %s; %s',
              describeLC( $gapMistakeLine, $gapMistakeColumn ),
              $gapMistakeMsg;
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => [ $bodyLine, $batteryLine ],
              };
        }
    }
    return \@mistakes;
}

sub validate {
  my ($policy, $node ) = @_;
  my $instance = $policy->{lint};

  $policy->validate_node($node);
  return if $node->{type} ne 'node';
  my $children = $node->{children};
  CHILD: for my $childIX ( 0 .. $#$children ) {
        my $child = $children->[$childIX];
        $policy->validate( $child );
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
    my ( $policy, $node ) = @_;

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
    my $tall_0_as_1RunningRule = $instance->{tall_0_as_1RunningRule};
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

    my $children = $node->{children};

    my $nodeType = $node->{type};
    return if $nodeType ne 'node';

    my $ruleID = $node->{ruleID};
    my ( $lhs, @rhs ) = $grammar->rule_expand( $node->{ruleID} );
    my $lhsName = $grammar->symbol_name($lhs);

    if ( $lhsName eq 'optGay4i' ) {
        return;
    }

    my $childCount = scalar @{$children};
    if ( $childCount <= 1 ) {
        return;
    }

    my $firstChildIndent = $instance->column( $children->[0]->{start} ) - 1;

    my $gapiness = $ruleDB->[$ruleID]->{gapiness} // 0;

    my $reportType = $gapiness < 0 ? 'sequence' : 'indent';

    # TODO: In another policy, warn on tall children of wide nodes
    if ( $gapiness == 0 ) {    # wide node
        return;
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

                if ( $lhsName eq 'fordFascomElements' ) {
                    $mistakes = $policy->checkFascomElements($node);
                    last TYPE_INDENT if @{$mistakes};
                    $indentDesc = 'FASCOM ELEMENTS';
                    last TYPE_INDENT;
                }

                if ( $lhsName eq 'fordHoopSeq' ) {
                    $mistakes = $policy->checkSeq($node, 'hoop');
                    last TYPE_INDENT if @{$mistakes};
                    $indentDesc = 'FORD_HOOP_SEQ';
                    last TYPE_INDENT;
                }

                if ( $lhsName eq 'hornSeq' ) {
                    $mistakes = $policy->checkSeq($node, 'horn');
                    last TYPE_INDENT if @{$mistakes};
                    $indentDesc = 'HORN_SEQ';
                    last TYPE_INDENT;
                }

                if ( $lhsName eq 'optBonzElements' ) {
                    $mistakes = $policy->checkSeq($node, 'bonz element');
                    last TYPE_INDENT if @{$mistakes};
                    $indentDesc = 'BONZ_ELEMENTS';
                    last TYPE_INDENT;
                }

                my $grandParent = $instance->ancestor( $node, 1 );
                my $grandParentName = $instance->brickName($grandParent);
                if ( $lhsName eq 'tall5dSeq' or $lhsName eq 'till5dSeq' ) {
                    if ( $grandParentName eq 'lute5d' ) {
                        $indentDesc = 'LUTE';
                        last TYPE_INDENT;
                    }
                    if ( $tall_1RunningRule->{$grandParentName} ) {
                        $indentDesc = '1-RUNNING';
                        last TYPE_INDENT;
                    }
                    if ( $tall_0RunningRule->{$grandParentName} ) {
                        $indentDesc = '0-RUNNING';
                        last TYPE_INDENT;
                    }
                    if ( $tall_0_as_1RunningRule->{$grandParentName} ) {
                        $indentDesc = '0_AS_1-RUNNING';
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
                      $policy->checkWhap5d( $node );
                    last TYPE_INDENT if @{$mistakes};
                    $indentDesc = 'CELL';
                    last TYPE_INDENT;
                }

                # By default, treat as not yet implemented
                $mistakes = $policy->checkNYI($node);
                last TYPE_INDENT if @{$mistakes};

                # should never reach here
                die "NYI";
            }

            last GATHER_MISTAKES;
        }

        # if here, gapiness > 0

      TYPE_INDENT: {

            if ( $lhsName eq "bont5d" ) {
                $mistakes = $policy->checkBont($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'BONT';
                last TYPE_INDENT;
            }

            if ( $lhsName eq "bonzElement" ) {
                $mistakes = $policy->checkBonzElement($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'BARCAB';
                last TYPE_INDENT;
            }

            if ( $lhsName eq "fordFascom" ) {
                $mistakes = $policy->checkFascom($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'FASWUT';
                last TYPE_INDENT;
            }

            for my $tag (
                qw(fordFassig fordFasbuc fordFascab fordFascen fordFashax
		fordHoop
		))
            {
                if ( $lhsName eq $tag ) {
                    $mistakes = $policy->checkFord_1Gap( $node, $tag );
                    last TYPE_INDENT if @{$mistakes};
                    $indentDesc = 'FAS' . uc $tag;
                    last TYPE_INDENT;
                }
            }

            if ( $lhsName eq "fordFastis" ) {
                $mistakes = $policy->checkFastis($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'FASTIS';
                last TYPE_INDENT;
            }

            if ( $lhsName eq "fordFaswut" ) {
                $mistakes = $policy->checkFaswut($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'FASWUT';
                last TYPE_INDENT;
            }

            if ( $lhsName eq "fordFascomElement" ) {
                $mistakes = $policy->checkFascomElement($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'FASCOM Element';
                last TYPE_INDENT;
            }

            if ( $lhsName eq "lute5d" ) {
                $mistakes = $policy->checkLute($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'LUTE';
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

            if ( $lhsName eq "tallAttribute" ) {
                $mistakes = $policy->checkSailAttribute($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'Sail attribute';
                last TYPE_INDENT;
            }

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

            if ( $lhsName eq "tallKetdot" ) {
                $mistakes = $policy->checkKetdot($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'KETDOT';
                last TYPE_INDENT;
            }

            if ( $lhsName eq 'tallTailOfElem' ) {
                $mistakes = $policy->checkTailOfElem($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = $lhsName;
                last TYPE_INDENT;
            }

            if ( $lhsName eq 'tallTailOfTop' ) {
                $mistakes = $policy->checkTailOfTop($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = $lhsName;
                last TYPE_INDENT;
            }

            if ( $lhsName eq "tallKidsOfTop" ) {
                $mistakes = $policy->checkTopKids($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = $lhsName;
                last TYPE_INDENT;
            }

            if ( $lhsName eq "tallTopSail" ) {
                $mistakes = $policy->checkTopSail($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = $lhsName;
                last TYPE_INDENT;
            }

            if ( $lhsName eq "wisp5d" ) {
                $mistakes = $policy->checkWisp5d($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'WISP5D';
                last TYPE_INDENT;
            }

            if ( $NYI_Rule->{$lhsName} ) {
                $mistakes = $policy->checkNYI($node);
                last TYPE_INDENT if @{$mistakes};

                # should never reach here
                die 'NYI failure';
            }

            if ( $tallJogRule->{$lhsName} ) {
                $mistakes = $policy->checkJog($node);
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

            if ( $tall_0_as_1RunningRule->{$lhsName} ) {
                $mistakes = $policy->check_0_as_1Running($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = '1_AS_0-RUNNING-STYLE';
                last TYPE_INDENT;
            }

            if ( $tall_1RunningRule->{$lhsName} ) {
                $mistakes = $policy->check_1Running($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'RUNNING-1-STYLE';
                last TYPE_INDENT;
            }

            if ( $tall_1JoggingRule->{$lhsName} ) {
                $mistakes = $policy->check_1Jogging($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = '1-JOGGING-STYLE';
                last TYPE_INDENT;
            }

            if ( $tall_2JoggingRule->{$lhsName} ) {
                $mistakes = $policy->check_2Jogging($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = '2-JOGGING-STYLE';
                last TYPE_INDENT;
            }

            if ( $tall_Jogging1Rule->{$lhsName} ) {
                $mistakes = $policy->check_Jogging1($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'JOGGING-1-STYLE';
                last TYPE_INDENT;
            }

            if ( $tallNoteRule->{$lhsName} ) {
                $mistakes = $policy->checkBackdented($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'CAST-STYLE';
                last TYPE_INDENT;
            }

            if ( $tallLuslusRule->{$lhsName} ) {
                $mistakes = $policy->checkLuslus($node, $lhsName);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'LUSLUS-STYLE';
                last TYPE_INDENT;
            }

            if ( $backdentedRule->{$lhsName} ) {
                $mistakes = $policy->checkBackdented($node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'BACKDENTED';
                last TYPE_INDENT;
            }

            # By default, treat as not yet implemented
            {
                $mistakes = $policy->checkNYI($node);
                last TYPE_INDENT if @{$mistakes};

                # should never reach here
                die 'NYI failure';
            }

        }
    }

  PRINT: {
        if ( @{$mistakes} ) {
	    for my $mistake (@{$mistakes}) {
	      $mistake->{policy} = $policyShortName;
	      $mistake->{subpolicy} = $mistake->{subpolicy} // $instance->diagName($node);
	    }
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

    return;
}

1;
