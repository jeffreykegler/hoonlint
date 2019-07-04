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

my $gapCommentDSL = <<'END_OF_DSL';
:start ::= gapComments
gapComments ::= OptExceptions Body
gapComments ::= OptExceptions
Body ::= InterPart PrePart
Body ::= InterPart
Body ::= PrePart
InterPart ::= InterComponent
InterPart ::= InterruptedInterComponents
InterPart ::= InterruptedInterComponents InterComponent

InterruptedInterComponents ::= InterruptedInterComponent+
InterruptedInterComponent ::= InterComponent Exceptions
InterComponent ::= Staircases
InterComponent ::= Staircases InterComments
InterComponent ::= InterComments

InterComments ::= InterComment+

Staircases ::= Staircase+
Staircase ::= UpperRisers Tread LowerRisers
UpperRisers ::= UpperRiser+
LowerRisers ::= LowerRiser+

PrePart ::= ProperPreComponent OptPreComponents
ProperPreComponent ::= PreComment
OptPreComponents ::= PreComponent*
PreComponent ::= ProperPreComponent
PreComponent ::= Exception

OptExceptions ::= Exception*
Exceptions ::= Exception+
Exception ::= MetaComment
Exception ::= BadComment
Exception ::= BlankLine

unicorn ~ [^\d\D]
BadComment ~ unicorn
BlankLine ~ unicorn
InterComment ~ unicorn
LowerRiser ~ unicorn
MetaComment ~ unicorn
PreComment ~ unicorn
Tread ~ unicorn
UpperRiser ~ unicorn

END_OF_DSL

# Format line and 0-based column as string
sub describeLC {
    my ( $line, $column ) = @_;
    return '@' . $line . ':' . ( $column + 1 );
}

sub describeMisindent {
    my ($difference) = @_;
    if ( $difference > 0 ) {
        return "overindented by $difference";
    }
    if ( $difference < 0 ) {
        return "underindented by " . ( -$difference );
    }
    return "correctly indented";
}

sub describeMisindent2 {
    my ( $got, $sought ) = @_;
    $DB::single = 1 if not defined $sought;
    return describeMisindent( $got - $sought );
}

sub new {
    my ( $class, $lintInstance ) = @_;
    my $policy = {};
    $policy->{lint} = $lintInstance;
    my %chainable = ();
    for my $key (keys %{ $lintInstance->{backdentedRule} }, keys %{ $lintInstance->{tallNoteRule} }) {
       $chainable{$key} = 1;
    }
    $policy->{chainable} = \%chainable;
    Scalar::Util::weaken( $policy->{lint} );
    $policy->{gapGrammar} =
      Marpa::R2::Scanless::G->new( { source => \$gapCommentDSL } );
    return bless $policy, $class;
}

# Return Perl true is node is chainable
sub chainable {
    my ( $policy, $node ) = @_;
    my $chainable = $policy->{chainable};
    my $instance = $policy->{lint};
    my $grammar  = $instance->{grammar};
    my $ruleID   = $node->{ruleID};
    return if not $ruleID;
    my ($lhs)    = $grammar->rule_expand( $node->{ruleID} );
    my $lhsName  = $grammar->symbol_name($lhs);
    # say STDERR join ' ', __FILE__, __LINE__, $lhsName, ($chainable->{$lhsName} // 'na');
    return $chainable->{$lhsName};
}

# Return the node tag for the subpolicy field.
# Archetypally, this is the 6-character form of
# rune for the node's brick.
sub runeName {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $name     = $instance->brickName($node);
    if ( my ($tag) = $name =~ /^optFord([B-Z][aeoiu][b-z][b-z][aeiou][b-z])$/ ) {
        return lc $tag;
    }
    if ( my ($tag) = $name =~ /^ford([B-Z][aeoiu][b-z][b-z][aeiou][b-z])$/ ) {
        return lc $tag;
    }
    if ( my ($tag) = $name =~ /^tall([B-Z][aeoiu][b-z][b-z][aeiou][b-z])$/ ) {
        return lc $tag;
    }
    if ( my ($tag) = $name =~ /^tall([B-Z][aeoiu][b-z][b-z][aeiou][b-z])Mold$/ )
    {
        return lc $tag;
    }
    return lc $name;
}

sub nodeSubpolicy {
    my ( $policy, $node ) = @_;
    return $policy->runeName($node);
}

# return standard anchor "detail" line
sub anchorDetailsBasic {
    my ( $policy, $rune, $anchorColumn ) = @_;
    my $instance = $policy->{lint};
    my ( $runeLine, $runeColumn ) = $instance->nodeLC($rune);
    my $anchorLiteral = $instance->literalLine($runeLine);
    my $anchorLexeme = substr $anchorLiteral, $anchorColumn;
    $anchorLexeme =~ s/[\s].*\z//xms;
    my $typeVerb = ( $anchorColumn == $runeColumn ) ? "anchor" : "re-anchor";
    return [qq{$typeVerb column is }
          . describeLC( $runeLine, $anchorColumn )
          . qq{ "$anchorLexeme"} ];
}

sub anchorDetails {
    my ( $policy, $rune, $anchorData ) = @_;
    my @desc     = ();
    my $instance = $policy->{lint};
    my $brick    = $anchorData->{brick};

    my ( $runeLine,  $runeColumn )  = $instance->nodeLC($rune);
    my ( $brickLine, $brickColumn ) = $instance->nodeLC($brick);
    my $anchorColumn    = $anchorData->{column};
    my $offset          = $anchorData->{offset};
    my $runeLineLiteral = $instance->literalLine($runeLine);
    $runeLineLiteral =~ s/\n\z//xms;

    if ( $anchorColumn == $runeColumn ) {
        my $brickLiteral = $instance->literalLine($runeLine);
        my $brickLexeme = substr $brickLiteral, $brickColumn;
        $brickLexeme =~ s/[\s].*\z//xms;
        return [qq{rune/anchor column is }
              . describeLC( $runeLine, $anchorColumn )
              . qq{ "$brickLexeme"} ];
    }
    push @desc,
      sprintf
're-anchor column (%d) = anchor brick column (%d) + re-anchor offset (%d)',
      $anchorColumn + 1, $brickColumn + 1, $offset;
    my $maxNumWidth    = $instance->maxNumWidth();
    my $pointersPrefix = ( ' ' x $maxNumWidth );
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
    my @gapSeq          = ($child);

    my $childIX = 1;
  CHILD: while ( $childIX < $#$children ) {
        $child = $children->[$childIX];
        my $symbol = $child->{symbol};
        if (   not defined $symbol
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
    my @gapSeq          = ();

    my $childIX = 0;
  CHILD: while ( $childIX < $#$children ) {
        my $child  = $children->[$childIX];
        my $symbol = $child->{symbol};
        if (   not defined $symbol
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
    my $instance   = $policy->{lint};
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
    my ( $policy, $string ) = @_;
    $string =~ s/ ([+][|]|[:][:]|[:][<]|[:][>]) .* \z//xms;
    return $string;
}

# Is this a one-line gap, or its equivalent?
sub isOneLineGap {
    my ( $policy, $gap, $options, $expectedColumn, $expectedColumn2 ) = @_;
    my $instance = $policy->{lint};
    my $start    = $gap->{start};
    my $length   = $gap->{length};
    # say STDERR join " ", __FILE__, __LINE__,  ($options->{subpolicy} ? Data::Dumper::Dumper($options->{subpolicy}) : 'na');
    return i_isOneLineGap( $policy, $options, $start + 2, $length - 2,
        $expectedColumn, $expectedColumn2 )
      if $instance->runeGapNode($gap);
    return i_isOneLineGap( $policy, $options, $start, $length, $expectedColumn,
        $expectedColumn2 );
}

sub checkGapComments {
    my ( $policy, $firstLine, $lastLine, $interOffset, $preOffset ) = @_;

# say STDERR join " ", __FILE__, __LINE__,  $policy, $firstLine, $lastLine, $interOffset, $preOffset;
    return if $lastLine < $firstLine;
    my $instance  = $policy->{lint};
    my $pSource   = $instance->{pHoonSource};
    my $lineToPos = $instance->{lineToPos};
    if ( defined $preOffset and $preOffset == $interOffset ) {
        $preOffset =
          undef;    # Do not allow pre-offset to be equal to inter-offset
    }
    my @mistakes = ();

    my $grammar  = $policy->{gapGrammar};
    my $recce    = Marpa::R2::Scanless::R->new( { grammar => $grammar } );
    my $startPos = $lineToPos->[$firstLine];
    my $input    = $instance->literal( $startPos,
        ( $lineToPos->[ $lastLine + 1 ] - $startPos ) );

# say STDERR join ' ', __FILE__, __LINE__, "$firstLine-$lastLine", qq{"$input"};

    if ( not defined eval { $recce->read( $pSource, $startPos, 0 ); 1 } ) {

        my $eval_error = $EVAL_ERROR;
        chomp $eval_error;
        say STDERR join ' ', __FILE__, __LINE__, "$firstLine-$lastLine",
          qq{"$input"};
        die $eval_error, "\n";
    }

    my $lineNum = 0;
  LINE:
    for ( my $lineNum = $firstLine ; $lineNum <= $lastLine ; $lineNum++ ) {
        my $line = $instance->literalLine($lineNum);

        # say STDERR join ' ', __FILE__, __LINE__, $lineNum, qq{"$line"};

      FIND_ALTERNATIVES: {
            my $expected = $recce->terminals_expected();

            # say Data::Dumper::Dumper($expected);
            my $tier1_ok;
            my @tier2         = ();
            my @failedOffsets = ();
          TIER1: for my $terminal ( @{$expected} ) {

                # say STDERR join ' ', __FILE__, __LINE__, $terminal;
                if ( $terminal eq 'InterComment' ) {
                    $line =~ m/^ [ ]* ([+][|]|[:][:]|[:][<]|[:][>]) /x;
                    my $commentOffset = $LAST_MATCH_START[1];
                    $commentOffset //= -1;

                    # say STDERR join ' ', __FILE__, __LINE__, qq{"$line"};
                    # say STDERR join ' ', __FILE__, __LINE__, $commentOffset;
                    if ( $commentOffset == $interOffset ) {

                        # say STDERR join ' ', __FILE__, __LINE__;
                        $recce->lexeme_alternative( $terminal, $line );
                        $tier1_ok = 1;
                        next TIER1;
                    }
                    push @failedOffsets, $interOffset;
                    next TIER1;
                }
                if ( $terminal eq 'PreComment' ) {
                    next TIER1 if not defined $preOffset;
                    $line =~ m/^ [ ]* ([+][|]|[:][:]|[:][<]|[:][>]) /x;
                    my $commentOffset = $LAST_MATCH_START[1];
                    $commentOffset //= -1;

                    # say STDERR join ' ', __FILE__, __LINE__, $commentOffset;
                    if ( $commentOffset == $preOffset ) {

                        # say STDERR join ' ', __FILE__, __LINE__;
                        $recce->lexeme_alternative( $terminal, $line );
                        $tier1_ok = 1;
                        next TIER1;
                    }
                    push @failedOffsets, $preOffset;
                    next TIER1;
                }
                if ( $terminal eq 'Tread' ) {
                    $line =~ m/^ [ ]* ([:][:][:][:][ \n]) /x;
                    my $commentOffset = $LAST_MATCH_START[1];
                    $commentOffset //= -1;

                    # say STDERR join ' ', __FILE__, __LINE__, $commentOffset;
                    if ( $commentOffset == $interOffset ) {

                        # say STDERR join ' ', __FILE__, __LINE__;
                        $recce->lexeme_alternative( $terminal, $line );
                        $tier1_ok = 1;
                        next TIER1;
                    }
                    push @failedOffsets, $interOffset;
                    next TIER1;
                }
                if ( $terminal eq 'UpperRiser' ) {
                    $line =~ m/^ [ ]* ([:][:]) /x;
                    my $commentOffset = $LAST_MATCH_START[1];
                    $commentOffset //= -1;

                    # say STDERR join ' ', __FILE__, __LINE__, $commentOffset;
                    if ( $commentOffset == $interOffset ) {

                        # say STDERR join ' ', __FILE__, __LINE__;
                        $recce->lexeme_alternative( $terminal, $line );
                        $tier1_ok = 1;
                        next TIER1;
                    }
                    push @failedOffsets, $interOffset;
                    next TIER1;
                }
                if ( $terminal eq 'LowerRiser' ) {
                    $line =~ m/^ [ ]* ([:][:]) /x;
                    my $commentOffset = $LAST_MATCH_START[1];
                    $commentOffset //= -1;

                    # say STDERR join ' ', __FILE__, __LINE__, $commentOffset;
                    if ( $commentOffset == $interOffset + 2 ) {

                        # say STDERR join ' ', __FILE__, __LINE__;
                        $recce->lexeme_alternative( $terminal, $line );
                        $tier1_ok = 1;
                        next TIER1;
                    }
                    push @failedOffsets, $interOffset;
                    next TIER1;
                }
                push @tier2, $terminal;
            }

            # If we found a tier 1 lexeme, do not look for the "backup"
            # lexemes on the other tiers
            last FIND_ALTERNATIVES if $tier1_ok;

            my @tier3 = ();
          TIER2: for my $terminal (@tier2) {
                if ( $terminal eq 'MetaComment' ) {
                    $line =~ m/^ [ ]* ([+][|]|[:][:]|[:][<]|[:][>]) /x;
                    my $commentOffset = $LAST_MATCH_START[1];
                    next TIER2 if not defined $commentOffset;
                    if ( $commentOffset == 0 ) {
                        $recce->lexeme_alternative( $terminal, $line );

                  # anything in this tier terminates the finding of alternatives
                        last FIND_ALTERNATIVES;
                    }
                    push @failedOffsets, $interOffset;
                }
                push @tier3, $terminal;
            }

          TIER3: for my $terminal (@tier3) {
                if ( $terminal eq 'BlankLine' ) {

               # say STDERR join ' ', __FILE__, __LINE__, $lineNum, qq{"$line"};
                    if ( $line =~ m/\A [\n ]* \z/xms ) {
                        $recce->lexeme_alternative( $terminal, $line );

                  # anything in this tier terminates the finding of alternatives
                        push @mistakes, [ 'vgap-blank-line', $lineNum ];
                        last FIND_ALTERNATIVES;
                    }
                }
                if ( $terminal eq 'BadComment' ) {
                    if ( $line =~ m/^ [ ]* ([+][|]|[:][:]|[:][<]|[:][>]) /x ) {
                        $recce->lexeme_alternative( $terminal, $line );
                        my $commentOffset = $LAST_MATCH_START[1];

                        my $closestHiOffset;
                        my $closestLoOffset;

                        for my $failedOffset (@failedOffsets) {
                            if ( $failedOffset > $commentOffset ) {
                                if ( not defined $closestHiOffset
                                    or $failedOffset < $closestHiOffset )
                                {
                                    $closestHiOffset = $failedOffset;
                                }
                            }
                            if ( $failedOffset < $commentOffset ) {
                                if ( not defined $closestLoOffset
                                    or $failedOffset > $closestLoOffset )
                                {
                                    $closestLoOffset = $failedOffset;
                                }
                            }
                        }
                        my $closestOffset =
                          ( $closestLoOffset // $closestHiOffset );

# say STDERR join ' ', __LINE__, 'vgap-bad-comment', $lineNum, $commentOffset, $closestOffset ;
                        push @mistakes,
                          [
                            'vgap-bad-comment', $lineNum,
                            $commentOffset,     $closestOffset
                          ];

                  # anything in this tier terminates the finding of alternatives
                        last FIND_ALTERNATIVES;
                    }
                }
            }

        }
        my $startPos = $lineToPos->[$lineNum];

        # say STDERR join ' ', __FILE__, __LINE__;
        my $eval_ok = eval {
            $recce->lexeme_complete( $startPos,
                ( $lineToPos->[ $lineNum + 1 ] - $startPos ) );
            1;
        };
        if ( not $eval_ok ) {

            my $eval_error = $EVAL_ERROR;
            chomp $eval_error;

            # say STDERR join ' ', __FILE__, __LINE__, "$firstLine-$lastLine",
            # qq{"$input"};
            die $eval_error, "\n";
        }
    }
    my $metric = $recce->ambiguity_metric();
    if ( $metric != 1 ) {
        my $issue = $metric ? "ambiguous" : "no parse";
        say STDERR $recce->show_progress( 0, -1 );
        say STDERR $input;

# say STDERR join " ", __FILE__, __LINE__,  $policy, $firstLine, $lastLine, $interOffset, $preOffset;
        die "Bad gap combinator parse: $issue\n";
    }
    return \@mistakes;
}

sub i_isOneLineGap {
    my ( $policy, $options, $start, $length, $mainColumn, $preColumn ) = @_;
    my $tag       = $options->{tag};
    my @mistakes  = ();
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};
    my $end       = $start + $length;
    my ( $startLine, $startColumn ) = $instance->line_column($start);
    my ( $endLine,   $endColumn )   = $instance->line_column($end);
    $mainColumn //= -1;    # -1 will never match


    my @subpolicyElements = ();
    SET_SUBPOLICY: {
        my $subpolicyArg = $options->{subpolicy};
    # say STDERR join " ", __FILE__, __LINE__;
    # say STDERR Data::Dumper::Dumper($subpolicyArg);
        last SET_SUBPOLICY if not defined $subpolicyArg;
        if ( not ref $subpolicyArg ) {
            push @subpolicyElements, $subpolicyArg;
            last SET_SUBPOLICY;
        }
        push @subpolicyElements, @{$subpolicyArg};
    }

    # say STDERR join " ", __FILE__, __LINE__;
    # say STDERR Data::Dumper::Dumper(\@subpolicyElements);

    # Criss-cross TISTIS lines are a special case
    if (    $startLine == $endLine
        and $instance->literal( $start - 2, 2 ) ne '=='
        and $instance->literal( $start - 2, 2 ) ne '--' )
    {
        return [
            {
                msg => "missing newline "
                  . describeLC( $startLine, $startColumn ),
                subpolicy => (join ':', @subpolicyElements, 'missing-newline'),
                line      => $startLine,
                column    => $startColumn,
            }
        ];
    }

    if ( $startLine + 1 < $#$lineToPos ) {
        my $literalFirstLine = $instance->literalLine( $startLine + 1 );
        if ( $literalFirstLine =~ /'''/ ) {

            # say join ' ', __FILE__, __LINE__, qq{"$literalFirstLine"};
            $startLine++;
        }
        if ( $literalFirstLine =~ /"""/ ) {

            # say join ' ', __FILE__, __LINE__, qq{"$literalFirstLine"};
            $startLine++;
        }
    }
    my $results = $policy->checkGapComments(
        $startLine + 1,
        $endLine - 1,
        $mainColumn, $preColumn
    );
  RESULT: for my $result ( @{$results} ) {
        my $type = $result->[0];
        if ( $type eq 'vgap-blank-line' ) {
            my ( undef, $lineNum, $offset ) = @{$result};
            push @mistakes,
              {
                msg    => "empty line in comment",
                subpolicy => (join ':', @subpolicyElements, 'empty-line'),
                reportLine => $lineNum,
                reportColumn => 0,
                line   => $lineNum,
                column => 0,
              };
            next RESULT;
        }
        if ( $type eq 'vgap-bad-comment' ) {
            my ( undef, $lineNum, $offset, $expectedOffset ) = @{$result};

    # say STDERR join " ", __FILE__, __LINE__;
    # say STDERR Data::Dumper::Dumper(\@subpolicyElements);
            my $desc = "comment";
            push @mistakes,
              {
                msg => "$desc "
                  . describeMisindent2( $offset, $expectedOffset ),
                subpolicy => (join ':', @subpolicyElements, 'comment-indent'),
                line   => $lineNum,
                column => $offset,
              };
        }
    }

    return \@mistakes;
}

sub checkOneLineGap {
    my ( $policy, $gap, $options ) = @_;
    my $instance   = $policy->{lint};
    my @mistakes   = ();
    my $tag        = $options->{tag} or die "No tag";
    my $mainColumn = $options->{mainColumn};
    my $preColumn  = $options->{preColumn};
    my $parent     = $options->{parent} // $gap->{PARENT};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($parent);
    my $topicLines = $options->{topicLines} // [];
    my $details    = $options->{details};
    my $subpolicy  = $options->{subpolicy};

    # say STDERR join " ", __FILE__, __LINE__,  'tag:', ($tag // 'na');
    # say STDERR join " ", __FILE__, __LINE__,  ($subpolicy ? Data::Dumper::Dumper($subpolicy) : 'na');
    if (
        my @gapMistakes = @{
            $policy->isOneLineGap( $gap, { subpolicy => $subpolicy, tag => $tag },
                $mainColumn, $preColumn )
        }
      )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            # TODO: use this as subpolicy in @mistakes
            my $gapMistakeSubpolicy = $gapMistake->{subpolicy};
            # say STDERR join " ", __FILE__, __LINE__;
            # say STDERR Data::Dumper::Dumper($gapMistake);
            # say STDERR Data::Dumper::Dumper($gapMistakeSubpolicy);
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
                reportLine   => $gapMistakeLine,
                reportColumn => $gapMistakeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                topicLines   => $topicLines,
                subpolicy    => $gapMistakeSubpolicy,
                details      => $details,
              };
        }
    }
    return \@mistakes;
}

# Replace all TISTIS logic with this
sub checkTistis {
    my ( $policy, $tistis, $options ) = @_;
    my $expectedColumn = $options->{expectedColumn};
    my $tag            = $options->{tag};
    my $instance       = $policy->{lint};
    my $parent         = $tistis->{PARENT};
    my $subpolicyTag   = $options->{subpolicyTag}
      // $policy->nodeSubpolicy($parent),
      my @mistakes = ();

    my ( $parentLine, $parentColumn ) = $instance->nodeLC($parent);
    my ( $tistisLine, $tistisColumn ) = $instance->nodeLC($tistis);
    my $literalLine = $instance->literalLine($tistisLine);
    $literalLine = $policy->deComment($literalLine);
    $literalLine =~ s/\n//g;
    $literalLine =~ s/==//g;
    if ( $literalLine =~ m/[^ ]/ ) {
        my $msg =
          sprintf q{TISTIS %s should only share line with other TISTIS's},
          describeLC( $tistisLine, $tistisColumn );
        push @mistakes,
          {
            desc         => $msg,
            subpolicy    => $subpolicyTag . ':tistis-alone',
            parentLine   => $parentLine,
            parentColumn => $parentColumn,
            line         => $tistisLine,
            column       => $tistisColumn,
            details      => [ [$tag] ],
          };
        return \@mistakes;
    }

    my $tistisIsMisaligned = $tistisColumn != $expectedColumn;

    if ($tistisIsMisaligned) {
        my $lineToPos     = $instance->{lineToPos};
        my $tistisPos     = $lineToPos->[$tistisLine] + $expectedColumn;
        my $tistisLiteral = $instance->literal( $tistisPos, 2 );

        $tistisIsMisaligned = $tistisLiteral ne '==';
    }
    if ($tistisIsMisaligned) {
        my $msg = sprintf 'TISTIS %s; %s',
          describeLC( $tistisLine, $tistisColumn ),
          describeMisindent2( $tistisColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            subpolicy      => $subpolicyTag . ':tistis-indent',
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $tistisLine,
            column         => $tistisColumn,
            details        => [ [$tag] ],
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
    my @nodesToAlign = ();

    # Traverse first to last to make it easy to record
    # first line of occurrence of each body column
  CHILD:
    for ( my $childIX = $#$children ; $childIX >= 0 ; $childIX-- ) {
        my $attribute = $children->[$childIX];
        my ( undef, $head, $gap, $body ) = @{ $policy->gapSeq0($attribute) };
        my ( $headLine, $headColumn ) = $instance->nodeLC($head);
        my ( $bodyLine, $bodyColumn ) = $instance->nodeLC($body);
        next CHILD if $headLine != $bodyLine;
        push @nodesToAlign, $gap, $body;
    }
    return $policy->findAlignment( \@nodesToAlign );
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
    my ($expectedBodyColumn, $expectBodyColumnDetails) = @{$policy->sailAttributeBodyAlignment($attributes)};

    my @mistakes = ();
    my $tag      = 'sail atttribute';

    # We deal with the elements list in its own node

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $headGap,
            {
                mainColumn => $expectedHeadColumn,
                tag        => $tag,
                subpolicy => [ 'sail attribute' ],
                topicLines => [$headLine],
            }
        )
      };

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
      CHECK_GAP: {
            last CHECK_GAP if $bodyGapLength == 2;
            if ( $expectedBodyColumn < 0 ) {
                my $msg = sprintf
                  "Sail attribute body %s; %s",
                  describeLC( $bodyLine, $bodyColumn ),
                  describeMisindent2( $bodyGapLength, 2 );
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $sailApexLine,
                    parentColumn => $sailApexColumn,
                    line         => $bodyLine,
                    column       => $bodyColumn,
                    topicLines   => [$bodyLine],
                  };
                last CHECK_GAP;
            }
            if ( $bodyColumn != $expectedBodyColumn ) {
                my $msg = sprintf
                  "Sail attribute body %s; %s",
                  describeLC( $bodyLine, $bodyColumn ),
                  describeMisindent2( $bodyColumn, $expectedBodyColumn );
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $sailApexLine,
                    parentColumn => $sailApexColumn,
                    line         => $bodyLine,
                    column       => $bodyColumn,
                    topicLines   => [$bodyLine],
                  };
            }
        }
    }

    return \@mistakes;
}

sub checkTailOfElem {
    my ( $policy, $node ) = @_;
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my ( $tistisGap, $tistis ) = @{ $policy->gapSeq0($node) };

    my $tallTopSail = $instance->ancestor( $node, 2 );
    my ( $tallTopSailLine, $tallTopSailColumn ) =
      $instance->nodeLC($tallTopSail);
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $tistisLine, $tistisColumn ) = $instance->nodeLC($tistis);

    # There is always a SEM before <tallTopSail> and this is our
    # anchor column
    my $expectedColumn = $tallTopSailColumn - 1;

    my @mistakes = ();
    my $tag      = 'tail of elem';

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $tistisGap,
            {
                mainColumn => $expectedColumn,
                tag        => $tag,
                subpolicy => [ $tag ],
                topicLines => [$tistisLine],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
                tag            => $tag,
                expectedColumn => $expectedColumn,
            }
        )
      };

    return \@mistakes;
}

sub checkTailOfTop {
    my ( $policy, $node ) = @_;
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my ( $tistisGap, $tistis ) = @{ $policy->gapSeq0($node) };

    my $tallTopSail = $instance->ancestor( $node, 2 );
    my ( $tallTopSailLine, $tallTopSailColumn ) =
      $instance->nodeLC($tallTopSail);
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $tistisLine, $tistisColumn ) = $instance->nodeLC($tistis);

    my $expectedColumn = $tallTopSailColumn;

    my @mistakes = ();
    my $tag      = 'tail of top';

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $tistisGap,
            {
                mainColumn => $expectedColumn,
                tag        => $tag,
                subpolicy => [ $tag ],
                topicLines => [$tistisLine],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
                tag            => $tag,
                expectedColumn => $expectedColumn,
            }
        )
      };

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
    my ( $anchorLine, $anchorColumn ) = $instance->nodeLC($anchor);

    my @mistakes = ();
    my $tag      = 'bont';

  BODY_ISSUES: {
        if ( $parentLine == $bodyLine ) {
            my $msg =
              sprintf
              'SIGGAR/SIGGAL element 2 %s; element must not be on rune line',
              describeLC( $bodyLine, $bodyColumn );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $bodyLine,
                column       => $bodyColumn,
                details      => [ [$tag] ],
              };
            last BODY_ISSUES;
        }

        # If here parent line != body line
        my $expectedBodyColumn = $anchorColumn + 2;
        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $gap,
                {
                    mainColumn => $expectedBodyColumn,
                    tag        => $tag,
                subpolicy => [ $tag ],
                    details    => [ [$tag] ],
                }
            )
          };

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
                details        => [ [$tag] ],
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
    my $tag      = 'bonz element';

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
                details        => [ [$tag] ],
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
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $bodyLine,
            column         => $bodyColumn,
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
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $bodyLine,
            column         => $bodyColumn,
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
    my ( $policy, $options ) = @_;
    my $instance        = $policy->{lint};
    my $runningChildren = $options->{children};
    my $tag             = $options->{tag} or die "No tag";
    my $anchorColumn    = $options->{anchorColumn};
    my $expectedColumn  = $options->{expectedColumn};

    # by default, in fact always at this point, the running can be
    # found as the parent of the last running child, and the parent
    # can be found as the parent
    my $running = $runningChildren->[-1]->{PARENT};
    my $parent  = $running->{PARENT};

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($parent);
    my ( $runningLine, $runningColumn ) = $instance->nodeLC($running);

    my $anchorDetails = $options->{anchorDetails}
      // $policy->anchorDetailsBasic( $parent, $anchorColumn );

    my $runeName = $policy->runeName($parent);
    my @mistakes         = ();

    my $skipFirst = $options->{skipFirst};
    my $childIX = $skipFirst ? 3 : 2;

    # Call an column of runsteps with the same row
    # position a "pile" because "column" and the other terms are
    # too overloaded.
    my @runStepsToAlignByPile = ();
  RUNNING_LINE: while (1) {

        # The index into the runstep alignment array.
        # The alignment of the 2nd runstep
        # in a row (or line) is at index 0.
        my $pileIX = 0;
      RUNSTEP: while (1) {
        # say STDERR join " ", __FILE__, __LINE__, $childIX, $pileIX;
            last RUNNING_LINE if $childIX + 1 > $#$runningChildren;
            my $gap       = $runningChildren->[$childIX];
            my $runStep   = $runningChildren->[ $childIX + 1 ];
            my ($gapLine) = $instance->nodeLC($gap);
            my ( $runStepLine, $runStepColumn ) = $instance->nodeLC($runStep);
        # say STDERR sprintf q{Gap: "%s"}, $instance->literalNode($gap);
        # say STDERR sprintf q{Runstep: "%s"}, $instance->literalNode($runStep);
        # say STDERR join " ", __FILE__, __LINE__, "gapline vs. run step line", $gapLine, $runStepLine;
            last RUNSTEP if $gapLine != $runStepLine;

            # Uses Perl's autoinstantiation
            push @{ $runStepsToAlignByPile[$pileIX] }, $gap, $runStep;
            $pileIX += 1;
            $childIX += 2;
        }

        # In this loop, childIX always points to a gap
        $childIX += 2;
    }

    my @pileAlignments = ();
  ELEMENT:
    for ( my $pileIX = 0 ; $pileIX <= $#runStepsToAlignByPile ; $pileIX++ ) {
        my $runStepsToAlign = $runStepsToAlignByPile[$pileIX];
        if ( not $runStepsToAlign ) {
            $pileAlignments[$pileIX] = [ -1, [] ];
            next ELEMENT;
        }
        $pileAlignments[$pileIX] =
          $policy->findAlignment($runStepsToAlign);
    }

    # If there are no alignments by runstep, then we check for
    # alignments among the running's children.
        my @nodesToAlignByElement;
  RUNNING_CHILD_ALIGNMENTS: {
        last RUNNING_CHILD_ALIGNMENTS if scalar @pileAlignments;
        my @bricks;
      RUNSTEP:
        for (
            my $runStepIX = ( $skipFirst ? 2 : 1 ) ;
            $runStepIX <= $#$runningChildren ;
            $runStepIX += 2
          )
        {
            my $runStep = $runningChildren->[$runStepIX];

            # say STDERR "Child $runStepIX: ", $instance->literalNode($runStep);
            my $brickDescendant = $instance->brickDescendant($runStep);
            last RUNSTEP if not $brickDescendant;

            # say STDERR "Brick $runStepIX: ",
              $instance->literalNode($brickDescendant);
            next RUNSTEP if not $policy->chainable($brickDescendant);

            push @bricks, $brickDescendant;
            # say STDERR "Chainable Child $runStepIX: ",
              $instance->literalNode($brickDescendant);
            my $gapSeq = $policy->gapSeq0($brickDescendant);
          BRICK_ELEMENT: for ( my $elementIX = 0 ; ; $elementIX++ ) {
                # say STDERR join " ", __FILE__, __LINE__;
                my $seqIX = $elementIX * 2;
                last BRICK_ELEMENT if $seqIX >= $#$gapSeq;
                my $gap     = $gapSeq->[$seqIX];
                my $element = $gapSeq->[ $seqIX + 1 ];
                # say STDERR join " ", __FILE__, __LINE__;
                push @{ $nodesToAlignByElement[$elementIX] }, $gap, $element;
            }
            # say STDERR join " ", __FILE__, __LINE__;
        }

        last RUNNING_CHILD_ALIGNMENTS unless scalar @nodesToAlignByElement;

        # say STDERR join " ", __FILE__, __LINE__;
        my @runStepChildAlignments = ();
        # say STDERR join " ", __FILE__, __LINE__;
      ELEMENT:
        for (
            my $elementIX = 0 ;
            $elementIX <= $#nodesToAlignByElement ;
            $elementIX++
          )
        {
            # say STDERR join " ", __FILE__, __LINE__;
            my $nodesToAlign = $nodesToAlignByElement[$elementIX];
            if ( not $nodesToAlign ) {
                $runStepChildAlignments[$elementIX] = [ -1, [] ];
                next ELEMENT;
            }
            $runStepChildAlignments[$elementIX] =
              $policy->findAlignment($nodesToAlign);
        }


        for my $brick (@bricks) {
            my $brickNodeIX = $brick->{IX};
            $policy->{perNode}->{$brickNodeIX}->{runningAlignments} =
             \@runStepChildAlignments;
        }

    }

    $childIX = 0;
    # Do the first run step
    my $gap          = $runningChildren->[$childIX];
    my ( $gapLine, $gapColumn ) = $instance->nodeLC($gap);
    my $runStep = $runningChildren->[ $childIX + 1 ];
    my ( $thisRunStepLine, $runStepColumn ) = $instance->nodeLC($runStep);

  my $runStepCount = 1;
  CHECK_FIRST_RUNNING: {
        last CHECK_FIRST_RUNNING if $skipFirst;
        last CHECK_FIRST_RUNNING if $runStepColumn == $expectedColumn;
        my $msg = sprintf
          "runstep #%d %s; %s",
          $runStepCount,
          describeLC( $thisRunStepLine, $runStepColumn ),
          describeMisindent2( $runStepColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $thisRunStepLine,
            parentColumn   => $runStepColumn,
            line           => $thisRunStepLine,
            column         => $runStepColumn,
            topicLines     => [$runeLine],
            details        => [ [ $tag, @{$anchorDetails} ] ],
          };
    }

    my $workingRunStepLine = $thisRunStepLine;

    # Initial runsteps may be on a single line,
    # separated by one stop
    $childIX = 2;
    $runStepCount = 2;
  RUN_STEP: while ( $childIX < $#$runningChildren ) {

    my ( $thisRunStepLine, $runStepColumn ) ;

      INLINE_RUN_STEP: while (1) {
            if ( $childIX >= $#$runningChildren ) {
                last RUN_STEP;
            }
            $gap = $runningChildren->[$childIX];
            ( $gapLine, $gapColumn ) = $instance->nodeLC($gap);
            $runStep = $runningChildren->[ $childIX + 1 ];
            ( $thisRunStepLine, $runStepColumn ) = $instance->nodeLC($runStep);
            if ( $thisRunStepLine != $workingRunStepLine ) {
                last INLINE_RUN_STEP;
            }
            CHECK_COLUMN: {
            my $tightColumn = $gapColumn + 2;
            last CHECK_COLUMN if $runStepColumn == $tightColumn;
            my @allowedColumns =([ $tightColumn => 'tight' ]);

            my ( $pileAlignmentColumn, $pileAlignmentLines );
            my $thisAlignment = $pileAlignments[ $runStepCount - 2 ];
            if ($thisAlignment) {
                ( $pileAlignmentColumn, $pileAlignmentLines ) = @{$thisAlignment};
                last CHECK_COLUMN if $pileAlignmentColumn > $tightColumn
                  and $pileAlignmentColumn == $runStepColumn;
            }

            my $details;
            my @topicLines = ();
            # say STDERR qq{$pileAlignmentColumn and $pileAlignmentColumn >= $tightColumn};
            if (defined $pileAlignmentColumn and $pileAlignmentColumn >= $tightColumn) {
                push @allowedColumns, [ $pileAlignmentColumn => 'runstep' ];
                my $oneBasedColumn = $pileAlignmentColumn + 1;
                my $pileAlignmentLines = $pileAlignmentLines;
                push @topicLines, @{$pileAlignmentLines};
                $details = [
                    [
                        $tag,
                        sprintf 'runstep alignment is %d, see %s',
                        $oneBasedColumn,
                        (
                            join q{ },
                            map { $_ . ':' . $oneBasedColumn }
                              @{$pileAlignmentLines}
                        )
                    ]
                ];
            }
            else {
                $details = [ [ $tag, "no runstep alignment detected" ] ];
            }

            my @sortedColumns = sort { $a->[0] <=> $b->[0] } @allowedColumns;
            my $allowedDesc = join "; ",
              map { sprintf '@%d:%d (%s)', $thisRunStepLine, $_->[0]+1, $_->[1] } @sortedColumns;
            if (scalar @sortedColumns >= 2) {
               $allowedDesc = 'one of ' . $allowedDesc;
            }

            my $msg = sprintf
              'runstep #%d of running %s, line %d is at %s; should be %s',
              $runStepCount,
              describeLC( $runeLine, $runeColumn ),
              $thisRunStepLine,
              describeLC( $thisRunStepLine, $runStepColumn ),
              $allowedDesc;
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $runeLine,
                parentColumn   => $runeColumn,
                line           => $thisRunStepLine,
                column         => $runStepColumn,
                reportLine     => $thisRunStepLine,
                reportColumn   => $runStepColumn,
                subpolicy      => $policy->nodeSubpolicy($parent) . ':hgap',
                topicLines => \@topicLines,
                details => $details,
              };
            }
            $childIX += 2;
            $runStepCount++;
        }

        $workingRunStepLine = $thisRunStepLine;

        # If the run step is mis-indented, complaints about the comments are
        # misleading and confusing.  Skip them.
        # TODO: Complain about blank lines anyway ?
        if ( $runStepColumn == $expectedColumn ) {
            push @mistakes,
              @{
                $policy->checkOneLineGap(
                    $gap,
                    {
                        mainColumn => $anchorColumn,
                        preColumn  => $runStepColumn,
                        tag =>
                          ( sprintf 'runstep #%d', int( 1 + $childIX / 2 ) ),
                        subpolicy  => [ $runeName ],
                        parent     => $runStep,
                        topicLines => [$runeLine],
                        details    => [
                            [
                                $tag,
                                'inter-comment indent should be '
                                  . ( $anchorColumn + 1 ),
                                'pre-comment indent should be '
                                  . ( $expectedColumn + 1 ),
                                @{$anchorDetails},
                            ]
                        ],
                    }
                )
              };
        }

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
                topicLines     => [$runeLine],
                details        => [ [ $tag, @{$anchorDetails} ] ],
              };
        }

        $childIX += 2;
        $runStepCount = 2;

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
    return checkJoined_0Running( $policy, $node, $column )
      if $column == $runeColumn + 4;
    return checkSplit_0Running( $policy, $node );
}

# Find the cell body column, based on alignment within
# a parent hoon.
sub cellBodyColumn {
    my ( $policy, $node ) = @_;
    my $instance       = $policy->{lint};
    my $nodeIX         = $node->{IX};
    my $cellBodyData = $policy->{perNode}->{$nodeIX}->{cellBodyData};
    return $cellBodyData if $cellBodyData;

  FIND_CELL_BODY_COLUMN: {
        my $instance = $policy->{lint};
        my $lhsName  = $instance->lhsName($node);
        if ( $lhsName and $lhsName eq 'whap5d' ) {
            $cellBodyData = $policy->whapCellBodyAlignment($node);
            last FIND_CELL_BODY_COLUMN;
        }
        $cellBodyData = $policy->cellBodyColumn( $node->{PARENT} );
    }
    $policy->{perNode}->{$nodeIX}->{cellBodyData} = $cellBodyData;
    return $cellBodyData;
}

# assumes this is a <whap5d> node
sub whapCellBodyAlignment {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $children = $node->{children};
    my @nodesToAlign = ();

  CHILD:
    for ( my $childIX = 0 ; $childIX <= $#$children ; $childIX += 2 ) {
        my $boog = $children->[$childIX];
        my $cell = $boog->{children}->[0];
        my ( undef, $head, $gap, $body ) = @{ $policy->gapSeq0($cell) };
        my ( $headLine ) = $instance->nodeLC($head);
        my ( $bodyLine ) = $instance->nodeLC($body);
        next CHILD unless $headLine == $bodyLine;
        # say STDERR sprintf q{Gap: "%s"}, $instance->literalNode($gap);
        # say STDERR sprintf q{Body: "%s"}, $instance->literalNode($body);
        push @nodesToAlign, $gap, $body;
    }
    return $policy->findAlignment( \@nodesToAlign );
}

sub checkWhap5d {
    my ( $policy, $node ) = @_;
    my $gapSeq           = $policy->gapSeq($node);
    my $instance         = $policy->{lint};
    my $censusWhitespace = $instance->{censusWhitespace};

    my @mistakes = ();
    my $tag      = 'whap';

    my $anchorNode = $instance->firstBrickOfLine($node);
    my ( $anchorLine, $anchorColumn ) = $instance->nodeLC($anchorNode);
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);

    # The battery is "joined" iff it starts on the same line as the anchor,
    # but at a different column.  "Different column" to catch the case where
    # the anchor rune *is* the battery rune.
    my $joined =
      ( $anchorLine == $parentLine and $anchorColumn != $parentColumn );
    my $children       = $node->{children};
    my $childIX        = 0;
    my $expectedColumn = $joined ? $parentColumn : $anchorColumn;
    my $expectedLine   = $joined ? $parentLine : $anchorLine + 1;

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
                topicLines     => [ $parentLine, $expectedLine ],
                details        => [ [$tag] ],
              };
        }

        $childIX++;
        last CHILD unless $childIX <= $#$children;
        my $boogGap = $children->[$childIX];
        my ( $boogGapLine, $boogGapColumn ) = $instance->nodeLC($boogGap);

        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $boogGap,
                {
                    mainColumn => $expectedColumn,
                    tag        => $tag,
                    subpolicy => ['whap5d'],
                    details    => [ [$tag] ],
                    topicLines => [ $parentLine, $boogGapLine ],
                }
            )
          };

        $childIX++;
    }

    return \@mistakes;

}

sub checkWisp5d {
    my ( $policy, $node ) = @_;
    my @mistakes = ();
    my $tag      = 'wisp';
    my $instance = $policy->{lint};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);

    my $battery =
      $instance->ancestorByLHS( $node,
        { tallBarcab => 1, tallBarcen => 1, tallBarket => 1 } );
    my ( $batteryLine, $batteryColumn ) = $instance->nodeLC($battery);
    my $batteryNodeIX = $battery->{IX};
    my $anchorColumn  = $policy->{perNode}->{$batteryNodeIX}->{anchorColumn};
    $anchorColumn //= $batteryColumn;

    my $gapSeq = $policy->gapSeq0($node);
    my ( $gap, $hephep ) = @{$gapSeq};

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $gap,
            {
                mainColumn => $parentColumn,
                tag        => $tag,
                    subpolicy => ['wisp5d'],
                topicLines => [$batteryLine],
                details    => [ [$tag] ],
            }
        )
      };

    my ( $hephepLine, $hephepColumn ) = $instance->nodeLC($hephep);

    {
        my $literalLine = $instance->literalLine($hephepLine);
        $literalLine = $policy->deComment($literalLine);
        $literalLine =~ s/\n//g;
        $literalLine =~ s/--//g;
        if ( $literalLine =~ m/[^ ]/ ) {
            my $msg =
              sprintf q{HEPHEP %s should only share line with other HEPHEP's},
              describeLC( $hephepLine, $hephepColumn );
            push @mistakes,
              {
                desc      => $msg,
                subpolicy => $policy->nodeSubpolicy($battery) . ':hephep-alone',
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $hephepLine,
                column       => $hephepColumn,
                details      => [ [$tag] ],
              };
        }
    }

    my $expectedColumn     = $anchorColumn;
    my $hephepIsMisaligned = $hephepColumn != $expectedColumn;

    if ($hephepIsMisaligned) {
        my $lineToPos     = $instance->{lineToPos};
        my $hephepPos     = $lineToPos->[$hephepLine] + $expectedColumn;
        my $hephepLiteral = $instance->literal( $hephepPos, 2 );
        $hephepIsMisaligned = $hephepLiteral ne '--';
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
            topicLines     => [$batteryLine],
            details        => [ [$tag] ],
          };
    }
    return \@mistakes;
}

sub checkSplitFascom {
    my ( $policy, $node ) = @_;
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    # say STDERR join " ", __FILE__, __LINE__, $instance->literalNode($node);
    my ( $bodyGap, $body, $tistisGap, $tistis ) =
      @{ $policy->gapSeq0($node) };

    my ( $runeLine,   $runeColumn )   = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);
    my ( $tistisLine, $tistisColumn ) = $instance->nodeLC($tistis);

    my $anchorLine = $runeLine;
    my ( $anchorColumn, $anchorData ) = $policy->reanchorInc(
        $node,
        {
            fordFascen => 1,
        }
    );
    my $anchorDetails = $policy->anchorDetails( $node, $anchorData );

    my @mistakes = ();
    my $runeName = $policy->runeName($node);
    my $tag      = $runeName;

    # We deal with the elements list itself,
    # in its own node

    my $expectedColumn = $anchorColumn + 2;
    my $expectedLine   = $runeLine + 1;

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $bodyGap,
            {
                mainColumn => $anchorColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                details    => [ [$tag] ],
                topicLines => [$bodyLine],
            }
        )
      };

    if ( $bodyColumn != $expectedColumn ) {
        my $msg = sprintf
          "split %s %s; %s",
          $runeName,
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent2( $bodyColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            subpolicy => [ $runeName, 'body-indent' ],
            parentLine     => $runeLine,
            parentColumn   => $runeColumn,
            line           => $bodyLine,
            column         => $bodyColumn,
            reportLine           => $bodyLine,
            reportColumn         => $bodyColumn,
            topicLines     => [ $runeLine, $expectedLine ],
            details        => [ [$tag, @{$anchorDetails} ] ],
          };
    }

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $tistisGap,
            {
                mainColumn => $anchorColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                topicLines => [ $anchorLine, $tistisLine ],
                details        => [ [$tag, @{$anchorDetails} ] ],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
                tag            => $tag,
                expectedColumn => $anchorColumn,
            }
        )
      };

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
    my $runeName = $policy->runeName($node);
    my $tag      = $runeName;

    # We deal with the elements list in its own node

    my $expectedColumn = $runeColumn + 4;
    my $expectedLine   = $runeLine + 1;

    if ( $bodyColumn != $expectedColumn ) {
        my $msg = sprintf
          "joined %s %s; %s",
          $runeName,
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent2( $bodyColumn, $expectedColumn );
        push @mistakes,
          {
            desc         => $msg,
            subpolicy    => [ $runeName, 'body-indent' ],
            parentLine   => $runeLine,
            parentColumn => $runeColumn,
            line         => $bodyLine,
            column       => $bodyColumn,
            reportLine   => $bodyLine,
            reportColumn => $bodyColumn,
            topicLines   => [ $runeLine, $expectedLine ],
            details      => [ [$tag] ],
          };
    }

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $tistisGap,
            {
                mainColumn => $runeColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                topicLines => [ $runeLine, $tistisLine ],
                details    => [ [$tag] ],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
                tag            => $tag,
                expectedColumn => $runeColumn,
            }
        )
      };

    return \@mistakes;
}

sub checkFascom {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    # say STDERR join " ", __FILE__, __LINE__, $instance->literalNode($node);
    my ( undef, $elements ) = @{ $policy->gapSeq0($node) };

    my ($runeLine)     = $instance->nodeLC($node);
    my ($elementsLine) = $instance->nodeLC($elements);

    my $isJoined = $elementsLine == $runeLine ? 1 : 0;

    my $nodeIX = $node->{IX};
    $policy->{perNode}->{$nodeIX}->{isJoined} = $isJoined;

    return checkJoinedFascom( $policy, $node ) if $isJoined;
    return checkSplitFascom( $policy, $node );
}

sub checkFascomElements {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $children = $node->{children};

    my $rune = $instance->ancestorByBrickName( $node, 'fordFascom' );
    my ( $runeLine,   $runeColumn )   = $instance->nodeLC($rune);
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);

    my @mistakes = ();
    my $tag      = 'fascom elements';

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
                subpolicy => [ 'fascom-element', 'indent' ],
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $elementLine,
                column         => $elementColumn,
                topicLines     => [$runeLine],
              };
        }

        $childIX++;
        last CHILD unless $childIX <= $#$children;
        my $elementGap = $children->[$childIX];
        my ( $elementGapLine, $elementGapColumn ) =
          $instance->nodeLC($elementGap);

        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $elementGap,
                {
                    mainColumn => $runeColumn,
                    tag        => $tag,
                subpolicy => [ $tag ],
                    topicLines => [$runeLine],
                    details    => [ [$tag] ],
                }
            )
          };

        $childIX++;
    }

    return \@mistakes;
}

sub checkFasdot {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    my ( $bodyGap, $body, $tistisGap, $tistis ) = @{ $policy->gapSeq0($node) };

    my ( $runeLine,   $runeColumn )   = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);
    my ( $tistisLine, $tistisColumn ) = $instance->nodeLC($tistis);

    my @mistakes = ();
    my $runeName = $policy->runeName($node);
    my $tag      = $runeName;

    # We deal with the elements list in its own node

    my $expectedColumn = $runeColumn + 4;

  CHECK_BODY: {
        if ( $bodyLine != $runeLine ) {
            my $msg = sprintf
              "%s %s; body must be on rune line",
              $runeName, describeLC( $bodyLine, $bodyColumn );
            push @mistakes,
              {
                desc         => $msg,
                subpolicy    => [ $runeName, 'body-split' ],
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $bodyLine,
                column       => $bodyColumn,
                reportLine   => $bodyLine,
                reportColumn => $bodyColumn,
                topicLines   => [$runeLine],
                details      => [ [$tag] ],
              };
            last CHECK_BODY;
        }

        if ( $bodyColumn != $expectedColumn ) {
            my $msg = sprintf
              "joined %s %s; %s",
              $runeName,
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $bodyColumn, $expectedColumn );
            push @mistakes,
              {
                desc         => $msg,
                subpolicy    => [ $runeName, 'body-indent' ],
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $bodyLine,
                column       => $bodyColumn,
                reportLine   => $bodyLine,
                reportColumn => $bodyColumn,
                topicLines   => [$runeLine],
                details      => [ [$tag] ],
              };
        }
    }

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $tistisGap,
            {
                mainColumn => $runeColumn,
                tag        => $tag,
                subpolicy  => [$runeName],
                topicLines => [ $runeLine, $tistisLine ],
                details    => [ [$tag] ],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
                tag            => $tag,
                expectedColumn => $runeColumn,
            }
        )
      };

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
              };
        }

        $childIX++;
        last CHILD unless $childIX <= $#$children;
        my $elementGap = $children->[$childIX];
        my ( $elementGapLine, $elementGapColumn ) =
          $instance->nodeLC($elementGap);

        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $elementGap,
                {
                    mainColumn => $expectedColumn,
                    tag        => $tag,
                subpolicy => [ $tag, 'sequence' ],
                    details    => [ [$tag] ],
                    topicLines => [$elementGapLine],
                }
            )
          };

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
      @{ $node->{children} };
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my $anchorNode = $node;
    my ( $anchorLine,  $anchorColumn )  = $instance->nodeLC($anchorNode);
    my ( $headLine,    $headColumn )    = $instance->nodeLC($head);
    my ( $batteryLine, $batteryColumn ) = $instance->nodeLC($battery);

    my @mistakes = ();
    my $runeName      = 'barcab';
    my $tag      = 'barcab';

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
          };

    }

    $expectedColumn = $anchorColumn;
    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $batteryGap,
            {
                mainColumn => $expectedColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                details    => [ [$tag] ],
                topicLines => [$batteryLine],
            }
        )
      };

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
          };
        return \@mistakes;
    }

    return \@mistakes;
}

sub checkBarcen {
    my ( $policy, $node )    = @_;
    my ( $gap,    $battery ) = @{ $policy->gapSeq0($node) };
    my $instance = $policy->{lint};
    my ( $parentLine,   $parentColumn ) = $instance->nodeLC($node);
    my ( $anchorColumn, $anchorData )   = $policy->reanchorInc(
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
    my $anchorDetails = $policy->anchorDetails( $node, $anchorData );

    my $batteryNodeIX = $battery->{IX};
    $policy->{perNode}->{$batteryNodeIX}->{anchorColumn} = $anchorColumn;

    my ( $batteryLine, $batteryColumn ) = $instance->nodeLC($battery);

    my @mistakes = ();
    my $runeName      = 'barcen';
    my $tag      = 'barcen';

    my $gapLiteral = $instance->literalNode($gap);
    my $expectedColumn;

    if ( $parentLine == $batteryLine ) {
        my $gapLength = $gap->{length};
        return [] if length $gapLiteral == 2;

        my $msg = sprintf 'joined Barcen battery %s; %s',
          describeLC( $batteryLine, $batteryColumn ),
          describeMisindent2( $gapLength, 2 );
        push @mistakes,
          {
            desc         => $msg,
            parentLine   => $parentLine,
            parentColumn => $parentColumn,
            line         => $batteryLine,
            column       => $batteryColumn,
          };
        return \@mistakes;
    }

    # If here head line != battery line
    $expectedColumn = $anchorColumn;
    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $gap,
            {
                mainColumn => $expectedColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                details    => [
                    [ $tag, @{ $policy->anchorDetails( $node, $anchorData ) } ]
                ],
            }
        )
      };

    if ( $batteryColumn != $expectedColumn ) {
        my $msg = sprintf 'split Barcen battery %s; %s',
          describeLC( $batteryLine, $batteryColumn ),
          describeMisindent2( $batteryColumn, $expectedColumn );
        push @mistakes,
          {
            desc         => $msg,
            parentLine   => $parentLine,
            parentColumn => $parentColumn,
            line         => $batteryLine,
            column       => $batteryColumn,
            anchorDetails  => $policy->anchorDetails( $node, $anchorData ),
          };
        return \@mistakes;
    }

    return \@mistakes;
}

sub checkBarket {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    # TODO: reanchoring logic, memoize anchorColumn for checkWisp5d()

    my ( $headGap, $head, $batteryGap, $battery ) =
      @{ $policy->gapSeq0($node) };
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my $anchorNode = $node;
    my ( $anchorLine,  $anchorColumn )  = $instance->nodeLC($anchorNode);
    my ( $headLine,    $headColumn )    = $instance->nodeLC($head);
    my ( $batteryLine, $batteryColumn ) = $instance->nodeLC($battery);

    my @mistakes = ();
    my $tag      = 'barket';
    my $runeName      = 'barket';

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
                    details        => [ [$tag] ],
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
                    details        => [ [$tag] ],
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
            details        => [ [$tag] ],
          };

    }

    $expectedColumn = $anchorColumn;
    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $batteryGap,
            {
                mainColumn => $expectedColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                details    => [ [$tag] ],
                topicLines => [$batteryLine],
            }
        )
      };

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
          };
        return \@mistakes;
    }

    return \@mistakes;
}

sub checkFordHoofRune {
    my ( $policy, $lhsName, $node ) = @_;
    my $instance = $policy->{lint};

    # FASWUT is very similar to these runes.  Combine them?

    # Ford hoof runes is special, so we need to find the components using low-level
    # techniques.
    # optFordFashep ::= (- FAS HEP GAP -) fordHoofSeq (- GAP -)
    my ( undef, undef, $leaderGap, $body, $trailerGap ) =
      @{ $node->{children} };

    my $runeName = $policy->runeName($node);

    # TODO: Should we require that parent column be 0?
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);
    my ( $leaderGapLine,   $leaderGapColumn )   = $instance->nodeLC($leaderGap);

    my @mistakes = ();
    my $tag      = $runeName;

    my $expectedColumn;

  BODY_ISSUES: {
        if ( $parentLine != $bodyLine ) {
            my $msg = sprintf '%s body %s; must be on rune line',
              $runeName,
              describeLC( $bodyLine, $bodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                reportLine           => $bodyLine,
                reportColumn         => $bodyColumn,
                subpolicy => [ $runeName, 'same-line' ],
              };
            last BODY_ISSUES;
        }
        my $expectedBodyColumn = $parentColumn + 4;
        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg =
              sprintf '%s body %s is %s',
              $runeName,
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                reportLine           => $bodyLine,
                reportColumn         => $bodyColumn,
                subpolicy => [ $runeName, 'hgap' ],
              };
        }
        last BODY_ISSUES;

    }

    $expectedColumn = $parentColumn;
    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $trailerGap,
            {
                mainColumn => $expectedColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                details    => [ [$tag] ],
            }
        )
      };

    return \@mistakes;
}

sub checkFordHoop {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    # TODO Split is implemented, but not split Ford-1 hoon
    # is represented in the corpus AFAICT

    my $tag = 'fordHoop';

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
                subpolicy => [ $tag, 'hgap' ],
                    parentLine     => $parentLine,
                    parentColumn   => $parentColumn,
                    line           => $bodyLine,
                    column         => $bodyColumn,
                  };
            }
            last BODY_ISSUES;
        }

        # If here parent line != body line
        $expectedBodyColumn = $parentColumn;
        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $gap,
                {
                    mainColumn => $expectedBodyColumn,
                    tag        => $tag,
                subpolicy => [ $tag ],
                    details    => [ [$tag] ],
                }
            )
          };

        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg =
              sprintf 'split %s body %s is %s',
              $tag,
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                subpolicy => [ $tag, 'body-indent' ],
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
              };
        }
    }

    return \@mistakes;
}

sub checkFord_1 {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    # TODO Split is implemented, but not split Ford-1 hoon
    # is represented in the corpus AFAICT

    my $runeName = $policy->runeName($node);

    # fordFassig ::= (- FAS SIG GAP -) tall5d
    my ( $gap, $body ) = @{ $policy->gapSeq0($node) };

    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);

    my ( $anchorColumn, $anchorData ) = $policy->reanchorInc(
        $node,
        {
            fordFascen => 1,
        }
    );
    my $anchorDetails = $policy->anchorDetails( $node, $anchorData );

    my @mistakes = ();

    my $expectedBodyColumn;

  BODY_ISSUES: {
        if ( $parentLine == $bodyLine ) {
            my $expectedBodyColumn = $anchorColumn + 4;
            if ( $bodyColumn != $expectedBodyColumn ) {
                my $msg =
                  sprintf 'joined %s body %s is %s',
                  $runeName,
                  describeLC( $bodyLine, $bodyColumn ),
                  describeMisindent2( $bodyColumn, $expectedBodyColumn );
                push @mistakes,
                  {
                    desc         => $msg,
                    subpolicy    => [ $runeName, 'hgap' ],
                    parentLine   => $parentLine,
                    parentColumn => $parentColumn,
                    reportLine   => $bodyLine,
                    reportColumn => $bodyColumn,
                    line         => $bodyLine,
                    column       => $bodyColumn,
                    details      => [ [ @{$anchorDetails} ] ],
                  };
            }
            last BODY_ISSUES;
        }

        # If here parent line != body line
        $expectedBodyColumn = $anchorColumn;
        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $gap,
                {
                    mainColumn => $expectedBodyColumn,
                    tag        => $runeName,
                    subpolicy  => [$runeName],
                    details    => [ [ $runeName, @{$anchorDetails}, ], ],
                }
            )
          };

        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg =
              sprintf 'split %s body %s is %s',
              $runeName,
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc         => $msg,
                subpolicy    => [ $runeName, 'body-indent' ],
                details      => [ [ $runeName, @{$anchorDetails} ] ],
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $bodyLine,
                column       => $bodyColumn,
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
    my $runeName      = 'faswut';
    my $tag      = 'faswut';

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
              };
            last BODY_ISSUES;
        }
        my $expectedBodyColumn = $parentColumn + 4;
        if ( $bodyColumn != $expectedBodyColumn ) {
            my $msg =
              sprintf 'body %s is %s',
              describeLC( $bodyLine, $bodyColumn ),
              describeMisindent2( $bodyColumn, $expectedBodyColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                subpolicy      => $policy->nodeSubpolicy($node) . ':hgap',
              };
        }
        last BODY_ISSUES;

    }

    $expectedColumn = $parentColumn;
    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $trailerGap,
            {
                mainColumn => $expectedColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                details    => [ [$tag] ],
            }
        )
      };

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
    my $children  = $node->{children};
    my $sel       = $children->[0];
    my $ser       = $children->[-1];
    my ($selLine) = $instance->nodeLC($sel);
    my ($serLine) = $instance->nodeLC($ser);

    return $instance->checkNYI($node) if $selLine != $serLine;
    return [];
}

sub checkSplit_0Running {
    my ( $policy, $node ) = @_;
    my $gapSeq          = $policy->gapSeq($node);
    my $instance        = $policy->{lint};
    my $lineToPos       = $instance->{lineToPos};
    my $minimumRunsteps = $instance->{minSplit_0RunningSteps} // 0;

    my $runeName = $policy->runeName($node);

    my ( $rune, $runningGap, $running, $tistisGap, $tistis ) =
      @{ $policy->gapSeq($node) };

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $runningLine, $runningColumn ) = $instance->nodeLC($running);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);

    my ( $anchorLine, $anchorColumn ) = ( $runeLine, $runeColumn );
    my $lhsName = $instance->symbol($node);
    my $anchorData;
    if ( $lhsName eq 'tallColsig' ) {

        # say join " ", __FILE__, __LINE__, $runeLine, $runeColumn;
        # TODO: Cleanup after development
        ( $anchorColumn, $anchorData ) = $policy->reanchorInc(
            $node,
            {
                'tallCendot' => 1,
                'tallCenhep' => 1,
                'tallCenlus' => 1,
                'tallCollus' => 1,
                'tallKethep' => 1,
                'tallTisfas' => 1,
            }
        );
    }
    my $anchorDetails;
    $anchorDetails = $policy->anchorDetails( $node, $anchorData )
      if $anchorData;

    # say join ' ', __FILE__, __LINE__, Data::Dumper::Dumper($anchorDetails);

    my $expectedColumn = $anchorColumn + 2;
    my $tag            = 'split 0-running';

    my @mistakes = ();

    # We deal with the running list here, rather than
    # in its own node

    my $runningChildren = [ $runningGap, @{ $running->{children} } ];
    my $runStepCount = ( scalar @{$runningChildren} ) / 2;
    if ( $runStepCount < $minimumRunsteps ) {

        # Untested

        my $msg = sprintf '%s %s; too few runsteps; has %d, minimum is %d',
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
                mainColumn => $runeColumn,
                preColumn  => $expectedColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkRunning(
            {
                children       => $runningChildren,
                tag            => $tag,
                anchorColumn   => $anchorColumn,
                expectedColumn => $expectedColumn,
                anchorDetails  => $anchorDetails,
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $tistisGap,
            {
                mainColumn => $anchorColumn,
                preColumn  => $expectedColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                topicLines => [ $anchorLine, $tistisLine ],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
                subpolicyTag   => $policy->nodeSubpolicy($node),
                tag            => $tag,
                expectedColumn => $anchorColumn,
            }
        )
      };
    return \@mistakes;
}

sub checkJoined_0Running {
    my ( $policy, $node, $joinColumn ) = @_;
    my $gapSeq          = $policy->gapSeq($node);
    my $instance        = $policy->{lint};
    my $lineToPos       = $instance->{lineToPos};
    my $maximumRunsteps = $instance->{maxJoined_0RunningSteps};

    my $runeName = $policy->runeName($node);
    my ( $rune, $runningGap, $running, $tistisGap, $tistis ) =
      @{ $policy->gapSeq($node) };

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $anchorLine,  $anchorColumn )  = ( $runeLine, $runeColumn );
    my ( $runningLine, $runningColumn ) = $instance->nodeLC($running);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);

    my @mistakes       = ();
    my $tag            = 'joined 0-running';
    my $expectedColumn = $joinColumn >= 0 ? $joinColumn : $runeColumn + 4;

    # We deal with the running list here, rather than
    # in its own node

    my @runningChildren = ( $runningGap, @{ $running->{children} } );
    my $runStepCount = ( scalar @runningChildren ) / 2;
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
            details      => [ [$tag] ],
          };
    }

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

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $tistisGap,
            {
                mainColumn => $runeColumn,
                preColumn  => $expectedColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                topicLines => [ $runeLine, $tistisLine ],
                details    => [ [$tag] ],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
                subpolicyTag   => $policy->nodeSubpolicy($node),
                tag            => $tag,
                expectedColumn => $runeColumn,
            }
        )
      };
    return \@mistakes;
}

sub check_1Running {
    my ( $policy, $node ) = @_;
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};
    my $tag       = '1-running';

    my $runeName = $policy->runeName($node);

    my ( $rune, $headGap, $head, $runningGap, $running, $tistisGap, $tistis ) =
      @{ $policy->gapSeq($node) };

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $anchorLine,  $anchorColumn )  = ( $runeLine, $runeColumn );
    my ( $headLine,    $headColumn )    = $instance->nodeLC($head);
    my ( $runningLine, $runningColumn ) = $instance->nodeLC($running);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);

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
          };
    }

    $expectedColumn = $anchorColumn + 2;

    # Note: runnings are never pseudo-joined, at
    # least not in the corpus.
    if ( $headLine != $runningLine ) {
        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $runningGap,
                {
                    mainColumn => $anchorColumn,
                    preColumn  => $runningColumn,
                    tag        => $tag,
                subpolicy => [ $runeName ],
                }
            )
          };

        my @runningChildren = ( $runningGap, @{ $running->{children} } );

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

    }
    else {
        # joined, that is, $headLine == $runningLine
        my $gapLength = $runningGap->{length};
        my ( $runningGapLine, $runningGapColumn ) =
          $instance->nodeLC($runningGap);
        my $nextExpectedColumn = $runningGapColumn + 2;

        if ( $nextExpectedColumn != $runningColumn ) {
            my $msg = sprintf
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
              };
        }

        my @runningChildren = ( $runningGap, @{ $running->{children} } );

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

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $tistisGap,
            {
                mainColumn => $runeColumn,
                preColumn  => $expectedColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                topicLines => [ $runeLine, $tistisLine ],
            }
        )
      };

    $expectedColumn = $runeColumn;

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
                tag            => $tag,
                expectedColumn => $runeColumn,
            }
        )
      };

    return \@mistakes;
}

# In the Hoon grammar, some 0 runnings are "punned" as 1-runnings.
# Indentation is *not* the same however, and `hoonlint` must
# treat them separately.
sub check_0_as_1Running {
    my ( $policy, $node ) = @_;
    my $gapSeq    = $policy->gapSeq($node);
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my $runeName = $policy->runeName($node);
    my ( $rune, $headGap, $head, $runningGap, $running, $tistisGap, $tistis ) =
      @{ $policy->gapSeq($node) };

    my $anchorNode = $instance->firstBrickOfLine($node);
    my ( $anchorLine,  $anchorColumn )  = $instance->nodeLC($anchorNode);
    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $headLine,    $headColumn )    = $instance->nodeLC($head);
    my ( $runningLine, $runningColumn ) = $instance->nodeLC($running);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);

    my @mistakes       = ();
    my $tag            = '0as1-running';
    my $expectedColumn = $anchorColumn;

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $runningGap,
            {
                mainColumn => $anchorColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
            }
        )
      };

    # We deal with the running list here, rather that
    # in its own node

    # "De-pun" the 0-running by prepending the
    # fake head to the list of running children
    my @runningChildren = ( $headGap, $head, $runningGap );
    push @runningChildren, @{ $running->{children} };

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

    # Needs to use lower level isOneLineGap() call
    # say STDERR join " ", __FILE__, __LINE__,  ( $policy->nodeSubpolicy($node) );
    if (
    my @gapMistakes = @{
        $policy->isOneLineGap( $tistisGap,
            { tag => $tag, subpolicy => $policy->nodeSubpolicy($node) },
            $anchorColumn )
      }
    )
    {
        for my $gapMistake (@gapMistakes) {
            my $gapMistakeMsg    = $gapMistake->{msg};
            my $gapMistakeLine   = $gapMistake->{line};
            my $gapMistakeColumn = $gapMistake->{column};
            my $gapSubpolicy     = $gapMistake->{subpolicy} // q{};
            my $gapSubpolicyTail = $gapSubpolicy;
            $gapSubpolicyTail =~ s/^.*[:]//;
            my $msg;
            if ( $gapSubpolicy eq 'missing-newline' ) {
                $msg = sprintf
                  'gap %s; vertical gap must precede TISTIS',
                  describeLC( $gapMistakeLine, $gapMistakeColumn );
            }
            else {
                $msg = sprintf
                  "$tag TISTIS %s; $gapMistakeMsg",
                  describeLC( $tistisLine, $tistisColumn );
            }
            push @mistakes,
              {
                desc         => $msg,
                subpolicy    => $gapSubpolicy,
                parentLine   => $runeLine,
                parentColumn => $runeColumn,
                line         => $gapMistakeLine,
                column       => $gapMistakeColumn,
                details      => [ [$tag] ],
                topicLines   => [ $anchorLine, $tistisLine ],
              };
        }
    }

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
                subpolicyTag   => $policy->nodeSubpolicy($node),
                tag            => $tag,
                expectedColumn => $anchorColumn,
            }
        )
      };
    return \@mistakes;
}

# Find the oversize alignment for this column of elements.
# of a chain alignment
# $nodes must a ref to an array of repeating
# ( gap, body, backdentColumn ) elements
sub findChainAlignment {
    my ( $policy, $nodes ) = @_;
    my $instance = $policy->{lint};
    my %allAlignments = ();

    # local $Data::Dumper::Maxdepth = 1;
    # say STDERR join " ", __FILE__, __LINE__;

    # "wide", meaning an alignment following a gap of more than
    # one stop
    my %wideAlignments = ();
  CHILD: for ( my $bodyIX = 2 ; 1 ; $bodyIX += 3 ) {
        my $body   = $nodes->[$bodyIX -1];
        # say STDERR join " ", __FILE__, __LINE__, $bodyIX, $#$nodes;
        # say STDERR join " ", __FILE__, __LINE__, $nodes->[$bodyIX];
        last CHILD if not defined $body;
        my ( $bodyLine, $bodyColumn ) = $instance->nodeLC( $body );
        # say STDERR join " ", __FILE__, __LINE__, $bodyLine, $bodyColumn;
        my $backdentColumn   = $nodes->[$bodyIX];
        next CHILD if $bodyColumn == $backdentColumn;
        $allAlignments{$bodyColumn} //= [];
        push @{ $allAlignments{$bodyColumn} }, $bodyLine;
        my $gap       = $nodes->[ $bodyIX - 2 ];
        my $gapLength = $instance->gapLength($gap);
        next CHILD if $gapLength <= 2;
        $wideAlignments{$bodyColumn} //= [];
        push @{ $wideAlignments{$bodyColumn} }, $bodyLine;
    }

    # say STDERR join " ", __FILE__, __LINE__, scalar @{$nodes};

    return [-1, {lines => []}] if not scalar %wideAlignments;

    # wide alignments, in order first by descending count of wide instances;
    # then by descending count of all instances;
    # then by ascending first line
    my @sortedWideAlignments =
      sort {
        ( scalar @{ $wideAlignments{$b} } ) <=> ( scalar @{ $wideAlignments{$a} } )
        or ( scalar @{ $allAlignments{$b} } ) <=> ( scalar @{ $allAlignments{$a} } )
          or $wideAlignments{$a}[0] <=> $wideAlignments{$b}[0]
      }
      keys %wideAlignments;

    my $topWideColumn = $sortedWideAlignments[0];

    
    # say STDERR join " ", __FILE__, __LINE__, $topWideColumn;

    # Make sure this is actually an *alignment*, that is,
    # that there are at least 2 instances.  Otherwise,
    # ignore it.
    return [-1, {lines => []}] if scalar @{ $allAlignments{$topWideColumn} } <= 1;

    # say STDERR join " ", __FILE__, __LINE__, $topWideColumn;

    return [$topWideColumn, {lines => [splice(@{$allAlignments{$topWideColumn}}, 0, 5)]}];

}

sub chainAlignmentData {
    my ( $policy, $argNode ) = @_;
    my $instance  = $policy->{lint};
    my $chainable = $policy->{chainable};
    my $grammar   = $instance->{grammar};

    state $mortar = {
       tall5d => 1,
       norm5d => 1,
    };

    my $argNodeIX = $argNode->{IX};
    my $chainAlignmentData =
      $policy->{perNode}->{$argNodeIX}->{chainAlignmentData};
    return $chainAlignmentData if $chainAlignmentData;

    # Find the base column of the alignment
    my $alignmentBaseColumn;
    my $chainHead;
    my ( $argNodeLine, $argNodeColumn ) = $instance->nodeLC($argNode);
    my $thisNode = $argNode;
    # say STDERR join " ", __FILE__, __LINE__;
  LINK: while ($thisNode) {
    # say STDERR join " ", __FILE__, __LINE__;
        last LINK if $thisNode->{NEXT};                    # Must be rightmost
        my $symbolName = $instance->symbol($thisNode);
        if ($mortar->{$symbolName}) {
            $thisNode            = $thisNode->{PARENT};
            next LINK;
        }

        # TODO -- replace this test by always ending the LINK
        # loop on non-recognized-mortar non-chainable.
        last LINK if $symbolName =~ m/^(rick5dJog|ruck5dJog|fordHoop)$/;
    # say STDERR join " ", __FILE__, __LINE__;
        if (not $policy->chainable($thisNode) and not $instance->brickName($thisNode)) {
            say STDERR join " ", __FILE__, __LINE__, $instance->symbol($thisNode);
        }

        last LINK if not $policy->chainable($thisNode);    # Must be chainable
    # say STDERR join " ", __FILE__, __LINE__;
        my ( $thisNodeLine, $thisNodeColumn ) = $instance->nodeLC($thisNode);
        last LINK if $thisNodeLine != $argNodeLine;
    # say STDERR join " ", __FILE__, __LINE__;
        $alignmentBaseColumn = $thisNodeColumn;
        $chainHead           = $thisNode;
        $thisNode            = $thisNode->{PARENT};
    }
    # say STDERR join " ", __FILE__, __LINE__;

    return if not defined $alignmentBaseColumn;    # TODO: delete after development

    # Find the head (first link) of this alignment chain
  LINK: while ($thisNode) {
        last LINK if $thisNode->{NEXT};                    # Must be rightmost
        my $symbolName = $instance->symbol($thisNode);
        if ($mortar->{$symbolName}) {
            $thisNode            = $thisNode->{PARENT};
            next LINK;
        }
        last LINK if not $policy->chainable($thisNode);    # Must be chainable
        my ( $thisNodeLine, $thisNodeColumn ) = $instance->nodeLC($thisNode);
        last LINK if $thisNodeColumn < $alignmentBaseColumn;
        $chainHead = $thisNode if $thisNodeColumn == $alignmentBaseColumn;
        $thisNode = $thisNode->{PARENT};
    }

    # Traverse the whole chain, from the head
    $thisNode = $chainHead;
    my $parentNodeLine     = -1;
    my $parentElementCount = 0;
    my $currentChainOffset = 0;
    my $thisNodeIX;

    # Array, indexed by child index, of "silos"
    # of [ gap, body ], to align
    my @nodesToAlignByChildIX;
  LINK: while ($thisNode) {
        # say STDERR sprintf q{Traversing %s}, $instance->literalNode($thisNode);

        # End of loop tests --
        # Is it chainable?
        # Is the line,column right for the current chain?
        # CURRENT say STDERR join q{ }, __FILE__, __LINE__;
        my $symbolName = $instance->symbol($thisNode);
        # Skip if it's one of the appropriate list of mortar symbols.
        if ($mortar->{$symbolName}) {
            my $children = $thisNode->{children};
            $thisNode = $children->[$#$children];
            next LINK;
        }
        last LINK if not $policy->chainable($thisNode);
        my ( $thisNodeLine, $thisNodeColumn ) = $instance->nodeLC($thisNode);
        if ( $thisNodeColumn == $alignmentBaseColumn ) {
            $currentChainOffset = 0;
        }
        else {
            last LINK if $thisNodeLine != $parentNodeLine;
            $currentChainOffset += $parentElementCount;
        }

        # Tracks "last" node IX, so do not set $thisNodeIX before "end
        # of loop" tests.
        $thisNodeIX = $thisNode->{IX};
        # CURRENT say STDERR join q{ }, __FILE__, __LINE__;

        my @thisGapSeq   = @{ $policy->gapSeq0($thisNode) };
        my $elementCount = ( scalar @thisGapSeq ) / 2;

      ELEMENT:
        for (
            my $elementNumber = 1 ;
            $elementNumber <= $elementCount ;
            $elementNumber++
          )
        {
            my $gap           = $thisGapSeq[ $elementNumber * 2 - 2 ];
            my $element       = $thisGapSeq[ $elementNumber * 2 - 1 ];
            my ($gapLine)     = $instance->nodeLC($gap);
            my ($elementLine) = $instance->nodeLC($element);

            # Not in aligment if not on rune line
            next ELEMENT if $elementLine != $thisNodeLine;

            my $backdentColumn =
              $thisNodeColumn + ( $elementCount - $elementNumber ) * 2;
            push @{ $nodesToAlignByChildIX[$currentChainOffset + $elementNumber-1] }, $gap, $element, $backdentColumn;
        }

        $policy->{perNode}->{$thisNodeIX}->{chainAlignmentData} =
          { offset => $currentChainOffset };

        # Set "parent" data for next pass
        $parentNodeLine     = $thisNodeLine;
        $parentElementCount = $elementCount;

        # descend to rightmost child
        my $children = $thisNode->{children};
        $thisNode = $children->[$#$children];
    }

    my $lastNodeIX = $thisNodeIX;

    my @chainAlignments = ();
  ELEMENT:
    for (
        my $childIX = 0 ;
        $childIX <= $#nodesToAlignByChildIX;
        $childIX++
      )
    {
        my $nodesToAlign = $nodesToAlignByChildIX[$childIX];
        if (not $nodesToAlign) {
            $chainAlignments[ $childIX ] = [ -1, [] ];
            next ELEMENT;
        }
        $chainAlignments[ $childIX ] = $policy->findChainAlignment( $nodesToAlign);
    }

    $thisNode = $chainHead;
  LINK: while ($thisNode) {
        my $thisNodeIX = $thisNode->{IX};
        $policy->{perNode}->{$thisNodeIX}->{chainAlignmentData}->{alignments} =
          \@chainAlignments;
        last LINK if $thisNodeIX == $lastNodeIX;

        # descend to rightmost child
        my $children = $thisNode->{children};
        $thisNode = $children->[$#$children];
    }

    $chainAlignmentData = $policy->{perNode}->{$argNodeIX}->{chainAlignmentData};
    return $chainAlignmentData;
}

sub chessSideOfJoggingHoon {
    my ( $policy, $node ) = @_;
    my $nodeIX    = $node->{IX};
    my $chessSide = $policy->{perNode}->{$nodeIX}->{chessSide};
    return $chessSide if defined $chessSide;

    my $instance    = $policy->{lint};
    my $joggingRule = $instance->{joggingRule};
    my $nodeName    = $instance->brickName($node);
    if ( not $nodeName or not $joggingRule->{$nodeName} ) {
        my $chessSide = $policy->chessSideOfJoggingHoon( $node->{PARENT} );
        $policy->{perNode}->{$nodeIX}->{chessSide} = $chessSide;
        return $chessSide;
    }

    my ( undef, $baseColumn ) = $instance->nodeLC($node);
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
    my $instance = $policy->{lint};

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
    my $nodeIX        = $node->{IX};
    my $jogBodyData = $policy->{perNode}->{$nodeIX}->{jogBodyData};
    return $jogBodyData if defined $jogBodyData;

    my $instance     = $policy->{lint};
    my $joggingRules = $instance->{joggingRule};
    my $joggingRule  = $instance->{joggingRule};
    my $nodeName     = $instance->brickName($node);
    if ( not $nodeName or not $joggingRule->{$nodeName} ) {
        my $jogBodyData =
          $policy->bodyColumn( $node->{PARENT}, $joggingRules );
        $policy->{perNode}->{$nodeIX}->{jogBodyData} = $jogBodyData;
        return $jogBodyData;
    }

    my $children = $node->{children};
  CHILD: for my $childIX ( 0 .. $#$children ) {
        my $child  = $children->[$childIX];
        my $symbol = $instance->symbol($child);
        next CHILD if $symbol ne 'rick5d' and $symbol ne 'ruck5d';
        my $jogBodyData = $policy->joggingBodyAlignment($child);
        $policy->{perNode}->{$nodeIX}->{jogBodyData} = $jogBodyData;
        return $jogBodyData;
    }
    die "No jogging found for ", $instance->symbol($node);
}

# TODO: Delete this?
# Compute the alignment for a chained series of fixed-N
# runes
sub chainedAlignment {
    my ( $policy, $link0 ) = @_;
    my $instance  = $policy->{lint};
    my $chainable = $policy->{chainable};
    my $grammar   = $instance->{grammar};

    # Assumes link is chainable
    my $link = $link0;
    my ( $linkLine, $linkColumn ) = $instance->line_column( $link->{start} );
    my $alignment0 = $linkColumn;

  LINK: while (1) {
        my $children = $link->{children};

        # compute next link
        $link = $children->[$#$children];
        my $ruleID = $link->{ruleID};
        my ( $lhs, @rhs ) = $grammar->rule_expand( $link->{ruleID} );
        my $lhsName = $grammar->symbol_name($lhs);

        # last LINK if next is not chainable
        last LINK unless $chainable->{$lhsName};
        ( $linkLine, $linkColumn ) = $instance->line_column( $link->{start} );
    }

}

# Find the oversize alignment for this column of elements.
# $nodes must a ref to an array of alternating gap, body,
# nodes.
sub findAlignment {
    my ( $policy, $nodes ) = @_;
    my $instance = $policy->{lint};
    my %allAlignments = ();

    # local $Data::Dumper::Maxdepth = 1;
    # say STDERR join " ", __FILE__, __LINE__;

    # "wide", meaning an alignment following a gap of more than
    # one stop
    my %wideAlignments = ();
  CHILD: for ( my $bodyIX = 1 ; 1 ; $bodyIX += 2 ) {
        my $body   = $nodes->[$bodyIX];
        # say STDERR join " ", __FILE__, __LINE__, $bodyIX, $#$nodes;
        # say STDERR join " ", __FILE__, __LINE__, $nodes->[$bodyIX];
        last CHILD if not defined $body;
        # say STDERR join " ", __FILE__, __LINE__, $nodes->[$bodyIX];
        my ( $bodyLine, $bodyColumn ) = $instance->nodeLC( $body );
        $allAlignments{$bodyColumn} //= [];
        push @{ $allAlignments{$bodyColumn} }, $bodyLine;
        my $gap       = $nodes->[ $bodyIX - 1 ];
        my $gapLength = $instance->gapLength($gap);
        next CHILD if $gapLength <= 2;
        $wideAlignments{$bodyColumn} //= [];
        push @{ $wideAlignments{$bodyColumn} }, $bodyLine;
    }

    # say STDERR join " ", __FILE__, __LINE__, scalar @{$nodes};

    return [-1, []] if not scalar %wideAlignments;

    # wide alignments, in order first by descending count of wide instances;
    # then by descending count of all instances;
    # then by ascending first line
    my @sortedWideAlignments =
      sort {
        ( scalar @{ $wideAlignments{$b} } ) <=> ( scalar @{ $wideAlignments{$a} } )
        or ( scalar @{ $allAlignments{$b} } ) <=> ( scalar @{ $allAlignments{$a} } )
          or $wideAlignments{$a}[0] <=> $wideAlignments{$b}[0]
      }
      keys %wideAlignments;

    my $topWideColumn = $sortedWideAlignments[0];

    # say STDERR join " ", __FILE__, __LINE__, $topWideColumn;

    # Make sure this is actually an *alignment*, that is,
    # that there are at least 2 instances.  Otherwise,
    # ignore it.
    return [-1, []] if scalar @{ $allAlignments{$topWideColumn} } <= 1;

    # say STDERR join " ", __FILE__, __LINE__, $topWideColumn;

    return [$topWideColumn, [splice(@{$allAlignments{$topWideColumn}}, 0, 5)]];

}

sub joggingBodyAlignment {
    my ( $policy, $jogging ) = @_;
    my $instance  = $policy->{lint};
    my $children = $jogging->{children};

    # Traverse first to last to make it easy to record
    # first line of occurrence of each body column
    my @nodesToAlign = ();
  CHILD:
    for ( my $childIX = 0; $childIX <= $#$children ; $childIX+=2 ) {
        my $jog         = $children->[$childIX];
        my $jogChildren = $jog->{children};
        my $jogChild1 = $jogChildren->[1];
        my $jogChild2 = $jogChildren->[2];
        my ( $line1    )    = $instance->nodeLC($jogChild1);
        my ( $line2    )    = $instance->nodeLC($jogChild2);
        next CHILD if $line1 ne $line2;
        push @nodesToAlign, $jogChild1, $jogChild2;
    }
    return $policy->findAlignment( \@nodesToAlign );
}

sub check_1Jogging {
    my ( $policy, $node ) = @_;
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my ( $rune, $headGap, $head, $joggingGap, $jogging, $tistisGap, $tistis ) =
      @{ $policy->gapSeq($node) };

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $headLine,    $headColumn )    = $instance->nodeLC($head);
    my ( $joggingLine, $joggingColumn ) = $instance->nodeLC($jogging);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);

    my $chessSide     = $policy->chessSideOfJoggingHoon($node);
    my $joggingRules  = $instance->{joggingRule};

    my @mistakes = ();
    my $runeName = $policy->runeName($node);
    my $tag      = '1-jogging';

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
          };
    }

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $joggingGap,
            {
                mainColumn => $runeColumn,
                tag        => ( sprintf '1-jogging %s jogging', $chessSide ),
                subpolicy => [ $runeName ],
                parent     => $rune,
                topicLines => [$joggingLine],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $tistisGap,
            {
                mainColumn => $runeColumn,
                tag        => $tag,
                subpolicy => [$runeName],
                topicLines => [$tistisLine],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
                tag            => $tag,
                expectedColumn => $runeColumn,
            }
        )
      };

    return \@mistakes;
}

sub check_2Jogging {
    my ( $policy, $node ) = @_;
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my $runeName = $policy->runeName($node);
    my (
        $rune,       $headGap, $head,      $subheadGap, $subhead,
        $joggingGap, $jogging, $tistisGap, $tistis
    ) = @{ $policy->gapSeq($node) };

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $headLine,    $headColumn )    = $instance->nodeLC($head);
    my ( $subheadLine, $subheadColumn ) = $instance->nodeLC($subhead);
    my ( $joggingLine, $joggingColumn ) = $instance->nodeLC($jogging);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);

    my $chessSide     = $policy->chessSideOfJoggingHoon($node);
    my $joggingRules  = $instance->{joggingRule};

    my @mistakes = ();
    my $tag      = '2-jogging';

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
              };
        }

        if ( $subheadGap->{length} != 2 ) {
            my ( undef, $subheadGapColumn ) = $instance->nodeLC($subheadGap);
            $expectedColumn = $subheadGapColumn + 2;
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
                line           => $headLine,
                column         => $headColumn,
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
              };
        }

        # If here, we have "split heads", which should follow the "pseudo-jog"
        # format

        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $subheadGap,
                {
                    mainColumn => $runeColumn,
                    tag        => $tag,
                subpolicy => [ $runeName ],
                    details    => [ [$tag] ],
                    topicLines => [$subheadLine],
                }
            )
          };

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
              };
        }
    }

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $joggingGap,
            {
                mainColumn => $runeColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                details    => [ [$tag] ],
                topicLines => [$joggingLine],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $tistisGap,
            {
                mainColumn => $runeColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                topicLines => [$tistisLine],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
                tag            => $tag,
                expectedColumn => $runeColumn,
            }
        )
      };

    return \@mistakes;
}

sub check_Jogging1 {
    my ( $policy, $node ) = @_;
    my $instance  = $policy->{lint};
    my $lineToPos = $instance->{lineToPos};

    my $runeName = $policy->runeName($node);
    my ( $rune, $joggingGap, $jogging, $tistisGap, $tistis, $tailGap, $tail ) =
      @{ $policy->gapSeq($node) };

    my ( $runeLine,    $runeColumn )    = $instance->nodeLC($rune);
    my ( $joggingLine, $joggingColumn ) = $instance->nodeLC($jogging);
    my ( $tistisLine,  $tistisColumn )  = $instance->nodeLC($tistis);
    my ( $tailLine,    $tailColumn )    = $instance->nodeLC($tail);

    my @mistakes = ();
    my $tag      = 'jogging-1';

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
            details      => [ [$tag] ],
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
            details        => [ [$tag] ],
          };
    }

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $tistisGap,
            {
                mainColumn => $runeColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                topicLines => [$tistisLine],
                details    => [ [$tag] ],
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkTistis(
            $tistis,
            {
                tag            => $tag,
                expectedColumn => $runeColumn + 2,
            }
        )
      };

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $tailGap,
            {
                mainColumn => $runeColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                details    => [ [$tag] ],
                topicLines => [$tailLine],
            }
        )
      };

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
    my @nodesToAlign = ();

  CHILD:
    for ( my $childIX = 0 ; $childIX <= $#$children ; $childIX++ ) {
        my $jog = $children->[$childIX];
        my ( $gap,      $body )       = @{ $policy->gapSeq0($jog) };
        my ( $headLine, $headColumn ) = $instance->nodeLC($jog);
        my ( $bodyLine, $bodyColumn ) = $instance->nodeLC($body);
        next CHILD unless $headLine == $bodyLine;
        push @nodesToAlign, $gap, $body;
    }
    return $policy->findAlignment( \@nodesToAlign );
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
            my $child2  = $children2->[$childIX2];
            my $symbol2 = $instance->symbol($child2);
            next CHILD2 if $symbol2 ne 'fordFascomElements';
            my $fascomBodyData = $policy->fascomBodyAlignment($child2);
            $policy->{perNode}->{$nodeIX}->{fascomBodyData} =
              $fascomBodyData;
            return $fascomBodyData;
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
    my $runeNodeIX = $runeNode->{IX};
    my $isJoined = $policy->{perNode}->{$runeNodeIX}->{isJoined};

    my ( $runeLine,   $runeColumn )   = $instance->nodeLC($runeNode);
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my ( $headLine,   $headColumn )   = ( $parentLine, $parentColumn );
    my ( $gap,        $body )         = @{ $policy->gapSeq0($node) };
    my ( $bodyLine,   $bodyColumn )   = $instance->nodeLC($body);

    my $anchorLine = $runeLine;
    my ( $anchorColumn, $anchorData ) = $policy->reanchorInc(
        $runeNode,
        {
            fordFascen => 1,
        }
    );
    my $anchorDetails = $policy->anchorDetails( $node, $anchorData );

    my ( $fascomBodyColumn, $fascomBodyColumnDetails ) =
      @{ $policy->fascomBodyColumn( $node, { fordFascom => 1 } ) };

    my @mistakes = ();
    my $tag      = 'fascom-element';

    my $baseColumn = $anchorColumn + ($isJoined ? 4 : 2);

    my $expectedHeadColumn = $baseColumn;
    if ( $headColumn != $expectedHeadColumn ) {
        my $msg = sprintf 'Fascom element head %s; %s',
          describeLC( $headLine, $headColumn ),
          describeMisindent2( $headColumn, $expectedHeadColumn );
        push @mistakes,
          {
            desc           => $msg,
            subpolicy => [ $tag, 'head-hgap' ],
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $headLine,
            column         => $headColumn,
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
                subpolicy => [ $tag, 'body-hgap' ],
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
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
                subpolicy => [ $tag, 'pseudo-comment-indent' ],
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
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
                subpolicy => [ $tag, 'pseudo-hgap' ],
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
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
            subpolicy => [ $tag, 'body-indent' ],
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $bodyLine,
            column         => $bodyColumn,
            topicLines     => [$runeLine],
          };
        return \@mistakes;
    }

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $gap,
            {
                mainColumn => $expectedBodyColumn,
                tag        => $tag,
                subpolicy => [ $tag ],
                details    => [ [$tag] ],
                topicLines => [$runeLine],
            }
        )
      };

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
    my $runeName      = 'fastis';
    my $tag      = 'fastis';

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
                reportLine   => $symbolLine,
                reportColumn => $symbolColumn,
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
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $symbolLine,
                column       => $symbolColumn,
                reportLine   => $symbolLine,
                reportColumn => $symbolColumn,
              };
        }
    }

  CHECK_HORN: {
        if ( $hornLine == $symbolLine ) {
            my $symbolLength       = $symbol->{length};
            my $expectedHornColumn = $symbolColumn + $symbolLength + 2;
            if ( $hornColumn != $expectedHornColumn ) {
                my $msg = sprintf 'Fastis horn %s; %s',
                  describeLC( $hornLine, $hornColumn ),
                  describeMisindent2( $hornColumn, $expectedHornColumn );
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $parentLine,
                    parentColumn => $parentColumn,
                    line         => $hornLine,
                    column       => $hornColumn,
                    reportLine   => $hornLine,
                    reportColumn => $hornColumn,
                  };
            }
            last CHECK_HORN;
        }

        # if here, horn Line != symbol line
        my $expectedHornColumn = $parentColumn + 2;

        push @mistakes,
          @{
            $policy->checkOneLineGap(
                $hornGap,
                {
                    mainColumn => $expectedHornColumn,
                    tag        => $tag,
                subpolicy => [ $runeName ],
                    details    => [ [$tag] ],
                }
            )
          };

        if ( $hornColumn != $expectedHornColumn ) {
            my $msg = sprintf 'Fastis split horn %s; %s',
              describeLC( $hornLine, $hornColumn ),
              describeMisindent2( $hornColumn, $expectedHornColumn );
            push @mistakes,
              {
                desc         => $msg,
                parentLine   => $parentLine,
                parentColumn => $parentColumn,
                line         => $hornLine,
                column       => $hornColumn,
                reportLine   => $hornLine,
                reportColumn => $hornColumn,
              };
        }

    }

    return \@mistakes;
}

sub checkKingsideJog {
    my ( $policy, $node ) = @_;
    my $instance          = $policy->{lint};
    my $tall_Jogging1Rule = $instance->{tallJogging1_Rule};
    my $fileName          = $instance->{fileName};
    my $grammar           = $instance->{grammar};
    my $ruleID            = $node->{ruleID};
    my ( $parentLine, $parentColumn ) =
      $instance->line_column( $node->{start} );

    my $joggingRules = $instance->{joggingRule};
    my ($jogBodyColumn, $jogBodyColumnLines) = @{$policy->bodyColumn( $node, $joggingRules )};
    $jogBodyColumn //= -1;

    my @mistakes = ();
    my $tag      = 'kingside jog';

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
            topicLines     => [$brickLine],
          };
    }

    if ( $headLine == $bodyLine ) {
        my $gapLength = $gap->{length};

        if ( $gapLength != 2 and $bodyColumn != $jogBodyColumn ) {
            my $misindent;
            my $details;
            my @topicLines = ($brickLine);
            if ( $jogBodyColumn < 0 ) {
              $misindent = describeMisindent2( $gapLength, 2 );
                $details = [ [ $tag, "no inter-line alignment detected" ] ];
            }
            else {
              $misindent = describeMisindent2( $bodyColumn, $jogBodyColumn );
                my $oneBasedColumn = $jogBodyColumn + 1;
                push @topicLines, @{$jogBodyColumnLines};
                $details = [
                    [
                        $tag,
                        sprintf 'inter-line alignment is %d, see %s',
                        $oneBasedColumn,
                        (
                            join q{ },
                            map { $_ . ':' . $oneBasedColumn }
                              @{$jogBodyColumnLines}
                        )
                    ]
                ];
            }
            my $msg = sprintf 'Jog %s body %s; %s',
              $sideDesc,
              describeLC( $bodyLine, $bodyColumn ),
              $misindent;
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                topicLines     => \@topicLines,
                details        => $details,
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
            topicLines     => [$brickLine],
            details        => [
                [
                    $tag,
                    sprintf qq{lexeme "%s" %s},
                    $instance->lexeme( $brickLine, $brickColumn ),
                    describeLC( $brickLine, $brickColumn )
                ]
            ],
          };
        return \@mistakes;
    }

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $gap,
            {
                mainColumn => $expectedBodyColumn,
                tag        => $tag,
                subpolicy => [ $tag ],
                details    => [ [$tag] ],
                topicLines => [ $bodyLine, $brickLine ],
            }
        )
      };

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
    my $tag      = 'queenside jog';

    my $joggingRules = $instance->{joggingRule};
    my ($jogBodyColumn, $jogBodyColumnLines) = @{$policy->bodyColumn( $node, $joggingRules )};
    $jogBodyColumn //= -1;

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
            topicLines     => [$brickLine],
          };
    }

    # Check for flat queenside misalignments
    my $expectedBodyColumn = $jogBodyColumn;
    if ( $headLine == $bodyLine ) {
        my $gapLength = $gap->{length};
        if ( $gapLength != 2 and $bodyColumn != $jogBodyColumn ) {
            my $misindent;
            my $details;
            my @topicLines = ($brickLine);
            if ($jogBodyColumn < 0) {
              $misindent = describeMisindent2( $gapLength, 2 );
               $details = [
                  [ $tag,
                  "no inter-line alignment detected"
                  ]
               ];
            } else {
              $misindent = describeMisindent2( $bodyColumn, $jogBodyColumn );
               my $oneBasedColumn = $jogBodyColumn+1;
               push @topicLines, @{$jogBodyColumnLines};
               $details = [
                  [ $tag,
                  sprintf 'inter-line alignment is %d, see %s', $oneBasedColumn,
                  (join q{ }, map { $_ . ':' . $oneBasedColumn } @{$jogBodyColumnLines})
                  ]
               ];
            }
            my $msg = sprintf 'Jog %s body %s; %s',
              $sideDesc,
              describeLC( $bodyLine, $bodyColumn ),
              $misindent;
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                topicLines     => \@topicLines,
                details => $details,
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
            topicLines     => [$brickLine],
            details        => [
                [
                    $tag,
                    sprintf qq{lexeme "%s" %s},
                    $instance->lexeme( $brickLine, $brickColumn ),
                    describeLC( $brickLine, $brickColumn )
                ]
            ],
          };
        return \@mistakes;
    }

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $gap,
            {
                mainColumn => $expectedBodyColumn,
                tag        => $tag,
                subpolicy => [ $tag ],
                details    => [ [$tag] ],
                topicLines => [ $bodyLine, $brickLine ],
            }
        )
      };

    return \@mistakes;
}

# TODO: Add a check (optional?) for queenside joggings with no
# split jogs.
sub checkJog {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    my $chessSide = $policy->chessSideOfJoggingHoon($node);
    return $policy->checkQueensideJog($node)
      if $chessSide eq 'queenside';
    return $policy->checkKingsideJog($node);
}

# not yet implemented
sub checkNYI {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};
    my $grammar  = $instance->{grammar};
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
    my $nodeIX       = $node->{IX};
    my @gapSeq       = @{ $policy->gapSeq0($node) };
    my $elementCount = ( scalar @gapSeq ) / 2;
    my $instance     = $policy->{lint};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);
    my @mistakes = ();
    my $runeName = $policy->runeName($node);
    my $tag      = $elementCount . '-backdented';

    my $chainOffset = 0;
    my $chainAlignments;
    my $chainAlignmentData = $policy->chainAlignmentData($node);
    if ($chainAlignmentData) {
        $chainOffset = $chainAlignmentData->{offset};
        $chainAlignments = $chainAlignmentData->{alignments};
    }

    my $runningAlignments =
      $policy->{perNode}->{$nodeIX}->{runningAlignments};
            # say STDERR join " ", __FILE__, __LINE__, "nodeIX:", $nodeIX;

    my $reanchorOffset;    # for re-anchoring logic

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
    my $anchorDetails =
      $policy->anchorDetailsBasic( $anchorNode, $anchorColumn );

    # say Data::Dumper::Dumper($anchorDetails);
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
        my $backdentColumn =
          $anchorColumn + ( $elementCount - $elementNumber ) * 2;

        if ( $elementLine == $parentLine ) {

            # OK if at proper alignment for backdent
            next ELEMENT if $backdentColumn == $elementColumn;

            my $gapLength = $instance->gapLength($gap);
            next ELEMENT if $gapLength == 2;

            my $thisAlignment;
            my ( $chainAlignmentColumn, $chainAlignmentDetails );
            my $chainAlignmentLines = [];
            if ($chainAlignments) {
                $thisAlignment =
                  $chainAlignments->[ $chainOffset + $elementNumber - 1 ];
                # say STDERR "$thisAlignment = chainAlignments->[ $chainOffset + $elementNumber - 1 ]";
                ( $chainAlignmentColumn, $chainAlignmentDetails ) =
                  @{$thisAlignment};

            }

            # say STDERR join " ", __FILE__, __LINE__, $elementNumber;
            my ( $runningAlignmentColumn, $runningAlignmentDetails );
            if ($runningAlignments) {
                $thisAlignment = $runningAlignments->[ $elementNumber - 1];
                ( $runningAlignmentColumn, $runningAlignmentDetails ) =
                  @{$thisAlignment};
            }

            if (    defined $chainAlignmentColumn
                and $chainAlignmentColumn >= 0
                and defined $runningAlignmentColumn
                and $runningAlignmentColumn >= 0
                and $chainAlignmentColumn != $runningAlignmentColumn )
            {
                my $msg = sprintf
'inter-line alignment conflict; element #%d of %s at %s; running is %d but chain is %d',
                  $elementNumber,
                  describeLC( $parentLine,  $parentColumn ),
                  describeLC( $elementLine, $elementColumn ),
                  describeLC( $elementLine, $runningAlignmentColumn ),
                  describeLC( $elementLine, $chainAlignmentColumn );
                push @mistakes,
                  {
                    desc         => $msg,
                    parentLine   => $parentLine,
                    parentColumn => $parentColumn,
                    line         => $elementLine,
                    column       => $elementColumn,
                    reportLine   => $elementLine,
                    reportColumn => $elementColumn,
                    subpolicy    => $policy->nodeSubpolicy($node)
                      . ':interline-mismatch',
                  };
            }

            my $interlineAlignmentType;
            my $interlineAlignmentColumn;
          SET_INTERLINE_ALIGNMENT: {
                if ( defined $chainAlignmentColumn
                    and $chainAlignmentColumn >= 0 )
                {
                    $interlineAlignmentType   = 'chain';
                    $interlineAlignmentColumn = $chainAlignmentColumn;
                    last SET_INTERLINE_ALIGNMENT;
                }
                if ( defined $runningAlignmentColumn)
                {
                    $interlineAlignmentType   = 'running';
                    $interlineAlignmentColumn = $runningAlignmentColumn;
                }
            }

            # say STDERR join " ", __FILE__, __LINE__, $interlineAlignmentColumn;
            next ELEMENT
              if defined $interlineAlignmentColumn
              and $interlineAlignmentColumn >= 0
              and $interlineAlignmentColumn == $elementColumn;

            my @topicLines = ();
            my $tightColumn = $gapColumn + 2;
            my @allowedColumns =([ $tightColumn => 'tight' ]);
            if ($backdentColumn > $tightColumn) {
                push @allowedColumns, [ $backdentColumn => 'backdent' ];
            }
            my $details;
            if ($interlineAlignmentColumn and $interlineAlignmentColumn >= $tightColumn) {
                push @allowedColumns, [ $interlineAlignmentColumn => $interlineAlignmentType ];
                my $oneBasedColumn = $interlineAlignmentColumn + 1;
                my $chainAlignmentLines = $chainAlignmentDetails->{lines};
                push @topicLines, @{$chainAlignmentLines};
                $details = [
                    [
                        $tag,
                        sprintf 'chain alignment is %d, see %s',
                        $oneBasedColumn,
                        (
                            join q{ },
                            map { $_ . ':' . $oneBasedColumn }
                              @{$chainAlignmentLines}
                        )
                    ]
                ];
            }
            else {
                $details = [ [ $tag, "no chain alignment detected" ] ];
            }
            my @sortedColumns = sort { $a->[0] <=> $b->[0] } @allowedColumns;
            my $allowedDesc = join "; ",
              map { sprintf '@%d:%d (%s)', $elementLine, $_->[0]+1, $_->[1] } @sortedColumns;
            if (scalar @sortedColumns >= 2) {
               $allowedDesc = 'one of ' . $allowedDesc;
            }

            my $msg = sprintf
              'joined backdent element #%d of %s is at %s; should be %s',
              $elementNumber,
              describeLC( $parentLine, $parentColumn ),
              describeLC( $elementLine, $elementColumn ),
              $allowedDesc;
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $elementLine,
                column         => $elementColumn,
                reportLine     => $elementLine,
                reportColumn   => $elementColumn,
                subpolicy      => $policy->nodeSubpolicy($node) . ':hgap',
                topicLines => \@topicLines,
                details => $details,
              };
            next ELEMENT;
        }

      CHECK_FOR_PSEUDOJOIN: {
            last CHECK_FOR_PSEUDOJOIN if $gapLine != $parentLine;
            my $pseudoJoinColumn = $policy->pseudoJoinColumn($gap);

            last CHECK_FOR_PSEUDOJOIN if $pseudoJoinColumn < 0;

            last CHECK_FOR_PSEUDOJOIN
              if $pseudoJoinColumn != $backdentColumn
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

        push @mistakes, @{
            $policy->checkOneLineGap(
                $gap,
                {
                    mainColumn => $anchorColumn,
                    preColumn  => $elementColumn,
                    tag =>
                      ( sprintf 'backdented element #%d,', $elementNumber ),
                subpolicy => [ $runeName ],
                    details => [
                        [
                            $tag,
                            @{$anchorDetails},
                            'inter-comment indent should be '
                              . ( $anchorColumn + 1 ),

                     # 'pre-comment indent should be ' . ( $runStepColumn + 1 ),
                        ]
                    ],
                }
            )
        };

        if ( $backdentColumn != $elementColumn ) {
            my $msg = sprintf
              'backdented element #%d %s; %s',
              $elementNumber,
              describeLC( $elementLine, $elementColumn ),
              describeMisindent2( $elementColumn, $backdentColumn );
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $elementLine,
                column         => $elementColumn,
                details        => [ [ $tag, @{$anchorDetails}, ] ],
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
    my $runeName      = 'ketdot';
    my $tag      = 'ketdot';

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
              };
        }
    }

    my $gap2     = $gapSeq[2];
    my $element2 = $gapSeq[3];
    my ( $element2Line, $element2Column ) = $instance->nodeLC($element2);

  ELEMENT2: {
        if ( $element1Line != $element2Line ) {    # Element 2 split

            my $expectedColumn = $anchorColumn;

            push @mistakes,
              @{
                $policy->checkOneLineGap(
                    $gap2,
                    {
                        mainColumn => $anchorColumn,
                        tag        => $tag,
                subpolicy => [ $runeName ],
                        details    => [ [$tag] ],
                    }
                )
              };

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
              };
        }
    }

    return \@mistakes;
}

sub checkLuslus {
    my ( $policy, $node, $cellLHS ) = @_;
    my $instance = $policy->{lint};
    my ( $parentLine, $parentColumn ) = $instance->nodeLC($node);

    my $battery = $instance->ancestorByLHS( $node, { whap5d => 1 } );
    die "battery not found" if not defined $battery;
    my ( $batteryLine, $batteryColumn ) = $instance->nodeLC($battery);
    my ($cellBodyColumn, $cellBodyColumnLines) = @{$policy->cellBodyColumn($battery)};

    my @mistakes = ();
    my $runeName      = 'luslus';
    my $tag      = 'luslus';

    # LuslusCell ::= (- LUS LUS GAP -) SYM4K (- GAP -) tall5d
    my ( $headGap, $head, $bodyGap, $body ) = @{ $policy->gapSeq0($node) };
    my ( $headLine, $headColumn ) = $instance->nodeLC($head);
    my ( $bodyLine, $bodyColumn ) = $instance->nodeLC($body);

    my $headGapLength = $headGap->{length};
    if ( $headGapLength != 2 ) {
        my $expectedColumn = $parentColumn + 4;
        my $msg            = sprintf 'Cell head %s; %s',
          describeLC( $bodyLine, $bodyColumn ),
          describeMisindent2( $headColumn, $expectedColumn );
        push @mistakes,
          {
            desc           => $msg,
            parentLine     => $parentLine,
            parentColumn   => $parentColumn,
            line           => $headLine,
            column         => $headColumn,
          };
    }

    if ( $headLine == $bodyLine ) {
        my $bodyGapLength = $bodyGap->{length};

        if ( $bodyGapLength != 2 and $bodyColumn != $cellBodyColumn ) {
            my @topicLines = ($batteryLine);
            my $misindent;
            my $details;
            if ( $cellBodyColumn < 0 ) {
                $misindent = describeMisindent2( $bodyGapLength, 2 );
                $details = [ [ $tag, "no inter-line alignment detected" ] ];
            }
            else {
                $misindent = describeMisindent2( $bodyColumn, $cellBodyColumn );
                my $oneBasedColumn = $cellBodyColumn + 1;
                push @topicLines, @{$cellBodyColumnLines};
                $details = [
                    [
                        $tag,
                        sprintf 'inter-line alignment is %d, see %s',
                        $oneBasedColumn,
                        (
                            join q{ },
                            map { $_ . ':' . $oneBasedColumn }
                              @{$cellBodyColumnLines}
                        )
                    ]
                ];
            }
            my $msg = sprintf 'Cell body %s; %s',
              describeLC( $bodyLine, $bodyColumn ), $misindent;
            push @mistakes,
              {
                desc           => $msg,
                parentLine     => $parentLine,
                parentColumn   => $parentColumn,
                line           => $bodyLine,
                column         => $bodyColumn,
                topicLines     => \@topicLines,
                details        => $details,
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
            topicLines     => [$batteryLine],
          };
        return \@mistakes;
    }

    push @mistakes,
      @{
        $policy->checkOneLineGap(
            $bodyGap,
            {
                mainColumn => $expectedBodyColumn,
                tag        => $tag,
                subpolicy => [ $runeName ],
                details    => [ [$tag] ],
                topicLines => [ $bodyLine, $batteryLine ],
            }
        )
      };

    return \@mistakes;
}

sub validate {
    my ( $policy, $node ) = @_;
    my $instance = $policy->{lint};

    $policy->validate_node($node);
    return if $node->{type} ne 'node';
    my $children = $node->{children};
  CHILD: for my $childIX ( 0 .. $#$children ) {
        my $child = $children->[$childIX];
        $policy->validate($child);
    }
    return;
}

sub reportMistakes {
    my ( $policy, $mistakes ) = @_;
    my $instance = $policy->{lint};
    my $fileName = $instance->{fileName};

    my @pieces = ();
  MISTAKE: for my $mistake ( @{$mistakes} ) {

        my $parentLine   = $mistake->{parentLine};
        my $parentColumn = $mistake->{parentColumn};
        my $desc         = $mistake->{desc};
        my $mistakeLine  = $mistake->{line};

        # The default report location should be line, column
        # instead of parentLine, parentColumn
        $mistake->{reportLine}   //= $parentLine;
        $mistake->{reportColumn} //= $parentColumn;

        $instance->reportItem( $mistake, $desc,
            ( $mistake->{topicLines} // [] ), $mistakeLine, );
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

    my $NYI_Rule               = $instance->{NYI_Rule};
    my $backdentedRule         = $instance->{backdentedRule};
    my $tallRuneRule           = $instance->{tallRuneRule};
    my $tallJogRule            = $instance->{tallJogRule};
    my $tallNoteRule           = $instance->{tallNoteRule};
    my $tallLuslusRule         = $instance->{tallLuslusRule};
    my $tall_0RunningRule      = $instance->{tall_0RunningRule};
    my $tall_0_as_1RunningRule = $instance->{tall_0_as_1RunningRule};
    my $tall_1RunningRule      = $instance->{tall_1RunningRule};
    my $tall_1JoggingRule      = $instance->{tall_1JoggingRule};
    my $tall_2JoggingRule      = $instance->{tall_2JoggingRule};
    my $tall_Jogging1Rule      = $instance->{tallJogging1_Rule};

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

    my $mistakes   = [];
    my $start      = $node->{start};
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
                    $mistakes = $policy->checkSeq( $node, 'hoop' );
                    last TYPE_INDENT if @{$mistakes};
                    $indentDesc = 'FORD_HOOP_SEQ';
                    last TYPE_INDENT;
                }

                if ( $lhsName eq 'hornSeq' ) {
                    $mistakes = $policy->checkSeq( $node, 'horn' );
                    last TYPE_INDENT if @{$mistakes};
                    $indentDesc = 'HORN_SEQ';
                    last TYPE_INDENT;
                }

                if ( $lhsName eq 'optBonzElements' ) {
                    $mistakes = $policy->checkSeq( $node, 'bonz element' );
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
                    $mistakes = $policy->checkWhap5d($node);
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
                last TYPE_INDENT;
            }

            if ( $lhsName eq "bonzElement" ) {
                $mistakes = $policy->checkBonzElement($node);
                last TYPE_INDENT;
            }

            if ( $lhsName eq 'fordFascom' ) {
                $mistakes = $policy->checkFascom($node);
                last TYPE_INDENT;
            }

            if ( $lhsName eq 'fordFasdot' ) {
                # say STDERR join " ", __FILE__, __LINE__, $lhsName, $instance->literalNode($node);
                $mistakes = $policy->checkFasdot($node);
                last TYPE_INDENT;
            }

            if ( $lhsName =~ m/^fordFas(buc|cab|cen|hax|sig)$/ ) {
                $mistakes = $policy->checkFord_1( $node );
                last TYPE_INDENT;
            }

            if ( $lhsName eq "fordHoop" ) {
                $mistakes = $policy->checkFordHoop( $node );
                last TYPE_INDENT;
            }

            if ( $lhsName eq "fordFastis" ) {
                $mistakes = $policy->checkFastis($node);
                last TYPE_INDENT;
            }

            if ( $lhsName eq "fordFaswut" ) {
                $mistakes = $policy->checkFaswut($node);
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

            if ( $lhsName =~ m/optFordFas(hep|lus)/ ) {
                $mistakes = $policy->checkFordHoofRune($lhsName, $node);
                last TYPE_INDENT if @{$mistakes};
                $indentDesc = 'Ford hoof rune';
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
                $mistakes = $policy->checkLuslus( $node, $lhsName );
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
            for my $mistake ( @{$mistakes} ) {
                $mistake->{policy}    = $policyShortName;
                $mistake->{subpolicy} = $mistake->{subpolicy}
                  // $instance->diagName($node);
            }
            $policy->reportMistakes($mistakes);
            last PRINT;
        }

    }

    return;
}

1;

# vim: expandtab shiftwidth=4:
