# Hoon "tidy" utility

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );
use Scalar::Util qw(looks_like_number);
use Getopt::Long;

require "yahc.pm";

my $style;

my $roundtrip;
my $verbose;
my $censusWhitespace;
my $suppressionFileName;

GetOptions(
    "roundtrip"           => \$roundtrip,
    "verbose"             => \$verbose,
    "census-whitespace"   => \$censusWhitespace,
    "suppressions_file=s" => \$suppressionFileName,
) or die("Error in command line arguments\n");

sub usage {
    die "usage: $PROGRAM_NAME [options ...] fileName\n";
}

usage() if scalar @ARGV != 1;
my $fileName = $ARGV[0];

sub slurp {
    my ($fileName) = @_;
    local $RS = undef;
    my $fh;
    open $fh, q{<}, $fileName or die "Cannot open $fileName";
    my $file = <$fh>;
    close $fh;
    return \$file;
}

my $defaultSuppressionFile = 'hoonlint.suppressions';
if ( not defined $suppressionFileName
    and -f $defaultSuppressionFile )
{
    $suppressionFileName = $defaultSuppressionFile;
}

sub suppressionError {
    my ( $error, $line ) = @_;
    say STDERR qq{Error in suppression file "$suppressionFileName": $error};
    say STDERR qq{Error in suppression file "$suppressionFileName"};
}

my $pSuppressions =
  $suppressionFileName ? slurp($suppressionFileName) : \"# empty file\n";
my %suppressionType   = map { +( $_, 1 ) } qw(indent sequence);
my %suppression       = ();
my %unusedSuppression = ();
SUPPRESSION: for my $suppression ( split "\n", ${$pSuppressions} ) {
    my $rawSuppression = $suppression;
    $suppression =~ s/\s*[#].*$//;    # remove comments and preceding whitespace
    $suppression =~ s/^\s*//;         # remove leading whitespace
    $suppression =~ s/\s*$//;         # remove trailing whitespace
    next SUPPRESSION unless $suppression;
    my ( $thisFileName, $lc, $type, $message ) = split /\s+/, $suppression, 4;
    suppressionError( "Problem in suppression line", $rawSuppression )
      if not $thisFileName;

    # "all" is only suppression type currently allowed
    suppressionError( qq{Bad suppression type "$type"}, $rawSuppression )
      if not exists $suppressionType{$type};
    suppressionError( qq{Malformed line:column in suppression line: "$lc"},
        $rawSuppression )
      unless $lc =~ /^[0-9]+[:][0-9]+$/;
    my ( $line, $column ) = split ':', $lc, 2;
    suppressionError( qq{Malformed line:column in suppression line: "$lc"},
        $rawSuppression )
      unless Scalar::Util::looks_like_number($line)
      and Scalar::Util::looks_like_number($column);
    next SUPPRESSION unless $thisFileName eq $fileName;

    # We reassemble line:column to "normalize" it -- be indifferent to
    # leading zeros, etc.
    my $tag = join ':', $line, $column;
    $suppression{$type}{$tag}       = $message;
    $unusedSuppression{$type}{$tag} = 1;
}

my $pHoonSource = slurp($fileName);

my @data = ();
my $recce;

my $semantics = <<'EOS';
:default ::= action=>main::doNode
lexeme default = latm => 1 action=>[start,length,name]
EOS

my $parser = MarpaX::YAHC::new( { semantics => $semantics, all_symbols => 1 } );
my $dsl = $parser->dsl();
my $grammar = $parser->rawGrammar();

# Preferred runes for alignment.  At this point all the tall ones
# except CENDOT (%.); KETHEP (^-); KETLUS (^+); SIGBAR (~|).
my %tallRuneRule = map { +( $_, 1 ) } grep {
         /^tall[B-Z][aeoiu][b-z][b-z][aeiou][b-z]$/
      or /^tall[B-Z][aeoiu][b-z][b-z][aeiou][b-z]Mold$/
} map { $grammar->symbol_name($_); } $grammar->symbol_ids();

my %tallAnnotationRule = map { +( $_, 1 ) }
  qw(tallCendot tallKethep tallKetdot tallKetlus tallSigbar tallSiglus);
my %tallMainRule =
  map { +( $_, 1 ) } grep { not $tallAnnotationRule{$_} } keys %tallRuneRule;
my %tallSemsigRule = map { +( $_, 1 ) } qw(tallSemsig);
my %tallLuslusRule = map { +( $_, 1 ) } qw(lusLusCell lusHepCell);
my %tallJogRule = map { +( $_, 1 ) } qw(rick5dJog ruck5dJog);

# say Data::Dumper::Dumper(\%tallMainRule);

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
    my @results    = ();
    my $childCount = scalar @children;
    no warnings 'once';
    my $ruleID = $Marpa::R2::Context::rule;
    use warnings;
    my ( $lhs, @rhs ) =
      map { $grammar->symbol_display_form($_) } $grammar->rule_expand($ruleID);
    my ( $first_g1, $last_g1 ) = Marpa::R2::Context::location();
    my ($lhsStart) = $recce->g1_location_to_span( $first_g1 + 1 );

    if ( $childCount <= 0 ) {
        return {
            type   => 'null',
            symbol => $lhs,
            start  => $lhsStart,
            length => 0,
        };
    }
    my ( $last_g1_start, $last_g1_length ) =
      $recce->g1_location_to_span($last_g1);
    my $lhsLength = $last_g1_start + $last_g1_length - $lhsStart;
  RESULT: {
      CHILD: for my $childIX ( 0 .. $#children ) {
            my $child   = $children[$childIX];
            my $refType = ref $child;
            next CHILD unless $refType eq 'ARRAY';

            # say STDERR Data::Dumper::Dumper( $children[$childIX] );
            my ( $lexemeStart, $lexemeLength, $lexemeName ) = @{$child};

            if ( $lexemeName eq 'TRIPLE_DOUBLE_QUOTE_STRING' ) {
                my $terminator = q{"""};
                my $terminatorPos = index ${$pHoonSource}, $terminator,
                  $lexemeStart + $lexemeLength;
                $lexemeLength =
                  $terminatorPos + ( length $terminator ) - $lexemeStart;
            }
            if ( $lexemeName eq 'TRIPLE_QUOTE_STRING' ) {
                my $terminator = q{'''};
                my $terminatorPos = index ${$pHoonSource}, $terminator,
                  $lexemeStart + $lexemeLength;
                $lexemeLength =
                  $terminatorPos + ( length $terminator ) - $lexemeStart;
            }
            $children[$childIX] = {
                type   => 'lexeme',
                start  => $lexemeStart,
                length => $lexemeLength,
                symbol => $lexemeName,
            };
        }

        my $lastLocation = $lhsStart;
        if ( ( scalar @rhs ) != $childCount ) {

          # This is a non-trivial (that is, longer than one item) sequence rule.
            my $childIX = 0;
            my $lastSeparator;
          CHILD: for ( ; ; ) {

                my $child     = $children[$childIX];
                my $childType = $child->{type};
                $childIX++;
              ITEM: {
                    if ( defined $lastSeparator ) {
                        my $length = $child->{start} - $lastSeparator->{start};
                        $lastSeparator->{length} = $length;
                    }
                    push @results, $child;
                    $lastLocation = $child->{start} + $child->{length};
                }
                last RESULT if $childIX > $#children;
                my $separator = $separator{$lhs};
                next CHILD unless $separator;
                $lastSeparator = {
                    type   => 'separator',
                    symbol => $separator,
                    start  => $lastLocation,

                    # length supplied later
                };
                push @results, $lastSeparator;
            }
            last RESULT;
        }

        # All other rules
      CHILD: for my $childIX ( 0 .. $#children ) {
            my $child = $children[$childIX];
            push @results, $child;
        }
    }
    return {
        type     => 'node',
        ruleID   => $ruleID,
        start    => $lhsStart,
        length   => $lhsLength,
        children => \@results,
    };
}

$parser->read($pHoonSource);
$recce  = $parser->rawRecce();
$parser = undef;                 # free up memory
my $astRef = $recce->value();

sub literal {
    my ( $start, $length ) = @_;
    return substr ${$pHoonSource}, $start, $length;
}

sub column {
    my ($pos) = @_;
    return $pos - ( rindex ${$pHoonSource}, "\n", $pos - 1 );
}

# The "name" of a node
sub name {
    my ($node) = @_;
    my $name = $node->{symbol};
    return $name if defined $name;
    my $type = $node->{type};
    if ( $type eq 'node' ) {
        my $ruleID = $node->{ruleID};
        my ( $lhs, @rhs ) = $grammar->rule_expand($ruleID);
        return $grammar->symbol_name($lhs) . '#' . ( scalar @rhs );
    }
    return "[$type]";
}

die "Parse failed" if not $astRef;

local $Data::Dumper::Deepcopy = 1;
local $Data::Dumper::Terse    = 1;

# say Data::Dumper::Dumper($astRef);

my $astValue = ${$astRef};

# Determine how many spaces we need.
# Arguments are an array of strings (intended
# to be concatenated) and an integer, representing
# the number of spaces needed by the app.
# (For hoon this will always between 0 and 2.)
# Hoon's notation of spacing, in which a newline is equivalent
# a gap and therefore two spaces, is used.
#
# Return value is the number of spaces needed after
# the trailing part of the argument string array is
# taken into account.  It is always less than or
# equal to the `spacesNeeded` argument.
sub spacesNeeded {
    my ( $strings, $spacesNeeded ) = @_;
    for ( my $arrayIX = $#$strings ; $arrayIX >= 0 ; $arrayIX-- ) {

 # say STDERR join " ", __FILE__, __LINE__, "tab command: needed=$spacesNeeded";
        my $string = $strings->[$arrayIX];

       # say STDERR join " ", __FILE__, __LINE__, "tab command: string=$string";
       # say STDERR +(join " ", __FILE__, __LINE__, ''), (length $string);
        for (
            my $stringIX = ( length $string ) - 1 ;
            $stringIX >= 0 ;
            $stringIX--
          )
        {
# say STDERR join " ", __FILE__, __LINE__, "tab command: stringIX=$stringIX; needed=$spacesNeeded";
            my $char = substr $string, $stringIX, 1;
            return 0 if $char eq "\n";
            return $spacesNeeded if $char ne q{ };
            $spacesNeeded--;
            return 0 if $spacesNeeded <= 0;
        }
    }

    # No spaces needed at beginning of string;
    return 0;
}

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
        $data->{lexeme} = 1;                     # default to lexeme
        $data->{gap}    = 1 if $name eq 'GAP';
        $data->{gap}    = 1
          if $name =~ m/^[B-Z][AEOIU][B-Z][B-Z][AEIOU][B-Z]GAP$/;
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
            if ( $separatorID == $gapID ) {
                $data->{gapiness} = -1;
            }
        }
        if ( not defined $data->{gapiness} ) {
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

my @pieces = ();

sub doCensus {
    no warnings 'recursion';
    my ( $baseIndent, $depth, $node, $argContext ) = @_;

    # say STDERR "doCensus($baseIndent, $depth, ...)";

    my $gapToPieces = sub {
        my ( $start, $length ) = @_;
        my $literal = literal( $start, $length );
        my $currentNL = index $literal, "\n";
        if ( $currentNL < 0 ) {

            # gap must be at least 2 spaces
            push @pieces, { type => 'tab', indent => $baseIndent, needed => 2 };
            return;
        }
        my $lastNL = -1;

        # Convert initialColumn to 0-based
        my $initialColumn = column($start) - 1;
      LEADING_LINES: for ( ; ; ) {
            pos $literal = $lastNL + 1;
            my ($spaces) = ( $literal =~ m/\G([ ]*)/ );
            die if not defined $spaces;    # TODO: is this necessary?
                                           # say STDERR qq{spaces="$spaces"};
            my $spaceCount      = length $spaces;
            my $firstCommentPos = $lastNL + $spaceCount + 1;
            if ( substr( $literal, $firstCommentPos, 1 ) ne "\n" ) {

                # say STDERR "pushing tab, indent=",
                # $initialColumn + $spaceCount;
                push @pieces,
                  {
                    type   => 'tab',
                    indent => ( $initialColumn + $spaceCount ),
                    needed => 1
                  };

                # say STDERR +( join " ", __FILE__, __LINE__, '' ),
                # qq{pushing piece: "},
                # substr( $literal, $firstCommentPos,
                # ( $currentNL - $firstCommentPos ) ),
                # q{"};
                push @pieces,
                  {
                    type => 'text',
                    text => substr(
                        $literal, $firstCommentPos,
                        ( $currentNL - $firstCommentPos )
                    )
                  };
            }
            my $nextNL = index $literal, "\n", $currentNL + 1;
            last LEADING_LINES if $nextNL < 0;
            push @pieces, { type => 'nl' };
            $lastNL        = $currentNL;
            $currentNL     = $nextNL;
            $initialColumn = 0;
        }
        push @pieces, { type => 'nl' },
          { type => 'tab', indent => $baseIndent };
        return;
    };

    my $parentSymbol = $node->{symbol};
    my $parentStart  = $node->{start};
    my $parentLength = $node->{length};
    my $parentRuleID = $node->{ruleID};
    my ( $parentLine, $parentColumn ) = $recce->line_column($parentStart);
    my $parentLC = join ':', $parentLine, $parentColumn;
    $parentColumn--;    # 0-based

    my @parentIndents = @{ $argContext->{indents} };

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

    my $argPreferredIndent    = $argContext->{preferredIndent};
    my $argTallRuneIndent    = $argContext->{tallRuneIndent};
    my $parentPreferredIndent;
    $parentPreferredIndent = $argPreferredIndent if $argLine == $parentLine;
    my $parentTallRuneIndent;
    $parentTallRuneIndent = $argTallRuneIndent if $argLine == $parentLine;
    my $parentContext         = {
        ancestors => \@ancestors,
        line      => $parentLine,
        indents   => [@parentIndents],
    };
    $parentContext->{preferredIndent} = $parentPreferredIndent if defined $parentPreferredIndent;
    $parentContext->{tallRuneIndent} = $parentTallRuneIndent if defined $parentTallRuneIndent;

  NODE: {
        die Data::Dumper::Dumper($node)
          if ref $node ne 'HASH';    # TODO: delete after development
        my $type = $node->{type};

        # say STDERR "= $type $key\n";
        if ( $type eq 'null' ) {
            last NODE;
        }
        if ( $type eq 'lexeme' ) {
            if ( $parentSymbol eq 'GAP' ) {
                $gapToPieces->( $parentStart, $parentLength );
                last NODE;
            }
            if ( $parentSymbol =~ m/^[B-Z][AEOIU][B-Z][B-Z][AEIOU][B-Z]GAP$/ ) {
                push @pieces,
                  { type => 'text', text => literal( $parentStart, 2 ) };
                $gapToPieces->( $parentStart + 2, $parentLength - 2 );
                last NODE;
            }

# say STDERR +(join " ", __FILE__, __LINE__, ''), qq{pushing piece: "}, literal( $parentStart, $parentLength ), q{"};
            push @pieces,
              {
                type => 'text',
                text => literal( $parentStart, $parentLength )
              };
            last NODE;
        }
        if ( $type eq 'separator' ) {
            if ( $parentSymbol eq 'GAP' ) {

                $gapToPieces->( $parentStart, $parentLength );
                last NODE;
            }
            push @pieces,
              {
                type => 'text',
                text => literal( $parentStart, $parentLength )
              };
            last NODE;
        }
        my $ruleID = $node->{ruleID};
        die Data::Dumper::Dumper($node) if not defined $ruleID;
        my ( $lhs, @rhs ) = $grammar->rule_expand( $node->{ruleID} );
        my $lhsName = $grammar->symbol_name($lhs);

        $parentContext->{preferredIndent} = $parentColumn if $tallMainRule{$lhsName};
        $parentContext->{tallRuneIndent} = $parentColumn if $tallRuneRule{$lhsName};

        # say STDERR join " ", __FILE__, __LINE__, "lhsName=$lhsName";
        if ( $lhsName eq 'optGay4i' ) {
            $gapToPieces->( $parentStart, $parentLength );
            last NODE;
        }

        my $children   = $node->{children};
        my $childCount = scalar @{$children};
        last NODE if $childCount <= 0;
        if ( $childCount == 1 ) {
            doCensus( $baseIndent, $depth + 1, $children->[0], $parentContext );
            last NODE;
        }

        my $firstChildIndent = column( $children->[0]->{start} ) - 1;

# say STDERR join " ", "$lhsName: column=$firstChildIndent", "baseIndent=$baseIndent";
        $baseIndent = $firstChildIndent if $firstChildIndent > $baseIndent;

        my $gapiness = $ruleDB[$ruleID]->{gapiness} // 0;

        # TODO: Add warning for tall children of wide nodes
        if ( $gapiness == 0 ) {    # wide node
            for my $child (@$children) {
                doCensus( $baseIndent, $depth + 1, $child, $parentContext );
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

        if ( $gapiness < 0 ) {    # sequence
            my ( $parentLine, $parentColumn ) =
              $recce->line_column($parentStart);
            my $parentLC = join ':', $parentLine, $parentColumn;
            $parentColumn--;      # 0-based
            my $previousLine = $parentLine;
          TYPE_INDENT: {

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
                        my $grandParentStart = $grandParent->{start};
                        ( $grandParentLine, $grandParentColumn ) =
                          $recce->line_column($grandParentStart);
                        $grandParentLC = join ':', $grandParentLine, $grandParentColumn;
                        $grandParentColumn--; # 0-based
                        my ($lhs) = $grammar->rule_expand($grandParentRuleID);
                        $grandParentName = $grammar->symbol_display_form($lhs);
                    }
                    if ( $grandParentName eq 'tallSemsig' ) {

                      $previousLine = $grandParentLine;
                      CHILD: for my $childIX ( 0 .. $#$children ) {
                            my $isProblem = 0;
                            my $child     = $children->[$childIX];
                            doCensus(
                                $baseIndent, $depth + 1,
                                $child,      $parentContext
                            );
                            my $childStart = $child->{start};
                            my $symbol     = $child->{symbol};
                            next CHILD
                              if defined $symbol
                              and $symbolReverseDB{$symbol}->{gap};
                            my ( $childLine, $childColumn ) =
                              $recce->line_column($childStart);
                            my $childLC = join ':', $childLine, $childColumn;
                            $childColumn--;    # 0-based

                            my $indentDesc = 'RUN';
                          SET_INDENT_DESC: {
                                my $suppression =
                                  $suppression{'sequence'}{$childLC};
                                if ( defined $suppression ) {
                                    $indentDesc = "SUPPRESSION $suppression";
                                    $unusedSuppression{'sequence'}{$childLC} =
                                      undef;
                                    last SET_INDENT_DESC;
                                }

                                if (    $childLine != $previousLine
                                    and $childColumn != $grandParentColumn + 2 )
                                {
                                    $isProblem = 1;
                                    $indentDesc = join " ", $grandParentLC, $childLC;
                                }
                            }
                            say
"SEQUENCE $lhsName $indentDesc # $fileName L$grandParentLC"
                              if $censusWhitespace or $isProblem;
                            $previousLine = $childLine;
                        }

                        last TYPE_INDENT;
                    }
                }

              CHILD: for my $childIX ( 0 .. $#$children ) {
                    my $isProblem = 0;
                    my $child     = $children->[$childIX];
                    doCensus( $baseIndent, $depth + 1, $child, $parentContext );
                    my $childStart = $child->{start};
                    my $symbol     = $child->{symbol};
                    next CHILD
                      if defined $symbol
                      and $symbolReverseDB{$symbol}->{gap};
                    my ( $childLine, $childColumn ) =
                      $recce->line_column($childStart);
                    my $childLC = join ':', $childLine, $childColumn;
                    $childColumn--;    # 0-based

                    my $indentDesc = 'REGULAR';
                  SET_INDENT_DESC: {
                        my $suppression = $suppression{'sequence'}{$childLC};
                        if ( defined $suppression ) {
                            $indentDesc = "SUPPRESSION $suppression";
                            $unusedSuppression{'sequence'}{$childLC} = undef;
                            last SET_INDENT_DESC;
                        }

                        if (    $childLine != $previousLine
                            and $childColumn != $parentColumn )
                        {
                            $isProblem = 1;
                            $indentDesc = join " ", $parentLC, $childLC;
                        }
                    }
                    say "SEQUENCE $lhsName $indentDesc # $fileName L$parentLC"
                      if $censusWhitespace or $isProblem;
                    $previousLine = $childLine;
                }
            }
            last NODE;
        }

        sub isluslusstyle {
            my ( $baseLine, $baseColumn, $indents ) = @_;

            # say join " ", __FILE__, __LINE__, "L$baseLine:$baseColumn";
            my $indentCount = scalar @{$indents};
            my $indentIX    = 0;
          INDENT: while ( $indentIX < $indentCount ) {
                my ( $thisLine, $thisColumn ) = @{ $indents->[$indentIX] };

                # say join " ", __FILE__, __LINE__, "L$thisLine vs. $baseLine";
                last INDENT if $thisLine != $baseLine;
                $indentIX++;
            }
          INDENT: while ( $indentIX < $indentCount ) {
                my ( $thisLine, $thisColumn ) = @{ $indents->[$indentIX] };

             # say join " ", __FILE__, __LINE__, "L$thisColumn vs. $baseColumn";
                return 0 if $thisColumn != $baseColumn + 2;
                $indentIX++;
            }
            return 1;
        }

        # Semsig must have first child properly aligned on same line as
        # rune; tistis (==) must be on its own line, aligned with the rune.
        sub issemsig {
            my ( $baseLine, $baseColumn, $indents ) = @_;
            my ( $firstChildLine, $firstChildColumn ) = @{ $indents->[2] };
            return 0 if $firstChildLine != $baseLine or $firstChildColumn != $baseColumn + 4;
            my ( $tistisLine, $tistisColumn ) = @{ $indents->[4] };
            return 0 if $tistisLine == $baseLine or $tistisColumn != $baseColumn;
            return 1;
        }

        sub isjog {
            my ( $baseLine, $baseColumn, $indents ) = @_;
            my ( $line1, $column1 ) = @{ $indents->[0] };
            my ( $line2, $column2 ) = @{ $indents->[1] };

            # TODO: enforce alignment for "flat jogs"
            return 1 if $line1 == $line2;

            # say "lc1: ( $line1, $column1, baseColumn: $baseColumn )";
            # say "lc2: ( $line2, $column2 )";
            return 1 if $column2 + 2 == $column1;
            return 0;
        }

        sub isbackdented {
            my ( $baseLine, $baseColumn, $verticalGaps, $indents ) = @_;

            # say "L$baseLine isbackdented for column $baseColumn";

            my $currentIndent = $baseColumn + $verticalGaps * 2;
            my $lastLine      = $baseLine;
          INDENT: for my $indent ( @{$indents} ) {
                my ( $thisLine, $thisColumn ) = @{$indent};
                next INDENT if $thisLine == $lastLine;
                $currentIndent -= 2;
                $lastLine = $thisLine;

                # say "L$lastLine $thisColumn vs. $currentIndent";
                if ( $currentIndent != $thisColumn ) {
                    return;
                }
            }
            return 1;
        }

        # say STDERR __LINE__, " parentIndents: ", (join " ", @parentIndents);
        # if here, gapiness > 0
        {
            my $isProblem = 0;
            my $start     = $node->{start};
            my @indents   = ();
          CHILD: for my $childIX ( 0 .. $#$children ) {
                my $child      = $children->[$childIX];
                my $childStart = $child->{start};
                my $symbol     = $child->{symbol};
                next CHILD
                  if defined $symbol and $symbolReverseDB{$symbol}->{gap};
                my ( $childLine, $childColumn ) =
                  $recce->line_column($childStart);
                push @indents, [ $childLine, $childColumn - 1 ];
            }
            my $indentDesc = '???';
          TYPE_INDENT: {

                my $suppression = $suppression{'indent'}{$parentLC};
                if ( defined $suppression ) {
                    $indentDesc = "SUPPRESSION $suppression";
                    $unusedSuppression{'indent'}{$parentLC} = undef;
                    last TYPE_INDENT;
                }

                if ($vertical_gaps <= 0) {
                    $indentDesc = 'FLAT';
                    last TYPE_INDENT;
                }

                if ($tallJogRule{$lhsName}) {
                    if ( isjog( $parentLine, $parentColumn, \@indents ) ) {
                        $indentDesc = 'JOG-STYLE';
                        last TYPE_INDENT;
                    }
                    $isProblem = 1;
                }

                if ($tallSemsigRule{$lhsName}) {
                    if (
                        issemsig($parentLine, $parentColumn, \@indents))
                    {
                        $indentDesc = 'SEMSIG-STYLE';
                        last TYPE_INDENT;
                    }
                    $isProblem = 1;
                    $indentDesc = join " ", map { join ':', @{$_} } @indents;
                    last TYPE_INDENT;
                }

                if ($tallAnnotationRule{$lhsName}) {

                   # align with preferred indent from ancestor, if there is one,
                   # with parent otherwise
                    my $indent = ($parentPreferredIndent // $parentTallRuneIndent) // $parentColumn;
                    if (
                        isbackdented(
                            $parentLine, $indent, $vertical_gaps, \@indents
                        )
                      )
                    {
                        $indentDesc = 'CAST-STYLE';
                        last TYPE_INDENT;
                    }
                    $isProblem = 1;

                }
                if ($tallLuslusRule{$lhsName}) {
                    if (
                        isluslusstyle( $parentLine, $parentColumn, \@indents ) )
                    {
                        $indentDesc = 'LUSLUS-STYLE';
                        last TYPE_INDENT;
                    }
                    $isProblem = 1;
                }
                for (
                    my $indentIX = $#parentIndents ;
                    $indentIX >= 0 ;
                    $indentIX--
                  )
                {
                    my $indent = $parentIndents[$indentIX];

                   # say "L$parentLine: L$parentLine trying backdent ; $indent";
                    if (
                        isbackdented(
                            $parentLine, $indent, $vertical_gaps, \@indents
                        )
                      )
                    {
                        my $depth = $#parentIndents - $indentIX;
                        if ($depth) {
                            $indentDesc = "BACKDENTED-$depth";
                            $isProblem  = 1;
                            last TYPE_INDENT;
                        }
                        $indentDesc = 'BACKDENTED';
                        last TYPE_INDENT;
                    }
                }

                $isProblem = 1;
                my $startOfLine = 1 + rindex ${$pHoonSource}, "\n", $start;
                my $lineLiteral = substr ${$pHoonSource},
                  $startOfLine, $start - $startOfLine;

                # say qq{lineLiteral: "$lineLiteral"};
                my ($spaces) = ( $lineLiteral =~ m/^([ ]*)/ );
                if (
                    isbackdented(
                        $parentLine, ( length $spaces ),
                        $vertical_gaps, \@indents
                    )
                  )
                {
                    $indentDesc = 'LINE-BACKDENTED';
                    last TYPE_INDENT;
                }
                if ( isluslusstyle( $parentLine, $parentColumn, \@indents ) ) {

                    # luslus style for non-luslus rules
                    $indentDesc = 'LUSLUS-STYLE';
                    last TYPE_INDENT;
                }
                $indentDesc = join " ", map { join ':', @{$_} } @indents;
            }
            say "FIXED-$gapiness $lhsName $indentDesc",
              " # $fileName L", ( join ':', $recce->line_column($start) )
              if $censusWhitespace or $isProblem;
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

        # If here, use backdenting
        my $currentIndent = $baseIndent + $vertical_gaps * 2;
      CHILD: for my $childIX ( 0 .. $#$children ) {
            $currentIndent -= 2 if $isVerticalGap[$childIX];
            my $child = $children->[$childIX];
            doCensus( $currentIndent, $depth + 1, $child, $parentContext );
        }
    }

    return $baseIndent;
}

doCensus( 0, 0, $astValue, { line => -1, indents => [], ancestors => [] } );
$grammar = undef;    # free up memory
$recce   = undef;    # free up memory

my @output        = ();
my $currentColumn = 0;

PIECE: for my $piece (@pieces) {
    my $type = $piece->{type};
    if ( $type eq 'tab' ) {
        my $spaces = $piece->{indent} - $currentColumn;
        my $needed = $piece->{needed} // 0;

        # say STDERR "tab command: spaces=$spaces; needed=$needed";
        if ( $spaces < 0 ) {

       # say STDERR join " ", __FILE__, __LINE__, "tab command: needed=$needed";
            $needed = spacesNeeded( \@output, $needed );

       # say STDERR join " ", __FILE__, __LINE__, "tab command: needed=$needed";
            $currentColumn += $needed;
            push @output, ( q{ } x $needed ) if $needed > 0;
            next PIECE;
        }
        $spaces += spacesNeeded( \@output, $needed - $spaces )
          if $needed > $spaces;
        $currentColumn += $spaces;
        push @output, ( q{ } x $spaces ) if $spaces > 0;
        next PIECE;
    }
    if ( $type eq 'nl' ) {

        # say STDERR "processing nl, indent=$indent";
        my $indent = $piece->{indent};
        push @output, "\n";
        $currentColumn = 0;
        next PIECE;
    }
    if ( $type eq 'text' ) {
        my $text = $piece->{text};
        push @output, $text;
        $currentColumn += length $text;
        next PIECE;
    }
    die qq{Unimplemented piece type: }, Data::Dumper::Dumper($piece);
}
if ($roundtrip) {
    print join q{}, @output;
}

for my $type ( keys %unusedSuppression ) {
    for my $tag (
        grep { $unusedSuppression{$type}{$_} }
        keys %{ $unusedSuppression{$type} }
      )
    {
        say "Unused suppression: $type $tag";
    }
}

# vim: expandtab shiftwidth=4:
