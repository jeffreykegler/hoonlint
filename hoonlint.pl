# Hoon "tidy" utility

use 5.010;
use strict;
use warnings;
no warnings 'recursion';

use Data::Dumper;
use English qw( -no_match_vars );
use Scalar::Util qw(looks_like_number);
use Getopt::Long;

require "yahc.pm";

my $style;

my $verbose; # right now does nothing
my $censusWhitespace;
my $inclusionsFileName;
my $suppressionsFileName;
my $contextLines = 0;

GetOptions(
    "verbose"             => \$verbose,
    "context|C=i"         => \$contextLines,
    "census-whitespace"   => \$censusWhitespace,
    "inclusions-file|I=s" => \$inclusionsFileName,
    "suppressions_file=s" => \$suppressionsFileName,
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
if ( not defined $suppressionsFileName
    and -f $defaultSuppressionFile )
{
    $suppressionsFileName = $defaultSuppressionFile;
}

sub itemError {
    my ( $error, $line ) = @_;
    return qq{Error in item file "$fileName": $error\n}
    . qq{  Problem with line: $line\n};
}

my $pSuppressions =
  $suppressionsFileName ? slurp($suppressionsFileName) : \"# empty file\n";
my %reportItemType   = map { +( $_, 1 ) } qw(indent sequence);
my ($suppressions, $unusedSuppressions) = parseReportItems($pSuppressions);
die $unusedSuppressions if not $suppressions;

my $pInclusions;
my ( $inclusions, $unusedInclusions );
if ( defined $inclusionsFileName ) {
    $pInclusions = slurp($inclusionsFileName);
    ( $inclusions, $unusedInclusions ) = parseReportItems($pInclusions);
    die $unusedInclusions if not $inclusions;
}

sub parseReportItems {
    my ($reportItems)        = @_;
    my %itemHash       = ();
    my %unusedItemHash = ();
  ITEM: for my $itemLine ( split "\n", ${$reportItems} ) {
        my $rawItemLine = $itemLine;
        $itemLine =~ s/\s*[#].*$//;   # remove comments and preceding whitespace
        $itemLine =~ s/^\s*//;        # remove leading whitespace
        $itemLine =~ s/\s*$//;        # remove trailing whitespace
        next ITEM unless $itemLine;
        my ( $thisFileName, $lc, $type, $message ) = split /\s+/, $itemLine, 4;
        return undef, itemError( "Problem in report line", $rawItemLine )
          if not $thisFileName;

        return undef, itemError( qq{Bad report item type "$type"}, $rawItemLine )
          if not exists $reportItemType{$type};
        return undef, itemError( qq{Malformed line:column in item line: "$lc"},
            $rawItemLine )
          unless $lc =~ /^[0-9]+[:][0-9]+$/;
        my ( $line, $column ) = split ':', $lc, 2;
        itemError( qq{Malformed line:column in item line: "$lc"},
            $rawItemLine )
          unless Scalar::Util::looks_like_number($line)
          and Scalar::Util::looks_like_number($column);
        next ITEM unless $thisFileName eq $fileName;

        # We reassemble line:column to "normalize" it -- be indifferent to
        # leading zeros, etc.
        my $tag = join ':', $line, $column;
        $itemHash{$type}{$tag}         = $message;
        $unusedItemHash{$type}{$tag} = 1;
    }
    return \%itemHash, \%unusedItemHash;
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

my %tallRuneRule = map { +( $_, 1 ) } grep {
         /^tall[B-Z][aeoiu][b-z][b-z][aeiou][b-z]$/
      or /^tall[B-Z][aeoiu][b-z][b-z][aeiou][b-z]Mold$/
} map { $grammar->symbol_name($_); } $grammar->symbol_ids();

# TODO: wisp5d needs study -- may depend on parent
my %tallNoteRule = map { +( $_, 1 ) } qw(
  tallBarhep tallBardot
  tallCendot tallColcab tallColsig
  tallKethep tallKetlus tallKetwut
  tallSigbar tallSigcab tallSigfas tallSiglus
  tallTisbar tallTiscom tallTisgal
  tallWutgal tallWutgar tallWuttis
  tallZapgar wisp5d
  tallTailOfElem tallTailOfTop
);

my %tallBodyRule =
  map { +( $_, 1 ) } grep { not $tallNoteRule{$_} } keys %tallRuneRule;
my %tallSemsigRule =
  map { +( $_, 1 ) } qw(tallCentis tallCencab tallSemcol tallSemsig);
my %tallLuslusRule = map { +( $_, 1 ) } qw(LuslusCell LushepCell LustisCell
  optFordFashep optFordFaslus fordFaswut fordFastis);
my %tallJogRule      = map { +( $_, 1 ) } qw(rick5dJog ruck5dJog);
my %tallBackdentRule = map { +( $_, 1 ) } qw(
  bonz5d
  fordFasbar
  fordFascom
  fordFasdot
  fordFasket
  fordFassem
  tallBarcab
  tallBarcen
  tallBarcol
  tallBarket
  tallBarsig
  tallBartar
  tallBartis
  tallBuccen
  tallBuccenMold
  tallBuccol
  tallBuccolMold
  tallBuchep
  tallBucket
  tallBucketMold
  tallBucpat
  tallBuctisMold
  tallBucwut
  tallBucwutMold
  tallCenhep
  tallCenhepMold
  tallCenket
  tallCenlus
  tallCenlusMold
  tallCensig
  tallCentar
  tallColhep
  tallColket
  tallCollus
  tallColtar
  tallDottar
  tallDottis
  tallKetcen
  tallKettis
  tallSigbuc
  tallSigcen
  tallSiggar
  tallSigpam
  tallSigwut
  tallSigzap
  tallTiscol
  tallTisdot
  tallTisfas
  tallTisgar
  tallTishep
  tallTisket
  tallTislus
  tallTissem
  tallTistar
  tallTiswut
  tallWutbar
  tallWutcol
  tallWutdot
  tallWuthep
  tallWutket
  tallWutlus
  tallWutpam
  tallWutpat
  tallWutsig
  tallZapcol
  tallZapdot
  tallZapwut
);

# say Data::Dumper::Dumper(\%tallBodyRule);

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

my @lineToPos = (-1, 0);
while (${$pHoonSource} =~ m/\n/g) { push @lineToPos, pos ${$pHoonSource} };

sub column {
    my ($pos) = @_;
    return $pos - ( rindex ${$pHoonSource}, "\n", $pos - 1 );
}

sub context2 {
  my ($runeLine, $mistakeLine, $contextLines) = @_;
  return '' if $contextLines <= 0;
  my @pieces = ();
  my $runeRangeStart = $runeLine - ($contextLines - 1);
  my $runeRangeEnd = $runeLine + ($contextLines - 1);
  my $mistakeRangeStart = $mistakeLine - ($contextLines - 1);
  my $mistakeRangeEnd = $mistakeLine + ($contextLines - 1);
  if ($mistakeRangeStart <= $runeRangeEnd + 2) {
    for my $lineNum ( $runeRangeStart .. $mistakeRangeEnd ) {
        my $start = $lineToPos[$lineNum];
        my $line = literal( $start, ( $lineToPos[$lineNum+1] - $start ) );
        my $tag =
            $lineNum == $runeLine        ? '> '
          : ( $lineNum == $mistakeLine ) ? '! '
          :                                q{  };
        push @pieces, $tag, $line;
    }
  } else {
      for my $lineNum ($runeRangeStart .. $runeRangeEnd) {
        my $start = $lineToPos[$lineNum];
        my $line = literal($start, ($lineToPos[$lineNum+1]-$start));
        my $tag = $lineNum == $runeLine ? '> ' : q{  };
        push @pieces, $tag, $line;
      }
      push @pieces, "  [ lines " . ($runeRangeEnd+1) . q{-} . ($mistakeRangeStart-1) . ' omitted ]';
      for my $lineNum ($mistakeRangeStart .. $mistakeRangeEnd) {
        my $start = $lineToPos[$lineNum];
        my $line = literal($start, ($lineToPos[$lineNum+1]-$start));
        my $tag = $lineNum == $mistakeLine ? '! ' : q{  };
        push @pieces, $tag, $line;
      }
  }
  return join q{}, @pieces;
}

sub reportItem {
  my ($topic, $runeLine, $mistakeLine, $contextLines) = @_;
  return "$topic\n" if not $contextLines;
  return join q{}, "==", $topic, "\n", context2($runeLine, $mistakeLine, $contextLines);
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

sub doLint {
    my ( $node, $argContext ) = @_;

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

    my $children = $node->{children};
    my $type = $node->{type};

  LINT_NODE: {

        if ( $inclusions and not $inclusions->{$type}{$parentLC} ) {
          last LINT_NODE;
        }

        # "Policies" will go here
        # As of this writing, these is only one policy -- the "whitespace"
        # policy.

      WHITESPACE_POLICY: {

            # say STDERR "= $type $key\n";
            last WHITESPACE_POLICY if $type eq 'null';
            last WHITESPACE_POLICY if $type eq 'lexeme';
            last WHITESPACE_POLICY if $type eq 'separator';

            my $ruleID = $node->{ruleID};
            die Data::Dumper::Dumper($node) if not defined $ruleID;
            my ( $lhs, @rhs ) = $grammar->rule_expand( $node->{ruleID} );
            my $lhsName = $grammar->symbol_name($lhs);

            $parentContext->{bodyIndent} = $parentColumn
              if $tallBodyRule{$lhsName};

            # TODO: This corresponds to some arvo/ file indentations,
            # but would cause many other discrepancies, including one
            # in toe.hoon
            # $parentContext->{bodyIndent} = $parentColumn+2
            # if $tallLuslusRule{$lhsName};

            $parentContext->{tallRuneIndent} = $parentColumn
              if $tallRuneRule{$lhsName};

            # say STDERR join " ", __FILE__, __LINE__, "lhsName=$lhsName";
            if ( $lhsName eq 'optGay4i' ) {
                last WHITESPACE_POLICY;
            }

            my $childCount = scalar @{$children};
            last WHITESPACE_POLICY if $childCount <= 0;
            if ( $childCount == 1 ) {
                last WHITESPACE_POLICY;
            }

            my $firstChildIndent = column( $children->[0]->{start} ) - 1;

            my $gapiness = $ruleDB[$ruleID]->{gapiness} // 0;

            # TODO: In another policy, warn on tall children of wide nodes
            if ( $gapiness == 0 ) {    # wide node
                last WHITESPACE_POLICY;
            }

            # tall node

            if ( $gapiness < 0 ) {     # sequence
                my ( $parentLine, $parentColumn ) =
                  $recce->line_column($parentStart);
                my $parentLC = join ':', $parentLine, $parentColumn;
                $parentColumn--;       # 0-based
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
                            my $grandParentStart  = $grandParent->{start};
                            ( $grandParentLine, $grandParentColumn ) =
                              $recce->line_column($grandParentStart);
                            $grandParentLC = join ':', $grandParentLine,
                              $grandParentColumn;
                            $grandParentColumn--;    # 0-based
                            my ($lhs) =
                              $grammar->rule_expand($grandParentRuleID);
                            $grandParentName =
                              $grammar->symbol_display_form($lhs);
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
                                  and $symbolReverseDB{$symbol}->{gap};
                                my ( $childLine, $childColumn ) =
                                  $recce->line_column($childStart);
                                my $childLC = join ':', $childLine,
                                  $childColumn;
                                $childColumn--;    # 0-based

                                my $indentDesc = 'RUN';
                              SET_INDENT_DESC: {
                                    my $suppression =
                                      $suppressions->{'sequence'}{$childLC};
                                    if ( defined $suppression ) {
                                        $indentDesc =
                                          "SUPPRESSION $suppression";
                                        $unusedSuppressions->{'sequence'}
                                          {$childLC} = undef;
                                        last SET_INDENT_DESC;
                                    }

                                    if (    $childLine != $previousLine
                                        and $childColumn !=
                                        $grandParentColumn + 2 )
                                    {
                                        $isProblem = 1;
                                        $indentDesc = join " ", $grandParentLC,
                                          $childLC;
                                    }
                                }
                                print reportItem(
"$fileName $grandParentLC sequence $lhsName $indentDesc",
                                    $parentLine, $childLine, $contextLines )
                                  if $censusWhitespace or $isProblem;
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
                          and $symbolReverseDB{$symbol}->{gap};
                        my ( $childLine, $childColumn ) =
                          $recce->line_column($childStart);
                        my $childLC = join ':', $childLine, $childColumn;
                        $childColumn--;    # 0-based

                        my $indentDesc = 'REGULAR';
                      SET_INDENT_DESC: {
                            my $suppression =
                              $suppressions->{'sequence'}{$childLC};
                            if ( defined $suppression ) {
                                $indentDesc = "SUPPRESSION $suppression";
                                $unusedSuppressions->{'sequence'}{$childLC} =
                                  undef;
                                last SET_INDENT_DESC;
                            }

                            if (    $childLine != $previousLine
                                and $childColumn != $parentColumn )
                            {
                                $isProblem = 1;
                                $indentDesc = join " ", $parentLC, $childLC;
                            }
                        }
                        print reportItem(
                            "$fileName $parentLC sequence $lhsName $indentDesc",
                            $parentLine, $childLine, $contextLines )
                          if $censusWhitespace or $isProblem;
                        $previousLine = $childLine;
                    }
                }
                last WHITESPACE_POLICY;
            }

            sub isLuslusStyle {
                my ($indents) = @_;
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

            # Semsig must have first child properly aligned on same line as
            # rune; tistis (==) must be on its own line, aligned with the rune.
            #
            # This is backdenting, with further restrictions:  The first
            # child must be on the rune line, and the last child must NOT
            # be on the rune line.
            sub issemsig {
                my ( $runeLine, $runeColumn, $gapIndents ) = @_;
                my @mistakes = ();
                die "Semsig-style rule with only $gapIndents gap indents"
                  if $#$gapIndents < 3;
                my ( $firstChildLine, $firstChildColumn ) =
                  @{ $gapIndents->[1] };
                if ( $firstChildLine != $runeLine ) {
                    my $msg = sprintf
"Semsig-style Child #%d @ line %d; first child is on line %d; should be on rune line",
                      1, $runeLine, $firstChildLine;
                    push @mistakes,
                      {
                        desc         => $msg,
                        line         => $firstChildLine,
                        column       => $firstChildColumn,
                        child        => 1,
                        expectedLine => $runeLine,
                      };
                }
                if ( $firstChildColumn != $runeColumn + 4 ) {
                    my $msg = sprintf
"Semsig-style Child #%d @ line %d; first child is at column %d; should be at column %d",
                      1, $runeLine, $firstChildColumn, $runeColumn + 4;
                    push @mistakes,
                      {
                        desc           => $msg,
                        line           => $firstChildLine,
                        column         => $firstChildColumn,
                        child          => 1,
                        expectedColumn => $runeColumn + 4,
                      };
                }

                # Second child must be on rune line, or
                # at ruleColumn+2
                my ( $secondChildLine, $secondChildColumn ) =
                  @{ $gapIndents->[2] };

                if (    $secondChildLine != $runeLine
                    and $secondChildColumn != $runeColumn + 2 )
                {
                    my $msg = sprintf
"Semsig-style Child #%d @ line %d; 2nd child is at column %d; should be at column %d",
                      2, $runeLine, $secondChildColumn, $runeColumn + 2;
                    push @mistakes,
                      {
                        desc           => $msg,
                        line           => $secondChildLine,
                        column         => $secondChildColumn,
                        child          => 2,
                        expectedColumn => $runeColumn + 2,
                      };
                }

                my ( $tistisLine, $tistisColumn ) = @{ $gapIndents->[3] };
                if ( $tistisLine == $runeLine ) {
                    my $msg = sprintf
"Semsig-style line %d; TISTIS is on rune line %d; should not be",
                      $runeLine, $tistisLine;
                    push @mistakes,
                      {
                        desc         => $msg,
                        line         => $tistisLine,
                        column       => $tistisColumn,
                        child        => 3,
                        expectedLine => $runeLine,
                      };
                }
                if ( $tistisColumn != $runeColumn ) {
                    my $msg = sprintf
"Semsig-style line %d; TISTIS is at column %d; should be at rune column %d",
                      $runeLine, $tistisColumn, $runeColumn;
                    push @mistakes,
                      {
                        desc           => $msg,
                        line           => $tistisLine,
                        column         => $tistisColumn,
                        child          => 3,
                        expectedColumn => $runeColumn,
                      };
                }
                return \@mistakes;
            }

            sub isJog {
                my ($indents) = @_;
                my @mistakes = ();
                die "Jog rule has "
                  . ( scalar @{$indents} )
                  . "indents; should have 2"
                  if $#$indents != 1;
                my ( $line1, $column1 ) = @{ $indents->[0] };
                my ( $line2, $column2 ) = @{ $indents->[1] };

                # TODO: enforce alignment for "flat jogs"
                return \@mistakes if $line1 == $line2;

                return \@mistakes if $column2 + 2 == $column1;
                my $msg =
                  sprintf
"Jog-style line %d; Child 2 is at column %d; should be at column %d",
                  $line1, $column2, $column1 - 2;
                push @mistakes,
                  {
                    desc           => $msg,
                    line           => $line2,
                    column         => $column2,
                    child          => 2,
                    expectedColumn => $column1 - 2,
                  };
                return \@mistakes;
            }

            sub isBackdented {
                my ( $indents, $baseIndent ) = @_;
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

         # By default, report anomalies in terms of differences from backdenting
         # to the rule start.
            sub defaultMistakes {
                my ( $indents, $type ) = @_;
                my $mistakes = isBackdented($indents);
                return [ { desc => "Undetected mistakes" } ]
                  if not @{$mistakes};
                for my $mistake ( @{$mistakes} ) {
                    my $mistakeChild  = $mistake->{child};
                    my $mistakeColumn = $mistake->{column};
                    my $defaultColumn = $mistake->{backdentColumn};
                    my $mistakeLine   = $mistake->{line};
                    my $msg           = sprintf
                      "$type child #%d; line %d; indent=%d vs. default of %d",
                      $mistakeChild, $mistakeLine, $mistakeColumn,
                      $defaultColumn;
                    $mistake->{desc} = $msg;
                }
                return $mistakes;
            }

            sub isFlat {
                my ($indents)   = @_;
                my ($firstLine) = @{ $indents->[0] };
                my ($lastLine)  = @{ $indents->[$#$indents] };
                return $firstLine == $lastLine;
            }

            my $displayMistakes = sub {

                # say join " ", __FILE__, __LINE__, "displayMistakes()";
                my ($mistakes) = @_;
                my @pieces = ();
              MISTAKE: for my $mistake ( @{$mistakes} ) {

                    # say join " ", __FILE__, __LINE__, "displayMistakes()";
                    my $desc        = $mistake->{desc};
                    my $type        = $mistake->{type};
                    my $mistakeLine = $mistake->{line};
                    push @pieces,
                      reportItem(
                        (
                                "$fileName "
                              . ( join ':', $parentLine, $parentColumn + 1 )
                              . " $type $lhsName $desc"
                        ),
                        $parentLine,
                        $mistakeLine,
                        $contextLines
                      );
                }
                return join "", @pieces;
            };

          # say STDERR __LINE__, " parentIndents: ", (join " ", @parentIndents);
          # if here, gapiness > 0
            {
                my $mistakes = [];
                my $start    = $node->{start};

                my $indentDesc = '???';

                my @gapIndents = ();
                {
                    my $child      = $children->[0];
                    my $childStart = $child->{start};
                    my ( $childLine, $childColumn ) =
                      $recce->line_column($childStart);
                    push @gapIndents, [ $childLine, $childColumn - 1 ];
                    for my $childIX ( 0 .. ( $#$children - 1 ) ) {
                        my $child  = $children->[$childIX];
                        my $symbol = $child->{symbol};
                        if ( defined $symbol
                            and $symbolReverseDB{$symbol}->{gap} )
                        {
                            my $nextChild = $children->[ $childIX + 1 ];
                            my $nextStart = $nextChild->{start};
                            my ( $nextLine, $nextColumn ) =
                              $recce->line_column($nextStart);
                            push @gapIndents, [ $nextLine, $nextColumn - 1 ];
                        }
                    }
                }

              TYPE_INDENT: {

                    my $suppression = $suppressions->{'indent'}{$parentLC};
                    if ( defined $suppression ) {
                        $indentDesc = "SUPPRESSION $suppression";
                        $unusedSuppressions->{'indent'}{$parentLC} = undef;
                        last TYPE_INDENT;
                    }

                    if ( isFlat( \@gapIndents ) ) {
                        $indentDesc = 'FLAT';
                        last TYPE_INDENT;
                    }

                    if ( $tallJogRule{$lhsName} ) {
                        $mistakes = isJog( \@gapIndents );
                        last TYPE_INDENT if @{$mistakes};
                        $indentDesc = 'JOG-STYLE';
                        last TYPE_INDENT;
                    }

                    if ( $tallSemsigRule{$lhsName} ) {
                        $mistakes =
                          issemsig( $parentLine, $parentColumn, \@gapIndents );
                        last TYPE_INDENT if @{$mistakes};
                        $indentDesc = 'SEMSIG-STYLE';
                        last TYPE_INDENT;
                    }

                    if ( $tallNoteRule{$lhsName} ) {
                        $mistakes = isBackdented( \@gapIndents, $noteIndent );
                        last TYPE_INDENT if @{$mistakes};
                        $indentDesc = 'CAST-STYLE';
                        last TYPE_INDENT;
                    }

                    if ( $tallLuslusRule{$lhsName} ) {
                        $mistakes = isLuslusStyle( \@gapIndents );
                        last TYPE_INDENT if @{$mistakes};
                        $indentDesc = 'LUSLUS-STYLE';
                        last TYPE_INDENT;
                    }

                    # By default, treat as backdented
                    $mistakes = isBackdented( \@gapIndents );
                    if ( not @{$mistakes} ) {
                        $indentDesc = 'BACKDENTED';
                        last TYPE_INDENT;
                    }

                }

              PRINT: {
          # say join " ", __FILE__, __LINE__, "$lhsName", (scalar @{$mistakes});
                    if ( @{$mistakes} ) {
                        $_->{type} = 'indent' for @{$mistakes};
                        print $displayMistakes->($mistakes);
                        last PRINT;
                    }

                    if ($censusWhitespace) {
                        print reportItem( "$fileName "
                              . ( join ':', $recce->line_column($start) )
                              . " indent $lhsName $indentDesc",
                            $parentLine, $parentLine, $contextLines );
                    }
                }
            }
        }    # End of whitespace "policy"
    }

  CHILD: for my $childIX ( 0 .. $#$children ) {
        my $child = $children->[$childIX];
        doLint( $child, $parentContext );
    }
}

doLint( $astValue, { line => -1, indents => [], ancestors => [] } );

for my $type ( keys %{$unusedSuppressions} ) {
    for my $tag (
        grep { $unusedSuppressions->{$type}{$_} }
        keys %{ $unusedSuppressions->{$type} }
      )
    {
        say "Unused suppression: $type $tag";
    }
}

# vim: expandtab shiftwidth=4:
