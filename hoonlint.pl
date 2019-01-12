# Hoon "tidy" utility

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );
use Getopt::Long;

require "yahc.pm";

my $style;

my $roundtrip;
my $verbose;

GetOptions(
    "roundtrip"  => \$roundtrip,
    "verbose"  => \$verbose
) or die("Error in command line arguments\n");

sub usage {
    die "usage: $PROGRAM_NAME [-v] fileName\n";
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

my $pHoonSource = slurp($fileName);

my @data = ();
my $grammar;
my $recce;

# Preferred runes for alignment.  At this point all the tall ones
# except KETHEP (^-);
my @alignables = qw(
tallBarcab
tallBarcen
tallBarcol
tallBardot
tallBarhep
tallBarket
tallBarsig
tallBartar
tallBartis
tallBarwut
tallBuccab
tallBuccabMold
tallBuccen
tallBuccenMold
tallBuccol
tallBuccolMold
tallBuchep
tallBuchepMold
tallBucket
tallBucketMold
tallBucpat
tallBucpatMold
tallBucsem
tallBucsemMold
tallBuctis
tallBuctisMold
tallBucwut
tallBucwutMold
tallCencab
tallCencolMold
tallCendot
tallCenhep
tallCenhepMold
tallCenket
tallCenketMold
tallCenlus
tallCenlusMold
tallCensig
tallCentar
tallCentis
tallColcab
tallColhep
tallColket
tallCollus
tallColsig
tallColtar
tallDotket
tallDotlus
tallDottar
tallDottis
tallDotwut
tallKetbar
tallKetcen
tallKetdot
tallKetpam
tallKetsig
tallKettis
tallKetwut
tallSemcol
tallSemfas
tallSemsem
tallSemsig
tallSigbar
tallSigbuc
tallSigcab
tallSigcen
tallSigfas
tallSiggal
tallSiggar
tallSiglus
tallSigpam
tallSigtis
tallSigwut
tallSigzap
tallTisbar
tallTiscol
tallTiscom
tallTisdot
tallTisfas
tallTisgal
tallTisgar
tallTishep
tallTisket
tallTislus
tallTissem
tallTissig
tallTistar
tallTiswut
tallWutbar
tallWutcol
tallWutdot
tallWutgal
tallWutgar
tallWuthep
tallWutket
tallWutlus
tallWutpam
tallWutpat
tallWutsig
tallWuttis
tallWutzap
tallZapcol
tallZapcom
tallZapdot
tallZapgar
tallZaptis
tallZapWut
);
my %alignable = ();
$alignable{$_} = 1 for @alignables;

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
            my $child   = $children[$childIX];
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

my $semantics = <<'EOS';
:default ::= action=>main::doNode
lexeme default = latm => 1 action=>[start,length,name]
EOS

my $parser = MarpaX::YAHC::new( { semantics => $semantics, all_symbols => 1 } );
$grammar = $parser->rawGrammar();
$parser->read( $pHoonSource );
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
    if ($type eq 'node') {
        my $ruleID = $node->{ruleID};
        my ( $lhs, @rhs ) = $grammar->rule_expand($ruleID);
        return $grammar->symbol_name($lhs) . '#' . (scalar @rhs);
    }
    return "[$type]";
}

sub showAncestors {
    my ($context) = @_;
    my $ancestors = $context->{ancestors};
    my $count = scalar @{$ancestors};
    return join " ", (reverse @{$ancestors});
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
  my ($strings, $spacesNeeded) = @_;
  for (my $arrayIX = $#$strings; $arrayIX >= 0; $arrayIX--) {
                # say STDERR join " ", __FILE__, __LINE__, "tab command: needed=$spacesNeeded";
    my $string = $strings->[$arrayIX];
                # say STDERR join " ", __FILE__, __LINE__, "tab command: string=$string";
                # say STDERR +(join " ", __FILE__, __LINE__, ''), (length $string);
    for (my $stringIX = (length $string) - 1; $stringIX >= 0; $stringIX--) {
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

{

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
                push @pieces, { type=>'tab', indent=>$baseIndent, needed=>2 };
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
                    push @pieces, { type => 'text',
                      text => substr( $literal, $firstCommentPos,
                        ( $currentNL - $firstCommentPos ) )};
                }
                my $nextNL = index $literal, "\n", $currentNL + 1;
                last LEADING_LINES if $nextNL < 0;
                push @pieces, { type => 'nl'};
                $lastNL        = $currentNL;
                $currentNL     = $nextNL;
                $initialColumn = 0;
            }
            push @pieces, { type => 'nl'}, { type=>'tab', indent => $baseIndent };
            return;
        };

                my $parentSymbol = $node->{symbol};
                my $parentStart  = $node->{start};
                my $parentLength = $node->{length};
                my ($parentLine, $parentColumn) = $recce->line_column($parentStart);
                my @parentIndents = @{$argContext->{ indents }};
                $parentColumn--; # 0-based
            my @ancestors = @{$argContext->{ancestors}};
            shift @ancestors if scalar @ancestors >= 5; # no more than 5
            push @ancestors, name($node);

            my $argLine = $argContext->{ line };
            if ($argLine != $parentLine) {
                @parentIndents = ($parentColumn);
                # say "line $parentLine: new indents: ", (join " ", @parentIndents);
            } elsif ($parentColumn != $parentIndents[$#parentIndents]) {
                push @parentIndents, $parentColumn;
                # say "line $parentLine: indents: ", (join " ", @parentIndents);
            }

            my $argPreferredIndent = $argContext->{ preferredIndent };
            my $parentPreferredIndent = $argPreferredIndent if $argLine == $parentLine;
            my $parentContext = {
                ancestors => \@ancestors,
                line      => $parentLine,
                indents   => [@parentIndents],
            };
            if (defined $parentPreferredIndent) {
                $parentContext->{preferredIndent} = $parentPreferredIndent;
            }

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
                    push @pieces, { type=>'text', text=>literal( $parentStart, 2 )};
                    $gapToPieces->( $parentStart + 2, $parentLength - 2 );
                    last NODE;
                }

# say STDERR +(join " ", __FILE__, __LINE__, ''), qq{pushing piece: "}, literal( $parentStart, $parentLength ), q{"};
                push @pieces, {type=>'text', text=>literal( $parentStart, $parentLength )};
                last NODE;
            }
            if ( $type eq 'separator' ) {
                if ( $parentSymbol eq 'GAP' ) {

                    $gapToPieces->( $parentStart, $parentLength );
                    last NODE;
                }
                push @pieces, {type=>'text', text=>literal( $parentStart, $parentLength )};
                last NODE;
            }
            my $ruleID = $node->{ruleID};
            die Data::Dumper::Dumper($node) if not defined $ruleID;
            my ( $lhs, @rhs ) = $grammar->rule_expand( $node->{ruleID} );
            my $lhsName = $grammar->symbol_name($lhs);

            if ($alignable{$lhsName}) {
                $parentContext->{ preferredIndent } = $parentColumn;
            }

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
                my $start  = $node->{start};
                my @indents = ();
                my $isAnomalous = 0;
                CHILD: for my $childIX (0 .. $#$children) {
                    my $child = $children->[$childIX];
                    doCensus( $baseIndent, $depth + 1, $child, $parentContext );
                    my $childStart  = $child->{start};
                    my $symbol  = $child->{symbol};
                    next CHILD if defined $symbol and $symbolReverseDB{$symbol}->{gap};
                    my $childColumn = column( $childStart ) - 1; # 0-based
                    if ($childColumn != $parentColumn) {
                        $isAnomalous = 1;
                        # say "SEQUENCE anomaly: lhs=$lhsName";
                        # say "  context: ", showAncestors($parentContext);
                        # say "  parent at $fileName L", ( join ':', $parentLine, $parentColumn );
                        # say "  child at $fileName L", ( join ':', $recce->line_column($childStart) );
                    }
                    push @indents, $childColumn;
                }
                my $alignmentDesc = $isAnomalous ?  ( join " ", @indents ) : 'REGULAR';
                say "SEQUENCE $lhsName $alignmentDesc # $fileName L",
                  ( join ':', $recce->line_column($start) )
                  if $verbose;
                last NODE;
            }

            sub isluslusstyle {
                my ( $baseLine, $baseColumn, $indents ) = @_;
# say join " ", __FILE__, __LINE__, "L$baseLine:$baseColumn";
                  my $indentCount = scalar @{$indents};
                  my $indentIX = 0;
                  INDENT: while ($indentIX < $indentCount) {
                        my ( $thisLine, $thisColumn ) = @{$indents->[$indentIX]};
# say join " ", __FILE__, __LINE__, "L$thisLine vs. $baseLine";
                      last INDENT if $thisLine != $baseLine;
                      $indentIX++;
                  }
                  INDENT: while ($indentIX < $indentCount) {
                      my ( $thisLine, $thisColumn ) = @{$indents->[$indentIX]};
# say join " ", __FILE__, __LINE__, "L$thisColumn vs. $baseColumn";
                      return 0 if $thisColumn != $baseColumn+2;
                      $indentIX++;
                  }
                  return 1;
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
                my $start  = $node->{start};
                my @indents = ();
                CHILD: for my $childIX (0 .. $#$children) {
                    my $child = $children->[$childIX];
                    my $childStart  = $child->{start};
                    my $symbol  = $child->{symbol};
                    next CHILD if defined $symbol and $symbolReverseDB{$symbol}->{gap};
                    my ($childLine, $childColumn) = $recce->line_column( $childStart );
                    push @indents, [ $childLine, $childColumn-1 ];
                }
                my $indentDesc = '???';
              TYPE_INDENT: {

                    # is it a backdent?
                    if ( $lhsName eq 'tallKethep' or $lhsName eq 'tallKetlus' ) {

                   # align with preferred indent from ancestor, if there is one,
                   # with parent otherwise
                        my $indent = $parentPreferredIndent // $parentColumn;
                        if (
                            isbackdented(
                                $parentLine,    $indent,
                                $vertical_gaps, \@indents
                            )
                          )
                        {
                            $indentDesc = 'CAST-STYLE';
                            last TYPE_INDENT;
                        }

                    }
                    if ( $lhsName eq 'lusLusCell' ) {
                        if ( isluslusstyle( $parentLine, $parentColumn, \@indents ) ) {
                            $indentDesc = 'LUSLUS-STYLE';
                            last TYPE_INDENT;
                        }
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
                                $parentLine,    $indent,
                                $vertical_gaps, \@indents
                            )
                          )
                        {
                            my $depth = $#parentIndents - $indentIX;
                            $indentDesc =
                              $depth ? "BACKDENTED-$depth" : 'BACKDENTED';
                            last TYPE_INDENT;
                        }
                    }
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
                  #  ' ## ', showAncestors($argContext),
                  " # $fileName L", ( join ':', $recce->line_column($start) ) if $verbose;
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
            if ($spaces < 0) {
                # say STDERR join " ", __FILE__, __LINE__, "tab command: needed=$needed";
                $needed = spacesNeeded(\@output, $needed);
                # say STDERR join " ", __FILE__, __LINE__, "tab command: needed=$needed";
                $currentColumn += $needed;
                push @output, ( q{ } x $needed ) if $needed > 0;
                next PIECE;
            }
            $spaces += spacesNeeded(\@output, $needed-$spaces)
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
}

# vim: expandtab shiftwidth=4:
