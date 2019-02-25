# Hoon "tidy" utility

use 5.010;
use strict;
use warnings;
no warnings 'recursion';

package MarpaX::YAHC::Lint;

use Data::Dumper;
use English qw( -no_match_vars );
use Scalar::Util qw(looks_like_number weaken);
use Getopt::Long;

require "yahc.pm";

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

sub internalError {
    my ($instance) = @_;
    my $fileName = $instance->{fileName} // "[No file name]";
    my @pieces = ( "$PROGRAM_NAME $fileName: Internal Error\n", @_ );
    push @pieces, "\n" unless $pieces[$#pieces] =~ m/\n$/;
    my ( undef, $codeFilename, $codeLine ) = caller;
    die join q{}, @pieces,
      "Internal error was at $codeFilename, line $codeLine";
}

sub doNode {
    my ( $instance, @argChildren ) = @_;
    my $pSource = $instance->{pHoonSource};
    my @results    = ();
    my $childCount = scalar @argChildren;
    no warnings 'once';
    my $ruleID = $Marpa::R2::Context::rule;
    use warnings;
    my ( $lhs, @rhs ) =
      map { $MarpaX::YAHC::Lint::grammar->symbol_display_form($_) }
      $MarpaX::YAHC::Lint::grammar->rule_expand($ruleID);
    my ( $first_g1, $last_g1 ) = Marpa::R2::Context::location();
    my ($lhsStart) =
      $MarpaX::YAHC::Lint::recce->g1_location_to_span( $first_g1 + 1 );

    my $node;
  CREATE_NODE: {
        if ( $childCount <= 0 ) {
            $node = {
                type   => 'null',
                symbol => $lhs,
                start  => $lhsStart,
                length => 0,
            };
            last CREATE_NODE;
        }
        my ( $last_g1_start, $last_g1_length ) =
          $MarpaX::YAHC::Lint::recce->g1_location_to_span($last_g1);
        my $lhsLength = $last_g1_start + $last_g1_length - $lhsStart;
      RESULT: {
          CHILD: for my $childIX ( 0 .. $#argChildren ) {
                my $child   = $argChildren[$childIX];
                my $refType = ref $child;
                next CHILD unless $refType eq 'ARRAY';

                my ( $lexemeStart, $lexemeLength, $lexemeName ) = @{$child};

                if ( $lexemeName eq 'TRIPLE_DOUBLE_QUOTE_STRING' ) {
                    my $terminator    = q{"""};
                    my $terminatorPos = index ${ $pSource },
                      $terminator,
                      $lexemeStart + $lexemeLength;
                    $lexemeLength =
                      $terminatorPos + ( length $terminator ) - $lexemeStart;
                }
                if ( $lexemeName eq 'TRIPLE_QUOTE_STRING' ) {
                    my $terminator    = q{'''};
                    my $terminatorPos = index ${ $pSource },
                      $terminator,
                      $lexemeStart + $lexemeLength;
                    $lexemeLength =
                      $terminatorPos + ( length $terminator ) - $lexemeStart;
                }
                $argChildren[$childIX] = {
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

                    my $child     = $argChildren[$childIX];
                    my $childType = $child->{type};
                    $childIX++;
                  ITEM: {
                        if ( defined $lastSeparator ) {
                            my $length =
                              $child->{start} - $lastSeparator->{start};
                            $lastSeparator->{length} = $length;
                        }
                        push @results, $child;
                        $lastLocation = $child->{start} + $child->{length};
                    }
                    last RESULT if $childIX > $#argChildren;
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
          CHILD: for my $childIX ( 0 .. $#argChildren ) {
                my $child = $argChildren[$childIX];
                push @results, $child;
            }
        }

        $node = {
            type     => 'node',
            ruleID   => $ruleID,
            start    => $lhsStart,
            length   => $lhsLength,
            children => \@results,
        };
    }

    # Add weak links
    my $children = $node->{children};
    if ( $children and scalar @{$children} >= 1 ) {
      CHILD: for my $childIX ( 0 .. $#$children ) {
            my $child = $children->[$childIX];
            $child->{PARENT} = $node;
            weaken( $child->{PARENT} );
        }
      CHILD: for my $childIX ( 1 .. $#$children ) {
            my $thisChild = $children->[$childIX];
            my $prevChild = $children->[ $childIX - 1 ];
            $thisChild->{PREV} = $prevChild;
            weaken( $thisChild->{PREV} );
            $prevChild->{NEXT} = $thisChild;
            weaken( $prevChild->{NEXT} );
        }
    }

    return $node;
}

sub literalNode {
    my ( $instance, $node ) = @_;
    my $start = $node->{start};
    my $length = $node->{length};
    return $instance->literal($start, $length);
}

sub literalLine {
    my ( $instance, $lineNum ) = @_;
    my $lineToPos     = $instance->{lineToPos};
    my $startPos = $lineToPos->[$lineNum];
    my $line =
      $instance->literal( $startPos,
        ( $lineToPos->[ $lineNum + 1 ] - $startPos ) );
    return $line;
}

sub literal {
    my ( $instance, $start, $length ) = @_;
    my $pSource = $instance->{pHoonSource};
    return substr ${$pSource}, $start, $length;
}

sub column {
    my ( $instance, $pos ) = @_;
    my $pSource = $instance->{pHoonSource};
    return $pos - ( rindex ${$pSource}, "\n", $pos - 1 );
}

sub contextDisplay {
    my ($instance)    = @_;
    my $pTopicLines   = $instance->{topicLines};
    my $pMistakeLines = $instance->{mistakeLines};
    my $contextSize   = $instance->{contextSize};
    my $lineToPos     = $instance->{lineToPos};
    my @pieces        = ();
    my %tag = map { $_ => q{>} } @{$pTopicLines};
    $tag{$_} = q{!} for keys %{$pMistakeLines};
    my @sortedLines = sort { $a <=> $b } map { $_ + 0; } keys %tag;

# say STDERR join " ", __FILE__, __LINE__, "# of sorted lines:", (scalar @sortedLines);
    if ( $contextSize <= 0 ) {
        for my $lineNum (@sortedLines) {
            my $mistakeDescs = $pMistakeLines->{$lineNum};
            for my $mistakeDesc ( @{$mistakeDescs} ) {
                push @pieces, $mistakeDesc, "\n";
            }
        }
        return join q{}, @pieces;
    }

    my $maxNumWidth   = length q{} . $#{ $instance->{lineToPos} };
    my $lineNumFormat = q{%} . $maxNumWidth . 'd';

    # Add to @pieces a set of lines to be displayed consecutively
    my $doConsec = sub () {
        my ( $start, $end ) = @_;
        $start = 1            if $start < 1;
        $end   = $#$lineToPos-1 if $end >= $#$lineToPos;
        for my $lineNum ( $start .. $end ) {
            my $startPos = $lineToPos->[$lineNum];
            my $line = $instance->literalLine($lineNum);
            my $tag          = $tag{$lineNum} // q{ };
            my $mistakeDescs = $pMistakeLines->{$lineNum};
            for my $mistakeDesc ( @{$mistakeDescs} ) {
                push @pieces, '[ ', $mistakeDesc, " ]\n";
            }
            push @pieces, ( sprintf $lineNumFormat, $lineNum ), $tag, q{ },
              $line;
        }
    };

    my $lastIX = -1;
  CONSEC_RANGE: while ( $lastIX < $#sortedLines ) {
        my $firstIX = $lastIX + 1;

        # Divider line if after first consecutive range
        push @pieces, ( '-' x ( $maxNumWidth + 2 ) ), "\n" if $firstIX > 0;
        $lastIX = $firstIX;
      SET_LAST_IX: while (1) {
            my $nextIX = $lastIX + 1;
            last SET_LAST_IX if $nextIX > $#sortedLines;

    # We combine lines if by doing so, we make the listing shorter.
    # This is calculated by
    # 1.) Taking the current last line.
    # 2.) Add the context lines for the last and next lines (2*($contextSize-1))
    # 3.) Adding 1 for the divider line, which we save if we combine ranges.
    # 4.) Adding 1 because we test if they abut, not overlap
    # Doing the arithmetic, we get
            last SET_LAST_IX
              if $sortedLines[$lastIX] + 2 * $contextSize <
              $sortedLines[$nextIX];
            $lastIX = $nextIX;
        }
        $doConsec->(
            $sortedLines[$firstIX] - ( $contextSize - 1 ),
            $sortedLines[$lastIX] + ( $contextSize - 1 )
        );
    }

    return join q{}, @pieces;
}

sub reportItem {
    my ( $instance, $mistake, $mistakeDesc, $topicLineArg, $mistakeLineArg ) =
      @_;

    my $inclusions   = $instance->{inclusions};
    my $suppressions = $instance->{suppressions};
    my $reportPolicy = $mistake->{policy};
    my $reportLine   = $mistake->{reportLine} // $mistake->{line};
    my $reportColumn = $mistake->{reportColumn} // $mistake->{column};
    my $reportLC     = join ':', $reportLine, $reportColumn + 1;
    my $suppressThisItem = 0;
    my $excludeThisItem = 0;

    $excludeThisItem = 1 if $inclusions and not $inclusions->{$reportPolicy}{$reportLC};
    my $suppression = $suppressions->{$reportPolicy}{$reportLC};
    if ( defined $suppression ) {
        $suppressThisItem = 1;
        $instance->{unusedSuppressions}->{$reportPolicy}{$reportLC} = undef;
    }

    return if $excludeThisItem;
    if ( $suppressThisItem ) {
        return unless $instance->{censusWhitespace};
        $mistakeDesc = "SUPPRESSION $suppression";
    }

    my $fileName     = $instance->{fileName};
    my $topicLines   = $instance->{topicLines};
    my $mistakeLines = $instance->{mistakeLines};
    push @{$topicLines}, ref $topicLineArg ? @{$topicLineArg} : $topicLineArg;
    my $thisMistakeDescs = $mistakeLines->{$mistakeLineArg};
    $thisMistakeDescs = [] if not defined $thisMistakeDescs;
    push @{$thisMistakeDescs}, "$fileName $reportLC $reportPolicy $mistakeDesc";
    $mistakeLines->{$mistakeLineArg} = $thisMistakeDescs;

}

# The "symbol" of a node.  Not necessarily unique.
sub symbol {
    my ( $instance, $node ) = @_;
    my $grammar = $instance->{grammar};
    my $name    = $node->{symbol};
    return $name if defined $name;
    my $type = $node->{type};
    die Data::Dumper::Dumper($node) if not $type;
    if ( $type eq 'node' ) {
        my $ruleID = $node->{ruleID};
        my ( $lhs, @rhs ) = $grammar->rule_expand($ruleID);
        return $grammar->symbol_name($lhs);
    }
    return "[$type]";
}

# Can be used as test of "brick-ness"
sub brickName {
    my ( $instance, $node ) = @_;
    my $grammar = $instance->{grammar};
    my $type    = $node->{type};
    return symbol($node) if $type ne 'node';
    my $ruleID = $node->{ruleID};
    my ( $lhs, @rhs ) = $grammar->rule_expand($ruleID);
    my $lhsName = $grammar->symbol_name($lhs);
    return $lhsName if not $instance->{mortarLHS}->{$lhsName};
    return;
}

# The name of a name for diagnostics purposes.  Prefers
# "brick" symbols over "mortar" symbols.
sub diagName {
    my ( $instance, $node, $hoonName ) = @_;
    my $grammar = $instance->{grammar};
    my $type    = $node->{type};
    return symbol($node) if $type ne 'node';
    my $ruleID = $node->{ruleID};
    my ( $lhs, @rhs ) = $grammar->rule_expand($ruleID);
    my $lhsName = $grammar->symbol_name($lhs);
    return $lhsName if not $instance->{mortarLHS}->{$lhsName};
    $instance->internalError("No hoon name for $lhsName") if not $hoonName;
    return $hoonName;
}

# The "name" of a node.  Not necessarily unique
sub name {
    my ( $instance, $node ) = @_;
    my $grammar = $instance->{grammar};
    my $type    = $node->{type};
    my $symbol  = $instance->symbol($node);
    return $symbol if $type ne 'node';
    my $ruleID = $node->{ruleID};
    my ( $lhs, @rhs ) = $grammar->rule_expand($ruleID);
    return $symbol . '#' . ( scalar @rhs );
}

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

        my $string = $strings->[$arrayIX];

        for (
            my $stringIX = ( length $string ) - 1 ;
            $stringIX >= 0 ;
            $stringIX--
          )
        {
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

sub testStyleCensus {
    my ($instance)      = @_;
    my $ruleDB          = $instance->{ruleDB};
    my $symbolDB        = $instance->{symbolDB};
    my $symbolReverseDB = $instance->{symbolReverseDB};
    my $grammar         = $instance->{grammar};

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
        $symbolDB->[$symbolID] = $data;
        $symbolReverseDB->{$name} = $data;
    }
    my $gapID = $symbolReverseDB->{'GAP'}->{id};
  RULE:
    for my $ruleID ( $grammar->rule_ids() ) {
        my $data = { id => $ruleID };
        my ( $lhs, @rhs ) = $grammar->rule_expand($ruleID);
        $data->{symbols} = [ $lhs, @rhs ];
        my $lhsName       = $grammar->symbol_name($lhs);
        my $separatorName = $separator{$lhsName};
        if ($separatorName) {
            my $separatorID = $symbolReverseDB->{$separatorName}->{id};
            $data->{separator} = $separatorID;
            if ( $separatorID == $gapID ) {
                $data->{gapiness} = -1;
            }
        }
        if ( not defined $data->{gapiness} ) {
            for my $rhsID (@rhs) {
                $data->{gapiness}++ if $symbolDB->[$rhsID]->{gap};
            }
        }
        $ruleDB->[$ruleID] = $data;

# say STDERR join " ", __FILE__, __LINE__, "setting rule $ruleID gapiness to", $data->{gapiness} // 'undef';
        $symbolReverseDB->{$lhs}->{lexeme} = 0;
    }

}

sub line_column {
    my ( $instance, $pos ) = @_;
    $Data::Dumper::Maxdepth = 3;
    die Data::Dumper::Dumper($instance) if not defined $instance->{recce};
    my ( $line, $column ) = $instance->{recce}->line_column($pos);
    $column--;
    return $line, $column;
}

sub ancestor {
    my ( $instance, $node, $generations ) = @_;
    my $thisNode = $node;
    PARENT: while ($thisNode) {
        return $thisNode if $generations <= 0;
        $generations--;
        $thisNode = $thisNode->{PARENT};
    }
    return;
}

sub nodeLC {
    my ( $instance, $node ) = @_;
    return $instance->line_column( $node->{start} )
}

sub brickLC {
    my ( $instance, $node ) = @_;
    my $thisNode = $node;
    while ($thisNode) {
        return $instance->nodeLC($thisNode)
          if $instance->brickName($thisNode);
        $thisNode = $thisNode->{PARENT};
    }
    $instance->internalError("No brick parent");
}

sub new {
    my ($class, $config) = (@_);
    my $fileName = $config->{fileName};
    my %lint         = %{$config};
    my $lintInstance = \%lint;
    bless $lintInstance, "MarpaX::YAHC::Lint";
    my $policies = $lintInstance->{policies};
    my $pSource = $lintInstance->{pHoonSource};

    my @data = ();

    my $semantics = <<'EOS';
:default ::= action=>MarpaX::YAHC::Lint::doNode
lexeme default = latm => 1 action=>[start,length,name]
EOS

    my $parser =
      MarpaX::YAHC::new( { semantics => $semantics, all_symbols => 1 } );
    my $dsl = $parser->dsl();

    $MarpaX::YAHC::Lint::grammar = $parser->rawGrammar();
    $lintInstance->{grammar} = $MarpaX::YAHC::Lint::grammar;

    my %tallRuneRule = map { +( $_, 1 ) } grep {
             /^tall[B-Z][aeoiu][b-z][b-z][aeiou][b-z]$/
          or /^tall[B-Z][aeoiu][b-z][b-z][aeiou][b-z]Mold$/
    } map { $MarpaX::YAHC::Lint::grammar->symbol_name($_); }
      $MarpaX::YAHC::Lint::grammar->symbol_ids();
    $lintInstance->{tallRuneRule} = \%tallRuneRule;

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
    $lintInstance->{tallNoteRule} = \%tallNoteRule;

    my %mortarLHS = map { +( $_, 1 ) }
      qw(rick5dJog ruck5dJog rick5d ruck5d till5dSeq tall5dSeq);
    $lintInstance->{mortarLHS} = \%mortarLHS;

    my %tallBodyRule =
      map { +( $_, 1 ) } grep { not $tallNoteRule{$_} } keys %tallRuneRule;
    $lintInstance->{tallBodyRule} = \%tallBodyRule;

    # Will include:
    # BuccenMold BuccolMold BucwutMold
    # Buccen Buccol Bucwut Colsig Coltar Wutbar Wutpam
    my %tall_0RunningRule = map { +( $_, 1 ) } qw(tallWutbar tallWutpam);
    $lintInstance->{tall_0RunningRule} = \%tall_0RunningRule;

    # Will include:
    # CencolMold
    # Dotket Semcol Semsig Tissig
    my %tall_1RunningRule = map { +( $_, 1 ) } qw( tallSemcol tallSemsig );
    $lintInstance->{tall_1RunningRule} = \%tall_1RunningRule;

    my %tall_1JoggingRule =
      map { +( $_, 1 ) } qw(tallCentis tallCencab tallWuthep);
    $lintInstance->{tall_1JoggingRule} = \%tall_1JoggingRule;

    my %tall_2JoggingRule = map { +( $_, 1 ) } qw(tallCentar tallWutlus);
    $lintInstance->{tall_2JoggingRule} = \%tall_2JoggingRule;

    my %tallJogging1_Rule = map { +( $_, 1 ) } qw(tallTiscol);
    $lintInstance->{tallJogging1_Rule} = \%tallJogging1_Rule;

    my %tallLuslusRule = map { +( $_, 1 ) } qw(LuslusCell LushepCell LustisCell
      optFordFashep optFordFaslus fordFaswut fordFastis);
    $lintInstance->{tallLuslusRule} = \%tallLuslusRule;

    my %tallJogRule = map { +( $_, 1 ) } qw(rick5dJog ruck5dJog);
    $lintInstance->{tallJogRule} = \%tallJogRule;

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
      tallTisdot
      tallTisfas
      tallTisgar
      tallTishep
      tallTisket
      tallTislus
      tallTissem
      tallTistar
      tallTiswut
      tallWutcol
      tallWutdot
      tallWutket
      tallWutpat
      tallWutsig
      tallZapcol
      tallZapdot
      tallZapwut
    );

    # say Data::Dumper::Dumper(\%tallBodyRule);

    $parser->read($pSource);

    $MarpaX::YAHC::Lint::recce = $parser->rawRecce();
    $lintInstance->{recce} = $MarpaX::YAHC::Lint::recce;

    $parser = undef;    # free up memory
    my $astRef = $MarpaX::YAHC::Lint::recce->value($lintInstance);

    my @lineToPos = ( -1, 0 );
    while ( ${$pSource} =~ m/\n/g ) { push @lineToPos, pos ${$pSource} }
    $lintInstance->{lineToPos} = \@lineToPos;

    die "Parse failed" if not $astRef;

    # local $Data::Dumper::Deepcopy = 1;
    # local $Data::Dumper::Terse    = 1;
    # local $Data::Dumper::Maxdepth    = 3;

    my $astValue = ${$astRef};

    $lintInstance->{ruleDB}          = [];
    $lintInstance->{symbolDB}        = [];
    $lintInstance->{symbolReverseDB} = {};

    $lintInstance->testStyleCensus();

    for my $policyShortName ( keys %{$policies} ) {
        my $policyFullName = $policies->{$policyShortName};
        my $constructor    = UNIVERSAL::can( $policyFullName, 'new' );
        my $policy         = $constructor->( $policyFullName, $lintInstance );
        $policy->{shortName} = $policyShortName;
        $policy->{fullName} = $policyFullName;
        $policy->validate(
            $astValue,
            {
                hoonName  => '[TOP]',
                line      => -1,
            }
        );
    }

    print $lintInstance->contextDisplay();

    my $unusedSuppressions = $lintInstance->{unusedSuppressions};
    for my $type ( keys %{$unusedSuppressions} ) {
        for my $tag (
            grep { $unusedSuppressions->{$type}{$_} }
            keys %{ $unusedSuppressions->{$type} }
          )
        {
            say "Unused suppression: $fileName $type $tag";
        }
    }

    return $lintInstance;
}

1;

# vim: expandtab shiftwidth=4:
