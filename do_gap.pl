use 5.010;
use strict;
use warnings;
use English qw( -no_match_vars );
use Getopt::Long;

use Marpa::R2 8.000000;

sub usage {

    die <<"END_OF_USAGE_MESSAGE";
    $PROGRAM_NAME
    $PROGRAM_NAME --stdin < file
With --stdin arg, reads expression from standard input.
By default, runs a test.
END_OF_USAGE_MESSAGE
} ## end sub usage

my $stdin_flag = 1;
my $interOffset;
my $preOffset;
my $getopt_result = Getopt::Long::GetOptions(
    'stdin!' => \$stdin_flag,
    'inter:i' => \$interOffset,
    'pre:i' => \$preOffset,
);

usage() if not $getopt_result;
die "interOffset required" if not defined $interOffset;
# convert to 1-based
$interOffset -= 1;
$preOffset -= 1 if defined $preOffset;

my $input;
if ($stdin_flag) {
    $input = do { local $INPUT_RECORD_SEPARATOR = undef; <> };
}

my @lineToPos = ( -1, 0 );
{
    my $lastPos = 0;
  LINE: while (1) {
        my $newPos = index $input, "\n", $lastPos;

        # say $newPos;
        last LINE if $newPos < 0;
        $lastPos = $newPos + 1;
        push @lineToPos, $lastPos;
    }
}
# say STDERR join " ", __FILE__, __LINE__, Data::Dumper::Dumper(\@lineToPos);

sub literalLine {
    my ( $lineNum ) = @_;
    my $startPos = $lineToPos[$lineNum];
    my $line =
      substr $input, $startPos,
        ( $lineToPos[ $lineNum + 1 ] - $startPos ) ;
    return $line;
}

my $rules = <<'END_OF_GRAMMAR';
:start ::= gapComments
gapComments ::= InterPart PrePart
InterPart ::=
InterPart ::= ProperInterComponent OptInterComponents
OptInterComponents ::= InterComponent*
InterComponent ::= ProperInterComponent
InterComponent ::= OtherStuff
ProperInterComponent ::= InterComment
ProperInterComponent ::= Staircase

Staircase ::= UpperRisers Tread LowerRisers
UpperRisers ::= UpperRiser+
LowerRisers ::= LowerRiser+

PrePart ::=
PrePart ::= ProperPreComponent OptPreComponents
ProperPreComponent ::= PreComment
OptPreComponents ::= PreComponent*
PreComponent ::= ProperPreComponent
PreComponent ::= OtherStuff

OtherStuff ::= MetaComment
OtherStuff ::= BadComment
OtherStuff ::= BlankLine

unicorn ~ [^\d\D]
BadComment ~ unicorn
BlankLine ~ unicorn
InterComment ~ unicorn
LowerRiser ~ unicorn
MetaComment ~ unicorn
PreComment ~ unicorn
Tread ~ unicorn
UpperRiser ~ unicorn

END_OF_GRAMMAR

# TO HERE !!!

my $grammar = Marpa::R2::Scanless::G->new(
    {   action_object  => 'My_Actions',
        default_action => 'do_arg0',
        source         => \$rules,
    }
);

my $recce = Marpa::R2::Scanless::R->new( { grammar => $grammar } );

my $self = bless { grammar => $grammar }, 'My_Actions';
$self->{recce}        = $recce;
$self->{symbol_table} = {};
local $My_Actions::SELF = $self;

if ( not defined eval { $recce->read( \$input, 0, 0 ); 1 } ) {

    # Add last expression found, and rethrow
    my $eval_error = $EVAL_ERROR;
    chomp $eval_error;
    die $eval_error, "\n";
} ## end if ( not defined eval { $recce->read($p_string); 1 })

my $mistakes = checkGapComments($input, $interOffset, $preOffset);
say Data::Dumper::Dumper($mistakes);

sub checkGapComments {
    my ( $input, $interOffset, $preOffset ) = @_;
    my @mistakes = ();

    my $lineNum = 0;
  LINE: while (1) {
        $lineNum++;
        last LINE if $lineNum >= $#lineToPos;
        my $line = literalLine($lineNum);
        say STDERR join ' ', __FILE__, __LINE__, $lineNum, qq{"$line"};

      FIND_ALTERNATIVES: {
            my $expected = $recce->terminals_expected();
            say Data::Dumper::Dumper($expected);
            my $tier1_ok;
            my @tier2 = ();
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
                    }
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
                    }
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
                    }
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
                    }
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
                    }
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
                }
                push @tier3, $terminal;
            }

          TIER3: for my $terminal (@tier3) {
                if ( $terminal eq 'BlankLine' ) {
                    say STDERR join ' ', __FILE__, __LINE__, qq{"$line"};
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

                        push @mistakes,
                          [ 'vgap-bad-comment', $lineNum, $commentOffset ];
                  # anything in this tier terminates the finding of alternatives
                        last FIND_ALTERNATIVES;
                    }
                }
            }

        }
        my $startPos = $lineToPos[$lineNum];
        say STDERR join ' ', __FILE__, __LINE__;
        $recce->lexeme_complete( $startPos,
            ( $lineToPos[ $lineNum + 1 ] - $startPos ) );
    }
    return \@mistakes;
}

# my $value_ref = $recce->value();
# if ( not defined $value_ref ) {
# die $self->show_last_expression(), "\n",
# "No parse was found, after reading the entire input\n";
# }
# return ${$value_ref}, $self->{symbol_table};

package My_Actions;
our $SELF;
sub new { return $SELF }

sub do_negate {
    return -$_[2];
}

sub do_arg0 { return $_[1]; }
sub do_arg1 { return $_[2]; }
sub do_arg2 { return $_[3]; }

# vim: expandtab shiftwidth=4:
