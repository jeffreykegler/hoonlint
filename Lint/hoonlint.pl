use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );
use Scalar::Util qw(looks_like_number weaken);
use Getopt::Long;

require "hoonlint.pm";

sub slurp {
    my ($fileName) = @_;
    local $RS = undef;
    my $fh;
    open $fh, q{<}, $fileName or die "Cannot open $fileName";
    my $file = <$fh>;
    close $fh;
    return \$file;
}

sub parseReportItems {
    my ( $config, $reportItems ) = @_;
    my $fileName       = $config->{fileName};
    my %itemHash       = ();
    my %unusedItemHash = ();

    my $itemError = sub {
        my ( $error, $line ) = @_;
        return qq{Error in item file "$fileName": $error\n}
          . qq{  Problem with line: $line\n};
    };

  ITEM: for my $itemLine ( split "\n", ${$reportItems} ) {
        my $rawItemLine = $itemLine;
        $itemLine =~ s/\s*[#].*$//;   # remove comments and preceding whitespace
        $itemLine =~ s/^\s*//;        # remove leading whitespace
        $itemLine =~ s/\s*$//;        # remove trailing whitespace
        next ITEM unless $itemLine;
        my ( $thisFileName, $lc, $policy, $subpolicy, $message ) = split /\s+/, $itemLine, 5;
        return undef, $itemError->( "Problem in report line", $rawItemLine )
          if not $thisFileName;

        return undef,
          $itemError->( qq{Malformed line:column in item line: "$lc"},
            $rawItemLine )
          unless $lc =~ /^[0-9]+[:][0-9]+$/;
        my ( $line, $column ) = split ':', $lc, 2;
        $itemError->( qq{Malformed line:column in item line: "$lc"}, $rawItemLine )
          unless Scalar::Util::looks_like_number($line)
          and Scalar::Util::looks_like_number($column);
        next ITEM unless $thisFileName eq $fileName;

        # We reassemble line:column to "normalize" it -- be indifferent to
        # leading zeros, etc.
        my $lcTag = join ':', $line, $column;
        $itemHash{$lcTag}{$policy}{$subpolicy}       = $message;
        $unusedItemHash{$lcTag}{$policy}{$subpolicy} = 1;
    }
    return \%itemHash, \%unusedItemHash;
}

my $verbose;    # right now does nothing
my $inclusionsFileName;
my @suppressionsFileNames;
my @policiesArg;
my $contextSize = 0;
my $displayDetails;

GetOptions(
    "verbose"               => \$verbose,
    "context|C=i"           => \$contextSize,
    "displayDetails|details=i"           => \$displayDetails,
    "inclusions-file|I=s"   => \$inclusionsFileName,
    "suppressions_file|S=s" => \@suppressionsFileNames,
    "policy|P=s"            => \@policiesArg,
) or die("Error in command line arguments\n");

sub usage {
    die "usage: $PROGRAM_NAME [options ...] fileName\n";
}

usage() if scalar @ARGV != 1;
my $fileName = $ARGV[0];

# Config is essentially a proto-lint-instance, containing all
# variables which are from some kind of "environment", which
# the lint instance must treat as a constant.  From the POV
# of the lint instance, the config is a global, but this is
# not necessarily the case.
#
# The archetypal example of a config is the "environment"
# created by the invocation of the `hoonlint` Perl script
# which contains information taken from the command line
# arguments and read from various files.

my %config = ();

$config{fileName} = $fileName;

$config{topicLines}       = {};
$config{mistakeLines}     = {};

my @policies = ();
push @policies, @policiesArg;
# Default policy
@policies = ('Test::Whitespace') if not scalar @policies;
die "Multiple policies not yet implemented" if scalar @policies != 1;
my %policies = ();
for my $shortPolicyName (@policies) {
  my $fullPolicyName = 'MarpaX::YAHC::Lint::Policy::' . $shortPolicyName;

  # "require policy name" is a hack until I create the full directory
  # structure required to make this a Perl module
  my $requirePolicyName = 'Policy::' . $shortPolicyName;
  my $eval_ok = eval "require $requirePolicyName";
  die $EVAL_ERROR if not $eval_ok;
  $policies{$shortPolicyName} = $fullPolicyName;
}
$config{policies} = \%policies;

my $defaultSuppressionFile = 'hoonlint.suppressions';
if ( not @suppressionsFileNames
    and -f $defaultSuppressionFile )
{
    @suppressionsFileNames = ($defaultSuppressionFile);
}

my $pSuppressions;
{
    my @suppressions = ();
    for my $fileName (@suppressionsFileNames) {
        push @suppressions, ${ slurp($fileName) };
    }
    $pSuppressions = \( join "", @suppressions );
}

my ( $suppressions, $unusedSuppressions ) = parseReportItems(\%config, $pSuppressions);
die $unusedSuppressions if not $suppressions;
$config{suppressions}       = $suppressions;
$config{unusedSuppressions} = $unusedSuppressions;

my $pInclusions;
my ( $inclusions, $unusedInclusions );
if ( defined $inclusionsFileName ) {
    $pInclusions = slurp($inclusionsFileName);
    ( $inclusions, $unusedInclusions ) = parseReportItems(\%config, $pInclusions);
    die $unusedInclusions if not $inclusions;
}
$config{inclusions}       = $inclusions;
$config{unusedInclusions} = $unusedInclusions;

my $pHoonSource = slurp($fileName);

$config{pHoonSource} = $pHoonSource;
$config{contextSize} = $contextSize;
SET_DISPLAY_DETAILS: {
    if (not defined $displayDetails) {
         $config{displayDetails} = $contextSize >= 1 ? 1 : 0;
         last SET_DISPLAY_DETAILS;
    }
    $config{displayDetails} = $displayDetails;
}

MarpaX::YAHC::Lint->new(\%config);

# vim: expandtab shiftwidth=4:
