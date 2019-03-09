# Test of hoonline utility

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => 24;

use Test::Differences;
use IPC::Cmd qw[run_forked];

require "./yahc.pm";

sub slurp {
    my ($fileName) = @_;
    local $RS = undef;
    my $fh;
    open $fh, q{<}, $fileName or die "Cannot open $fileName";
    my $file = <$fh>;
    close $fh;
    return \$file;
}

my @tests = (
    ['t/ast.d/fizzbuzz.hoon', 't/util.d/fizzbuzz.census', '--census', '--sup=t/util.d/examples.hoonlint.suppressions'],
    ['t/ast.d/sieve_b.hoon', 't/util.d/sieve_b.census', '--census', '--sup=t/util.d/examples.hoonlint.suppressions'],
    ['t/ast.d/sieve_k.hoon', 't/util.d/sieve_k.census', '--census', '--sup=t/util.d/examples.hoonlint.suppressions'],
    ['t/ast.d/toe.hoon', 't/util.d/toe.census', '--census', '--sup=t/util.d/examples.hoonlint.suppressions'],
    ['t/ast.d/fizzbuzz.hoon', 't/util.d/fizzbuzz.lint.out', '--sup=t/util.d/examples.hoonlint.suppressions'],
    ['t/ast.d/sieve_b.hoon', 't/util.d/sieve_b.lint.out', '--sup=t/util.d/examples.hoonlint.suppressions'],
    ['t/ast.d/sieve_k.hoon', 't/util.d/sieve_k.lint.out', '--sup=t/util.d/examples.hoonlint.suppressions'],
    ['t/ast.d/toe.hoon', 't/util.d/toe.lint.out', '--sup=t/util.d/examples.hoonlint.suppressions'],
);

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;


for my $testData (@tests) {

    my ($stdinName, $stdoutName, @options) = @{$testData};

    my $cmd = [ 'perl', '-I', 'Lint', 'Lint/hoonlint.pl', @options, $stdinName ];

    my @stdout       = ();
    my $gatherStdout = sub {
        push @stdout, @_;
    };

    my @stderr       = ();
    my $gatherStderr = sub {
        push @stderr, @_;
    };

    my $pExpectedStdout = $stdoutName ? slurp($stdoutName) : \q{};

    my $result = run_forked(
        $cmd,
        {
            child_stdin    => '',
            stdout_handler => $gatherStdout,
            stderr_handler => $gatherStderr,
            discard_output => 1,
        }
    );

    my $exitCode = $result->{'exit_code'};
    Test::More::ok( $exitCode eq 0, "exit code for $stdinName.pl is $exitCode" );

    my $errMsg = $result->{'err_msg'};
    Test::More::diag($errMsg) if $errMsg;

    my $stderr = join q{}, @stderr;
    Test::More::diag($stderr) if $stderr;
    Test::More::ok( $stderr eq q{}, "STDERR for $stdinName" );

    my $stdout = join q{}, @stdout;
    eq_or_diff( $stdout, ${$pExpectedStdout}, "STDOUT for $stdinName" );
  }

# vim: expandtab shiftwidth=4:
