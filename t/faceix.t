# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => 3;

use Test::Differences;
use IPC::Cmd qw[run_forked];

require "./yahc.pm";

my $fileList = <<'END_OF_LIST';
t/ast.d/sieve_b.hoon
t/ast.d/fizzbuzz.hoon
t/ast.d/sieve_k.hoon
t/ast.d/toe.hoon
END_OF_LIST

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

my @stdout = ();
my $gatherStdout = sub {
   push @stdout, @_;
};

my @stderr = ();
my $gatherStderr = sub {
   push @stderr, @_;
};

my $cmd = ['perl', 'faceix.pl'];

my $expectedStdoutFile = 't/util.d/faceix.out';
my $pExpectedStdout = do { open my $fh, q{<}, $expectedStdoutFile or die "Cannot open $expectedStdoutFile";
local $RS = undef; my $slurp = <$fh>; close $fh; \$slurp };

my $result = run_forked($cmd, { child_stdin => $fileList,
   stdout_handler => $gatherStdout,
   stderr_handler => $gatherStderr,
   discard_output => 1,
});

my $exitCode = $result->{'exit_code'};
Test::More::ok($exitCode eq 0, "exit code of faceix.pl is $exitCode");

my $errMsg = $result->{'err_msg'};
Test::More::diag($errMsg) if $errMsg;

my $stderr = join q{}, @stderr;
Test::More::diag($stderr) if $stderr;
Test::More::ok($stderr eq q{}, "STDERR of faceix.pl");

my $stdout = join q{}, @stdout;
eq_or_diff($stdout, ${$pExpectedStdout}, "STDOUT of faceix.pl");

# vim: expandtab shiftwidth=4:
