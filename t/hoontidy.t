# Test of hoontidy utility "round trip" option

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => 6;

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

my @fileList = qw(
  t/ast.d/sieve_b.hoon
  t/ast.d/sieve_k.hoon
);

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

my $cmd = [ 'perl', 'hoontidy.pl', '--style=test' ];

for my $fileName (@fileList) {

    my @stdout       = ();
    my $gatherStdout = sub {
        push @stdout, @_;
    };

    my @stderr       = ();
    my $gatherStderr = sub {
        push @stderr, @_;
    };

    my $pExpectedStdout = slurp($fileName);
    my $pChildStdin      = slurp($fileName);

    my $result = run_forked(
        $cmd,
        {
            child_stdin    => ${$pChildStdin},
            stdout_handler => $gatherStdout,
            stderr_handler => $gatherStderr,
            discard_output => 1,
        }
    );

    my $exitCode = $result->{'exit_code'};
    Test::More::ok( $exitCode eq 0, "exit code of hoontidy.pl is $exitCode" );

    my $errMsg = $result->{'err_msg'};
    Test::More::diag($errMsg) if $errMsg;

    my $stderr = join q{}, @stderr;
    Test::More::diag($stderr) if $stderr;
    Test::More::ok( $stderr eq q{}, "STDERR of hoontidy.pl" );

    my $stdout = join q{}, @stdout;
    eq_or_diff( $stdout, ${$pExpectedStdout}, "STDOUT of hoontidy.pl" );
  }

# vim: expandtab shiftwidth=4:
