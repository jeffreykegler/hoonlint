# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => 103;

require "./yahc.pm";

# Size is for developer convenience -- not used in
# the code
my $fileList = <<'END_OF_LIST';
ok 6 hoons/arvo/gen/pipe/cancel.hoon
ok 6 hoons/arvo/gen/pipe/connect.hoon
ok 6 hoons/arvo/gen/pipe/list.hoon
ok 7 hoons/arvo/gen/hood/hi.hoon
ok 7 hoons/arvo/gen/hood/rm.hoon
ok 7 hoons/arvo/gen/hood/schedule.hoon
ok 8 hoons/arvo/gen/bug.hoon
ok 10 hoons/arvo/gen/hood/private.hoon
ok 10 hoons/arvo/gen/hood/public.hoon
ok 11 hoons/arvo/gen/hood/syncs.hoon
ok 11 hoons/arvo/gen/hood/tlon/add-fora.hoon
ok 11 hoons/arvo/gen/hood/tlon/add-stream.hoon
ok 12 hoons/arvo/gen/hello.hoon
ok 12 hoons/arvo/gen/hood/ping.hoon
ok 12 hoons/arvo/gen/hood/tlon/init-stream.hoon
ok 12 hoons/arvo/gen/serving.hoon
ok 13 hoons/arvo/gen/curl/url.hoon
ok 13 hoons/arvo/gen/hall/load.hoon
ok 13 hoons/arvo/gen/hall/load-legacy.hoon
ok 13 hoons/arvo/gen/hall/log.hoon
ok 13 hoons/arvo/gen/hall/save.hoon
ok 13 hoons/arvo/gen/hall/unlog.hoon
ok 13 hoons/arvo/gen/hood/ask.hoon
ok 13 hoons/arvo/gen/hood/deset.hoon
ok 13 hoons/arvo/gen/hood/mass.hoon
ok 13 hoons/arvo/gen/hood/nuke.hoon
ok 13 hoons/arvo/gen/hood/obey.hoon
ok 13 hoons/arvo/gen/hood/overload.hoon
ok 13 hoons/arvo/gen/hood/rc.hoon
ok 13 hoons/arvo/gen/hood/reboot.hoon
ok 13 hoons/arvo/gen/hood/reset.hoon
ok 13 hoons/arvo/gen/hood/rf.hoon
ok 13 hoons/arvo/gen/hood/wipe-ford.hoon
ok 13 hoons/arvo/gen/womb/balance.hoon
ok 13 hoons/arvo/gen/womb/shop.hoon
ok 13 hoons/arvo/gen/womb/stats.hoon
ok 14 hoons/arvo/gen/curl.hoon
ok 14 hoons/arvo/gen/hood/autoload.hoon
ok 14 hoons/arvo/gen/hood/breload.hoon
ok 14 hoons/arvo/gen/hood/cancel.hoon
ok 14 hoons/arvo/gen/hood/claim.hoon
ok 14 hoons/arvo/gen/hood/label.hoon
ok 14 hoons/arvo/gen/hood/manage.hoon
ok 14 hoons/arvo/gen/hood/manage-old-key.hoon
ok 14 hoons/arvo/gen/hood/rekey.hoon
ok 14 hoons/arvo/gen/hood/release.hoon
ok 14 hoons/arvo/gen/hood/reload-desk.hoon
ok 14 hoons/arvo/gen/hood/reload.hoon
ok 14 hoons/arvo/gen/hood/replay-womb-log.hoon
ok 14 hoons/arvo/gen/hood/sync.hoon
ok 14 hoons/arvo/gen/hood/track.hoon
ok 14 hoons/arvo/gen/hood/unmount.hoon
ok 14 hoons/arvo/gen/hood/unsync.hoon
ok 14 hoons/arvo/gen/ls.hoon
ok 14 hoons/arvo/gen/twit/as.hoon
ok 15 hoons/arvo/gen/ask/admins.hoon
ok 15 hoons/arvo/gen/code.hoon
ok 15 hoons/arvo/gen/gmail/send.hoon
ok 15 hoons/arvo/gen/hood/commit.hoon
ok 15 hoons/arvo/gen/hood/reinvite.hoon
ok 15 hoons/arvo/gen/hood/release-ships.hoon
ok 15 hoons/arvo/gen/hood/report.hoon
ok 15 hoons/arvo/gen/womb/balances.hoon
ok 16 hoons/arvo/gen/hood/exit.hoon
ok 16 hoons/arvo/gen/hood/link.hoon
ok 16 hoons/arvo/gen/hood/unlink.hoon
ok 16 hoons/arvo/gen/hood/verb.hoon
ok 16 hoons/arvo/gen/static/build.hoon
ok 16 hoons/arvo/gen/ticket.hoon
ok 17 hoons/arvo/gen/curl-hiss.hoon
ok 17 hoons/arvo/gen/hood/cp.hoon
ok 17 hoons/arvo/gen/hood/mount.hoon
ok 17 hoons/arvo/gen/hood/save.hoon
ok 17 hoons/arvo/gen/hood/start.hoon
ok 17 hoons/arvo/gen/hood/transfer.hoon
ok 18 hoons/arvo/gen/hood/bonus.hoon
ok 19 hoons/arvo/gen/gmail/list.hoon
ok 19 hoons/arvo/gen/hood/mv.hoon
ok 20 hoons/arvo/gen/hood/load.hoon
ok 20 hoons/arvo/gen/hood/serve.hoon
ok 20 hoons/arvo/gen/twit/feed.hoon
ok 22 hoons/arvo/gen/tree.hoon
ok 25 hoons/arvo/gen/moon.hoon
ok 27 hoons/arvo/gen/pope.hoon
ok 33 hoons/arvo/gen/cat.hoon
ok 33 hoons/arvo/gen/hood/init-oauth2/google.hoon
ok 34 hoons/arvo/gen/hood/init-auth-basic.hoon
ok 36 hoons/arvo/gen/hood/init-oauth1.hoon
ok 36 hoons/arvo/gen/hood/init-oauth2.hoon
ok 42 hoons/arvo/gen/ivory.hoon
ok 47 hoons/arvo/gen/hood/merge.hoon
ok 53 hoons/arvo/gen/hood/invite.hoon
ok 58 hoons/arvo/gen/hood/begin.hoon
ok 64 hoons/arvo/gen/help.hoon
todo 76 hoons/arvo/gen/solid.hoon
todo 80 hoons/arvo/gen/test.hoon
ok 131 hoons/arvo/gen/glass.hoon
todo 180 hoons/arvo/gen/deco.hoon
todo 285 hoons/arvo/gen/capitalize.hoon
ok 294 hoons/arvo/gen/brass.hoon
ok 324 hoons/arvo/gen/metal.hoon
todo 366 hoons/arvo/gen/musk.hoon
ok 458 hoons/arvo/gen/al.hoon
END_OF_LIST

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

my $errorCount = 0;

sub doTest {
   my ($testName, $hoonSource) = @_;
    my $ok = eval { MarpaX::YAHC::parse( \$hoonSource ); 1; };
    if ($ok) {
        Test::More::pass($testName);
	return;
    }
    Test::More::fail($testName);
    $errorCount++;
    my $evalErr = $EVAL_ERROR;
    Test::More::diag($evalErr);
    return;
}

FILE: for my $fileLine (split "\n", $fileList) {
    my $origLine = $fileLine;
    chomp $fileLine;
    $fileLine =~ s/\s*[#].*$//xmsg; # Eliminate comments
    $fileLine =~ s/^\s*//xmsg; # Eliminate leading space
    $fileLine =~ s/\s*$//xmsg; # Eliminate trailing space
    next FILE unless $fileLine;

    my ($testStatus, undef, $fileName) = split /\s+/, $fileLine;
    $testStatus //= "Misformed line: $origLine";

    open my $fh, '<', $fileName or die "Cannot open $fileName";
    my $testName = $fileName;
    $testName =~ s/^hoons\///;
    $testName = "Test of " . $testName;
    my $hoonSource = do { local $RS = undef; <$fh>; };
    if ($testStatus eq 'ok') {
      doTest($testName, $hoonSource);
      next FILE;
    }
    if ($testStatus eq 'todo') {
       TODO: {
           local $TODO = 'NYI';
	   doTest($testName, $hoonSource);
       }
      next FILE;
    }
    die("Bad test status: $testStatus\n")
}

# my $fileCount = scalar @files;
# say "$errorCount failed parses in $fileCount files";
