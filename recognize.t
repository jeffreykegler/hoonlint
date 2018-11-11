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
ok hoons/arvo/gen/pipe/cancel.hoon
ok hoons/arvo/gen/pipe/connect.hoon
ok hoons/arvo/gen/pipe/list.hoon
ok hoons/arvo/gen/hood/hi.hoon
ok hoons/arvo/gen/hood/rm.hoon
ok hoons/arvo/gen/hood/schedule.hoon
ok hoons/arvo/gen/bug.hoon
ok hoons/arvo/gen/hood/private.hoon
ok hoons/arvo/gen/hood/public.hoon
ok hoons/arvo/gen/hood/syncs.hoon
ok hoons/arvo/gen/hood/tlon/add-fora.hoon
ok hoons/arvo/gen/hood/tlon/add-stream.hoon
ok hoons/arvo/gen/hello.hoon
ok hoons/arvo/gen/hood/ping.hoon
ok hoons/arvo/gen/hood/tlon/init-stream.hoon
ok hoons/arvo/gen/serving.hoon
ok hoons/arvo/gen/curl/url.hoon
ok hoons/arvo/gen/hall/load.hoon
ok hoons/arvo/gen/hall/load-legacy.hoon
ok hoons/arvo/gen/hall/log.hoon
ok hoons/arvo/gen/hall/save.hoon
ok hoons/arvo/gen/hall/unlog.hoon
ok hoons/arvo/gen/hood/ask.hoon
ok hoons/arvo/gen/hood/deset.hoon
ok hoons/arvo/gen/hood/mass.hoon
ok hoons/arvo/gen/hood/nuke.hoon
ok hoons/arvo/gen/hood/obey.hoon
ok hoons/arvo/gen/hood/overload.hoon
ok hoons/arvo/gen/hood/rc.hoon
ok hoons/arvo/gen/hood/reboot.hoon
ok hoons/arvo/gen/hood/reset.hoon
ok hoons/arvo/gen/hood/rf.hoon
ok hoons/arvo/gen/hood/wipe-ford.hoon
ok hoons/arvo/gen/womb/balance.hoon
ok hoons/arvo/gen/womb/shop.hoon
ok hoons/arvo/gen/womb/stats.hoon
ok hoons/arvo/gen/curl.hoon
ok hoons/arvo/gen/hood/autoload.hoon
ok hoons/arvo/gen/hood/breload.hoon
ok hoons/arvo/gen/hood/cancel.hoon
ok hoons/arvo/gen/hood/claim.hoon
ok hoons/arvo/gen/hood/label.hoon
ok hoons/arvo/gen/hood/manage.hoon
ok hoons/arvo/gen/hood/manage-old-key.hoon
ok hoons/arvo/gen/hood/rekey.hoon
ok hoons/arvo/gen/hood/release.hoon
ok hoons/arvo/gen/hood/reload-desk.hoon
ok hoons/arvo/gen/hood/reload.hoon
ok hoons/arvo/gen/hood/replay-womb-log.hoon
ok hoons/arvo/gen/hood/sync.hoon
ok hoons/arvo/gen/hood/track.hoon
ok hoons/arvo/gen/hood/unmount.hoon
ok hoons/arvo/gen/hood/unsync.hoon
ok hoons/arvo/gen/ls.hoon
ok hoons/arvo/gen/twit/as.hoon
ok hoons/arvo/gen/ask/admins.hoon
ok hoons/arvo/gen/code.hoon
ok hoons/arvo/gen/gmail/send.hoon
ok hoons/arvo/gen/hood/commit.hoon
ok hoons/arvo/gen/hood/reinvite.hoon
ok hoons/arvo/gen/hood/release-ships.hoon
ok hoons/arvo/gen/hood/report.hoon
ok hoons/arvo/gen/womb/balances.hoon
ok hoons/arvo/gen/hood/exit.hoon
ok hoons/arvo/gen/hood/link.hoon
ok hoons/arvo/gen/hood/unlink.hoon
ok hoons/arvo/gen/hood/verb.hoon
ok hoons/arvo/gen/static/build.hoon
ok hoons/arvo/gen/ticket.hoon
ok hoons/arvo/gen/curl-hiss.hoon
ok hoons/arvo/gen/hood/cp.hoon
ok hoons/arvo/gen/hood/mount.hoon
ok hoons/arvo/gen/hood/save.hoon
ok hoons/arvo/gen/hood/start.hoon
ok hoons/arvo/gen/hood/transfer.hoon
ok hoons/arvo/gen/hood/bonus.hoon
ok hoons/arvo/gen/gmail/list.hoon
ok hoons/arvo/gen/hood/mv.hoon
ok hoons/arvo/gen/hood/load.hoon
ok hoons/arvo/gen/hood/serve.hoon
ok hoons/arvo/gen/twit/feed.hoon
ok hoons/arvo/gen/tree.hoon
ok hoons/arvo/gen/moon.hoon
ok hoons/arvo/gen/pope.hoon
ok hoons/arvo/gen/cat.hoon
ok hoons/arvo/gen/hood/init-oauth2/google.hoon
ok hoons/arvo/gen/hood/init-auth-basic.hoon
ok hoons/arvo/gen/hood/init-oauth1.hoon
ok hoons/arvo/gen/hood/init-oauth2.hoon
ok hoons/arvo/gen/ivory.hoon
ok hoons/arvo/gen/hood/merge.hoon
ok hoons/arvo/gen/hood/invite.hoon
ok hoons/arvo/gen/hood/begin.hoon
ok hoons/arvo/gen/help.hoon
ok hoons/arvo/gen/solid.hoon
todo hoons/arvo/gen/test.hoon
ok hoons/arvo/gen/glass.hoon
ok hoons/arvo/gen/deco.hoon
todo hoons/arvo/gen/capitalize.hoon
ok hoons/arvo/gen/brass.hoon
ok hoons/arvo/gen/metal.hoon
ok hoons/arvo/gen/musk.hoon
ok hoons/arvo/gen/al.hoon
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

    my ($testStatus, $fileName) = split /\s+/, $fileLine;
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
