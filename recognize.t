# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use autodie;
use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => 3;

require "./yahc.pm";

my $fileList = <<'END_OF_LIST';
6 hoons/arvo/gen/pipe/cancel.hoon
6 hoons/arvo/gen/pipe/connect.hoon
6 hoons/arvo/gen/pipe/list.hoon
7 hoons/arvo/gen/hood/hi.hoon
7 hoons/arvo/gen/hood/rm.hoon
7 hoons/arvo/gen/hood/schedule.hoon
8 hoons/arvo/gen/bug.hoon
10 hoons/arvo/gen/hood/private.hoon
10 hoons/arvo/gen/hood/public.hoon
11 hoons/arvo/gen/hood/syncs.hoon
11 hoons/arvo/gen/hood/tlon/add-fora.hoon
11 hoons/arvo/gen/hood/tlon/add-stream.hoon
12 hoons/arvo/gen/hello.hoon
12 hoons/arvo/gen/hood/ping.hoon
12 hoons/arvo/gen/hood/tlon/init-stream.hoon
12 hoons/arvo/gen/serving.hoon
13 hoons/arvo/gen/curl/url.hoon
13 hoons/arvo/gen/hall/load.hoon
13 hoons/arvo/gen/hall/load-legacy.hoon
13 hoons/arvo/gen/hall/log.hoon
13 hoons/arvo/gen/hall/save.hoon
13 hoons/arvo/gen/hall/unlog.hoon
13 hoons/arvo/gen/hood/ask.hoon
13 hoons/arvo/gen/hood/deset.hoon
13 hoons/arvo/gen/hood/mass.hoon
13 hoons/arvo/gen/hood/nuke.hoon
13 hoons/arvo/gen/hood/obey.hoon
13 hoons/arvo/gen/hood/overload.hoon
13 hoons/arvo/gen/hood/rc.hoon
13 hoons/arvo/gen/hood/reboot.hoon
13 hoons/arvo/gen/hood/reset.hoon
13 hoons/arvo/gen/hood/rf.hoon
13 hoons/arvo/gen/hood/wipe-ford.hoon
13 hoons/arvo/gen/womb/balance.hoon
13 hoons/arvo/gen/womb/shop.hoon
13 hoons/arvo/gen/womb/stats.hoon
14 hoons/arvo/gen/curl.hoon
14 hoons/arvo/gen/hood/autoload.hoon
14 hoons/arvo/gen/hood/breload.hoon
14 hoons/arvo/gen/hood/cancel.hoon
14 hoons/arvo/gen/hood/claim.hoon
14 hoons/arvo/gen/hood/label.hoon
14 hoons/arvo/gen/hood/manage.hoon
14 hoons/arvo/gen/hood/manage-old-key.hoon
14 hoons/arvo/gen/hood/rekey.hoon
14 hoons/arvo/gen/hood/release.hoon
14 hoons/arvo/gen/hood/reload-desk.hoon
14 hoons/arvo/gen/hood/reload.hoon
14 hoons/arvo/gen/hood/replay-womb-log.hoon
14 hoons/arvo/gen/hood/sync.hoon
14 hoons/arvo/gen/hood/track.hoon
14 hoons/arvo/gen/hood/unmount.hoon
14 hoons/arvo/gen/hood/unsync.hoon
14 hoons/arvo/gen/ls.hoon
14 hoons/arvo/gen/twit/as.hoon
15 hoons/arvo/gen/ask/admins.hoon
15 hoons/arvo/gen/code.hoon
15 hoons/arvo/gen/gmail/send.hoon
15 hoons/arvo/gen/hood/commit.hoon
15 hoons/arvo/gen/hood/reinvite.hoon
15 hoons/arvo/gen/hood/release-ships.hoon
15 hoons/arvo/gen/hood/report.hoon
15 hoons/arvo/gen/womb/balances.hoon
16 hoons/arvo/gen/hood/exit.hoon
16 hoons/arvo/gen/hood/link.hoon
16 hoons/arvo/gen/hood/unlink.hoon
16 hoons/arvo/gen/hood/verb.hoon
16 hoons/arvo/gen/static/build.hoon
16 hoons/arvo/gen/ticket.hoon
17 hoons/arvo/gen/curl-hiss.hoon
17 hoons/arvo/gen/hood/cp.hoon
17 hoons/arvo/gen/hood/mount.hoon
17 hoons/arvo/gen/hood/save.hoon
17 hoons/arvo/gen/hood/start.hoon
17 hoons/arvo/gen/hood/transfer.hoon
18 hoons/arvo/gen/hood/bonus.hoon
19 hoons/arvo/gen/gmail/list.hoon
19 hoons/arvo/gen/hood/mv.hoon
20 hoons/arvo/gen/hood/load.hoon
20 hoons/arvo/gen/hood/serve.hoon
20 hoons/arvo/gen/twit/feed.hoon
22 hoons/arvo/gen/tree.hoon
25 hoons/arvo/gen/moon.hoon
27 hoons/arvo/gen/pope.hoon
33 hoons/arvo/gen/cat.hoon
33 hoons/arvo/gen/hood/init-oauth2/google.hoon
34 hoons/arvo/gen/hood/init-auth-basic.hoon
36 hoons/arvo/gen/hood/init-oauth1.hoon
36 hoons/arvo/gen/hood/init-oauth2.hoon
42 hoons/arvo/gen/ivory.hoon
47 hoons/arvo/gen/hood/merge.hoon
53 hoons/arvo/gen/hood/invite.hoon
58 hoons/arvo/gen/hood/begin.hoon
64 hoons/arvo/gen/help.hoon
76 hoons/arvo/gen/solid.hoon
80 hoons/arvo/gen/test.hoon
131 hoons/arvo/gen/glass.hoon
180 hoons/arvo/gen/deco.hoon
285 hoons/arvo/gen/capitalize.hoon
294 hoons/arvo/gen/brass.hoon
324 hoons/arvo/gen/metal.hoon
366 hoons/arvo/gen/musk.hoon
458 hoons/arvo/gen/al.hoon
END_OF_LIST

$fileList =~ s/\s*[#].*$//xmsg; # Eliminate comments
$fileList =~ s/^\s*//xmsg; # Eliminate leading space
$fileList =~ s/\s*$//xmsg; # Eliminate trailing space
# Size is for developer convenience -- not used in
# the code
$fileList =~ s/^[0-9]*\s*//xmsg; # Eliminate leading size

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

my $errorCount = 0;
my @files = split "\n", $fileList;
FILE: for my $fileName (@files) {
    chomp $fileName;
    next FILE unless length $fileName;
    open my $fh, '<', $fileName;
    my $hoonSource = do { local $RS = undef; <$fh>; };
    my $ok = eval { MarpaX::YAHC::parse( \$hoonSource ); 1; };
    say "=== $fileName ===";
    next FILE if $ok;
    $errorCount++;
    my $evalErr = $EVAL_ERROR;
    say $evalErr;
}
my $fileCount = scalar @files;
say "$errorCount failed parses in $fileCount files";
