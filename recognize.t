# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => 311;

require "./yahc.pm";

# Size is for developer convenience -- not used in
# the code
my $fileList = <<'END_OF_LIST';
ok hoons/arvo/gen/al.hoon
ok hoons/arvo/gen/ask/admins.hoon
ok hoons/arvo/gen/brass.hoon
ok hoons/arvo/gen/bug.hoon
ok hoons/arvo/gen/cat.hoon
ok hoons/arvo/gen/code.hoon
ok hoons/arvo/gen/curl-hiss.hoon
ok hoons/arvo/gen/curl.hoon
ok hoons/arvo/gen/curl/url.hoon
ok hoons/arvo/gen/deco.hoon
ok hoons/arvo/gen/glass.hoon
ok hoons/arvo/gen/gmail/list.hoon
ok hoons/arvo/gen/gmail/send.hoon
ok hoons/arvo/gen/hall/load.hoon
ok hoons/arvo/gen/hall/load-legacy.hoon
ok hoons/arvo/gen/hall/log.hoon
ok hoons/arvo/gen/hall/save.hoon
ok hoons/arvo/gen/hall/unlog.hoon
ok hoons/arvo/gen/hello.hoon
ok hoons/arvo/gen/help.hoon
ok hoons/arvo/gen/hood/ask.hoon
ok hoons/arvo/gen/hood/autoload.hoon
ok hoons/arvo/gen/hood/begin.hoon
ok hoons/arvo/gen/hood/bonus.hoon
ok hoons/arvo/gen/hood/breload.hoon
ok hoons/arvo/gen/hood/cancel.hoon
ok hoons/arvo/gen/hood/claim.hoon
ok hoons/arvo/gen/hood/commit.hoon
ok hoons/arvo/gen/hood/cp.hoon
ok hoons/arvo/gen/hood/deset.hoon
ok hoons/arvo/gen/hood/exit.hoon
ok hoons/arvo/gen/hood/hi.hoon
ok hoons/arvo/gen/hood/init-auth-basic.hoon
ok hoons/arvo/gen/hood/init-oauth1.hoon
ok hoons/arvo/gen/hood/init-oauth2/google.hoon
ok hoons/arvo/gen/hood/init-oauth2.hoon
ok hoons/arvo/gen/hood/invite.hoon
ok hoons/arvo/gen/hood/label.hoon
ok hoons/arvo/gen/hood/link.hoon
ok hoons/arvo/gen/hood/load.hoon
ok hoons/arvo/gen/hood/manage.hoon
ok hoons/arvo/gen/hood/manage-old-key.hoon
ok hoons/arvo/gen/hood/mass.hoon
ok hoons/arvo/gen/hood/merge.hoon
ok hoons/arvo/gen/hood/mount.hoon
ok hoons/arvo/gen/hood/mv.hoon
ok hoons/arvo/gen/hood/nuke.hoon
ok hoons/arvo/gen/hood/obey.hoon
ok hoons/arvo/gen/hood/overload.hoon
ok hoons/arvo/gen/hood/ping.hoon
ok hoons/arvo/gen/hood/private.hoon
ok hoons/arvo/gen/hood/public.hoon
ok hoons/arvo/gen/hood/rc.hoon
ok hoons/arvo/gen/hood/reboot.hoon
ok hoons/arvo/gen/hood/reinvite.hoon
ok hoons/arvo/gen/hood/rekey.hoon
ok hoons/arvo/gen/hood/release.hoon
ok hoons/arvo/gen/hood/release-ships.hoon
ok hoons/arvo/gen/hood/reload-desk.hoon
ok hoons/arvo/gen/hood/reload.hoon
ok hoons/arvo/gen/hood/replay-womb-log.hoon
ok hoons/arvo/gen/hood/report.hoon
ok hoons/arvo/gen/hood/reset.hoon
ok hoons/arvo/gen/hood/rf.hoon
ok hoons/arvo/gen/hood/rm.hoon
ok hoons/arvo/gen/hood/save.hoon
ok hoons/arvo/gen/hood/schedule.hoon
ok hoons/arvo/gen/hood/serve.hoon
ok hoons/arvo/gen/hood/start.hoon
ok hoons/arvo/gen/hood/sync.hoon
ok hoons/arvo/gen/hood/syncs.hoon
ok hoons/arvo/gen/hood/tlon/add-fora.hoon
ok hoons/arvo/gen/hood/tlon/add-stream.hoon
ok hoons/arvo/gen/hood/tlon/init-stream.hoon
ok hoons/arvo/gen/hood/track.hoon
ok hoons/arvo/gen/hood/transfer.hoon
ok hoons/arvo/gen/hood/unlink.hoon
ok hoons/arvo/gen/hood/unmount.hoon
ok hoons/arvo/gen/hood/unsync.hoon
ok hoons/arvo/gen/hood/verb.hoon
ok hoons/arvo/gen/hood/wipe-ford.hoon
ok hoons/arvo/gen/ivory.hoon
ok hoons/arvo/gen/ls.hoon
ok hoons/arvo/gen/metal.hoon
ok hoons/arvo/gen/moon.hoon
ok hoons/arvo/gen/musk.hoon
ok hoons/arvo/gen/pipe/cancel.hoon
ok hoons/arvo/gen/pipe/connect.hoon
ok hoons/arvo/gen/pipe/list.hoon
ok hoons/arvo/gen/pope.hoon
ok hoons/arvo/gen/serving.hoon
ok hoons/arvo/gen/solid.hoon
ok hoons/arvo/gen/static/build.hoon
ok hoons/arvo/gen/ticket.hoon
ok hoons/arvo/gen/tree.hoon
ok hoons/arvo/gen/twit/as.hoon
ok hoons/arvo/gen/twit/feed.hoon
ok hoons/arvo/gen/womb/balance.hoon
ok hoons/arvo/gen/womb/balances.hoon
ok hoons/arvo/gen/womb/shop.hoon
ok hoons/arvo/gen/womb/stats.hoon
todo hoons/arvo/app/ask.hoon
todo hoons/arvo/app/curl.hoon
todo hoons/arvo/app/dojo.hoon
todo hoons/arvo/app/fora.hoon
todo hoons/arvo/app/gh.hoon
todo hoons/arvo/app/github.hoon
todo hoons/arvo/app/gmail.hoon
todo hoons/arvo/app/gmail/split.hoon
todo hoons/arvo/app/hall.hoon
todo hoons/arvo/app/hood.hoon
todo hoons/arvo/app/pipe.hoon
todo hoons/arvo/app/static.hoon
todo hoons/arvo/app/talk.hoon
todo hoons/arvo/app/test.hoon
todo hoons/arvo/app/time.hoon
todo hoons/arvo/app/twit.hoon
todo hoons/arvo/gen/capitalize.hoon
todo hoons/arvo/gen/test.hoon
todo hoons/arvo/lib/basic-auth.hoon
todo hoons/arvo/lib/connector.hoon
todo hoons/arvo/lib/cram.hoon
todo hoons/arvo/lib/down-jet.hoon
todo hoons/arvo/lib/down-jet/parse.hoon
todo hoons/arvo/lib/down-jet/rend.hoon
todo hoons/arvo/lib/elem-to-react-json.hoon
todo hoons/arvo/lib/frontmatter.hoon
todo hoons/arvo/lib/gh-parse.hoon
todo hoons/arvo/lib/hall.hoon
todo hoons/arvo/lib/hall-json.hoon
todo hoons/arvo/lib/hall-legacy.hoon
todo hoons/arvo/lib/hep-to-cab.hoon
todo hoons/arvo/lib/hood/drum.hoon
todo hoons/arvo/lib/hood/helm.hoon
todo hoons/arvo/lib/hood/kiln.hoon
todo hoons/arvo/lib/hood/womb.hoon
todo hoons/arvo/lib/hood/write.hoon
todo hoons/arvo/lib/http.hoon
todo hoons/arvo/lib/httr-to-json.hoon
todo hoons/arvo/lib/interpolate.hoon
todo hoons/arvo/lib/map-to-json.hoon
todo hoons/arvo/lib/new-hoon.hoon
todo hoons/arvo/lib/oauth1.hoon
todo hoons/arvo/lib/oauth2.hoon
todo hoons/arvo/lib/old-phon.hoon
todo hoons/arvo/lib/old-zuse.hoon
todo hoons/arvo/lib/pretty-file.hoon
todo hoons/arvo/lib/prey.hoon
todo hoons/arvo/lib/show-dir.hoon
todo hoons/arvo/lib/sole.hoon
todo hoons/arvo/lib/tester.hoon
todo hoons/arvo/lib/time-to-id.hoon
todo hoons/arvo/lib/tree.hoon
todo hoons/arvo/lib/twitter.hoon
todo hoons/arvo/lib/urb-split.hoon
todo hoons/arvo/mar/ask-mail.hoon
todo hoons/arvo/mar/atom.hoon
todo hoons/arvo/mar/coffee.hoon
todo hoons/arvo/mar/css.hoon
todo hoons/arvo/mar/dill/belt.hoon
todo hoons/arvo/mar/dill/blit.hoon
todo hoons/arvo/mar/down.hoon
todo hoons/arvo/mar/drum-put.hoon
todo hoons/arvo/mar/elem.hoon
todo hoons/arvo/mar/email.hoon
todo hoons/arvo/mar/fora/comment.hoon
todo hoons/arvo/mar/fora/post.hoon
todo hoons/arvo/mar/front.hoon
todo hoons/arvo/mar/gh/commit.hoon
todo hoons/arvo/mar/gh/issue-comment.hoon
todo hoons/arvo/mar/gh/issue.hoon
todo hoons/arvo/mar/gh/issues.hoon
todo hoons/arvo/mar/gh/list-issues.hoon
todo hoons/arvo/mar/gh/poke.hoon
todo hoons/arvo/mar/gh/repository.hoon
todo hoons/arvo/mar/gmail/req.hoon
todo hoons/arvo/mar/hall/action.hoon
todo hoons/arvo/mar/hall/command.hoon
todo hoons/arvo/mar/hall/prize.hoon
todo hoons/arvo/mar/hall/rumor.hoon
todo hoons/arvo/mar/hall/speeches.hoon
todo hoons/arvo/mar/hall/telegrams.hoon
todo hoons/arvo/mar/helm-hi.hoon
todo hoons/arvo/mar/hoon.hoon
todo hoons/arvo/mar/html.hoon
todo hoons/arvo/mar/httr.hoon
todo hoons/arvo/mar/hymn.hoon
todo hoons/arvo/mar/jam-crub.hoon
todo hoons/arvo/mar/jam.hoon
todo hoons/arvo/mar/js.hoon
todo hoons/arvo/mar/json.hoon
todo hoons/arvo/mar/lens/command.hoon
todo hoons/arvo/mar/lens/json.hoon
todo hoons/arvo/mar/markdown.hoon
todo hoons/arvo/mar/md.hoon
todo hoons/arvo/mar/mime.hoon
todo hoons/arvo/mar/noun.hoon
todo hoons/arvo/mar/path.hoon
todo hoons/arvo/mar/plan-diff.hoon
todo hoons/arvo/mar/plan.hoon
todo hoons/arvo/mar/purl.hoon
todo hoons/arvo/mar/quri.hoon
todo hoons/arvo/mar/recoverable-error.hoon
todo hoons/arvo/mar/rss-xml.hoon
todo hoons/arvo/mar/ships.hoon
todo hoons/arvo/mar/snip.hoon
todo hoons/arvo/mar/sole/action.hoon
todo hoons/arvo/mar/sole/effect.hoon
todo hoons/arvo/mar/static/action.hoon
todo hoons/arvo/mar/tang.hoon
todo hoons/arvo/mar/tree/comments.hoon
todo hoons/arvo/mar/tree/elem.hoon
todo hoons/arvo/mar/tree/hymn.hoon
todo hoons/arvo/mar/tree/include.hoon
todo hoons/arvo/mar/tree/index.hoon
todo hoons/arvo/mar/tree/json.hoon
todo hoons/arvo/mar/twit/cred.hoon
todo hoons/arvo/mar/twit/feed.hoon
todo hoons/arvo/mar/twit/post.hoon
todo hoons/arvo/mar/twit/req.hoon
todo hoons/arvo/mar/twit/usel.hoon
todo hoons/arvo/mar/txt-diff.hoon
todo hoons/arvo/mar/txt.hoon
todo hoons/arvo/mar/umd.hoon
todo hoons/arvo/mar/unicode-data.hoon
todo hoons/arvo/mar/urb.hoon
todo hoons/arvo/mar/urbit.hoon
todo hoons/arvo/mar/will.hoon
todo hoons/arvo/mar/womb/balance.hoon
todo hoons/arvo/mar/womb/bonus.hoon
todo hoons/arvo/mar/womb/claim.hoon
todo hoons/arvo/mar/womb/do-claim.hoon
todo hoons/arvo/mar/womb/do-ticket.hoon
todo hoons/arvo/mar/womb/invite.hoon
todo hoons/arvo/mar/womb/part.hoon
todo hoons/arvo/mar/womb/recycle.hoon
todo hoons/arvo/mar/womb/replay-log.hoon
todo hoons/arvo/mar/womb/stat-all.hoon
todo hoons/arvo/mar/womb/ticket-info.hoon
todo hoons/arvo/mar/write/paste.hoon
todo hoons/arvo/mar/write/plan-info.hoon
todo hoons/arvo/mar/write/tree.hoon
todo hoons/arvo/mar/write/wipe.hoon
todo hoons/arvo/mar/xml.hoon
todo hoons/arvo/ren/css.hoon
todo hoons/arvo/ren/js.hoon
todo hoons/arvo/ren/rss-xml.hoon
todo hoons/arvo/ren/run.hoon
todo hoons/arvo/ren/test-tree.hoon
todo hoons/arvo/ren/tree/body.hoon
todo hoons/arvo/ren/tree/combine.hoon
todo hoons/arvo/ren/tree/comments.hoon
todo hoons/arvo/ren/tree/elem.hoon
todo hoons/arvo/ren/tree/head.hoon
todo hoons/arvo/ren/tree/include.hoon
todo hoons/arvo/ren/tree/index.hoon
todo hoons/arvo/ren/tree/json.hoon
todo hoons/arvo/ren/urb.hoon
todo hoons/arvo/ren/urb/tree.hoon
todo hoons/arvo/sec/com/asana.hoon
todo hoons/arvo/sec/com/digitalocean.hoon
todo hoons/arvo/sec/com/dropboxapi.hoon
todo hoons/arvo/sec/com/facebook.hoon
todo hoons/arvo/sec/com/github.hoon
todo hoons/arvo/sec/com/googleapis.hoon
todo hoons/arvo/sec/com/instagram.hoon
todo hoons/arvo/sec/com/slack.hoon
todo hoons/arvo/sec/com/twitter.hoon
todo hoons/arvo/sur/down.hoon
todo hoons/arvo/sur/gh.hoon
todo hoons/arvo/sur/gmail-label.hoon
todo hoons/arvo/sur/gmail-message.hoon
todo hoons/arvo/sur/hall.hoon
todo hoons/arvo/sur/kyev.hoon
todo hoons/arvo/sur/lens.hoon
todo hoons/arvo/sur/markdown.hoon
todo hoons/arvo/sur/plan/acct.hoon
todo hoons/arvo/sur/plan/diff.hoon
todo hoons/arvo/sur/recoverable-error.hoon
todo hoons/arvo/sur/rfc.hoon
todo hoons/arvo/sur/sole.hoon
todo hoons/arvo/sur/static.hoon
todo hoons/arvo/sur/tree-include.hoon
todo hoons/arvo/sur/twitter.hoon
todo hoons/arvo/sur/unicode-data.hoon
todo hoons/arvo/sys/arvo.hoon
todo hoons/arvo/sys/hoon.hoon
todo hoons/arvo/sys/vane/ames.hoon
todo hoons/arvo/sys/vane/behn.hoon
todo hoons/arvo/sys/vane/clay.hoon
todo hoons/arvo/sys/vane/dill.hoon
todo hoons/arvo/sys/vane/eyre.hoon
todo hoons/arvo/sys/vane/ford.hoon
todo hoons/arvo/sys/vane/gall.hoon
todo hoons/arvo/sys/vane/jael.hoon
todo hoons/arvo/sys/vane/xmas.hoon
todo hoons/arvo/sys/zuse.hoon
todo hoons/arvo/tests/new-hoon/ls.hoon
todo hoons/arvo/tests/new-hoon/mp.hoon
todo hoons/arvo/tests/new-hoon/myb.hoon
todo hoons/arvo/tests/new-hoon/thr.hoon
todo hoons/arvo/tests/zuse/crypto/keccak.hoon
todo hoons/arvo/web/404.hoon
todo hoons/arvo/web/dojo.hoon
todo hoons/arvo/web/listen.hoon
todo hoons/arvo/web/pack/css/codemirror-fonts-bootstrap-tree.hoon
todo hoons/arvo/web/pack/js/tree-urb.hoon
todo hoons/arvo/web/talk.hoon
todo hoons/arvo/web/unmark/all.hoon
todo hoons/arvo/web/unmark/test.hoon
todo hoons/arvo/web/womb.hoon
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
