# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => 622;

require "./yahc.pm";

my $fileList = <<'END_OF_LIST';
no hoons/arvo/ren/tree/head.hoon
no hoons/arvo/web/unmark/test.hoon
no hoons/arvo/app/hall.hoon
no hoons/arvo/web/unmark/all.hoon
no hoons/arvo/ren/tree/index.hoon
no hoons/arvo/web/pack/css/codemirror-fonts-bootstrap-tree.hoon
no hoons/arvo/web/pack/js/tree-urb.hoon
no hoons/arvo/ren/rss-xml.hoon
no hoons/arvo/ren/test-tree.hoon
no hoons/arvo/ren/tree/body.hoon
no hoons/arvo/ren/tree/combine.hoon
no hoons/arvo/ren/tree/elem.hoon
no hoons/arvo/ren/tree/include.hoon
no hoons/arvo/ren/css.hoon
no hoons/arvo/ren/js.hoon
no hoons/arvo/ren/run.hoon
no hoons/arvo/ren/urb.hoon
no hoons/arvo/ren/urb/tree.hoon
no hoons/arvo/ren/tree/comments.hoon
no hoons/arvo/gen/test.hoon
no hoons/arvo/ren/tree/json.hoon
no hoons/arvo/gen/capitalize.hoon
no hoons/arvo/mar/urb.hoon
no hoons/arvo/app/gmail.hoon
no hoons/arvo/app/hood.hoon
no hoons/arvo/lib/down-jet.hoon
no hoons/arvo/mar/rss-xml.hoon
no hoons/arvo/web/dojo.hoon
no hoons/arvo/sys/vane/eyre.hoon
no hoons/arvo/sys/hoon.hoon
no hoons/arvo/sur/twitter.hoon
no hoons/arvo/mar/css.hoon
no hoons/arvo/mar/elem.hoon
no hoons/arvo/mar/js.hoon
no hoons/arvo/mar/snip.hoon
no hoons/arvo/mar/down.hoon
no hoons/arvo/lib/hood/kiln.hoon
no hoons/arvo/mar/ask-mail.hoon
no hoons/arvo/mar/txt.hoon
no hoons/arvo/sys/arvo.hoon
no hoons/arvo/sys/vane/clay.hoon
no hoons/arvo/mar/helm-hi.hoon
no hoons/arvo/mar/hoon.hoon
no hoons/arvo/lib/down-jet/parse.hoon
no hoons/arvo/web/listen.hoon
no hoons/arvo/web/womb.hoon
no hoons/arvo/web/talk.hoon
no hoons/arvo/web/404.hoon
no hoons/arvo/tests/zuse/crypto/keccak.hoon
no hoons/arvo/tests/new-hoon/thr.hoon
no hoons/arvo/tests/new-hoon/myb.hoon
no hoons/arvo/tests/new-hoon/mp.hoon
no hoons/arvo/tests/new-hoon/ls.hoon
no hoons/arvo/sys/zuse.hoon
no hoons/arvo/sys/vane/xmas.hoon
no hoons/arvo/sys/vane/jael.hoon
no hoons/arvo/sys/vane/gall.hoon
no hoons/arvo/sys/vane/ford.hoon
no hoons/arvo/sys/vane/dill.hoon
no hoons/arvo/sys/vane/behn.hoon
no hoons/arvo/sys/vane/ames.hoon
no hoons/arvo/sur/unicode-data.hoon
no hoons/arvo/sur/tree-include.hoon
no hoons/arvo/sur/static.hoon
no hoons/arvo/sur/sole.hoon
no hoons/arvo/sur/rfc.hoon
no hoons/arvo/sur/recoverable-error.hoon
no hoons/arvo/sur/plan/diff.hoon
no hoons/arvo/sur/plan/acct.hoon
no hoons/arvo/sur/markdown.hoon
no hoons/arvo/sur/lens.hoon
no hoons/arvo/sur/kyev.hoon
no hoons/arvo/sur/hall.hoon
no hoons/arvo/sur/gmail-message.hoon
no hoons/arvo/sur/gmail-label.hoon
no hoons/arvo/sur/gh.hoon
no hoons/arvo/sur/down.hoon
no hoons/arvo/sec/com/twitter.hoon
no hoons/arvo/sec/com/slack.hoon
no hoons/arvo/sec/com/instagram.hoon
no hoons/arvo/sec/com/googleapis.hoon
no hoons/arvo/sec/com/github.hoon
no hoons/arvo/sec/com/facebook.hoon
no hoons/arvo/sec/com/dropboxapi.hoon
no hoons/arvo/sec/com/digitalocean.hoon
no hoons/arvo/sec/com/asana.hoon
no hoons/arvo/mar/xml.hoon
no hoons/arvo/mar/write/wipe.hoon
no hoons/arvo/mar/write/tree.hoon
no hoons/arvo/mar/write/plan-info.hoon
no hoons/arvo/mar/write/paste.hoon
no hoons/arvo/mar/womb/ticket-info.hoon
no hoons/arvo/mar/womb/stat-all.hoon
no hoons/arvo/mar/womb/replay-log.hoon
no hoons/arvo/mar/womb/recycle.hoon
no hoons/arvo/mar/womb/part.hoon
no hoons/arvo/mar/womb/invite.hoon
no hoons/arvo/mar/womb/do-ticket.hoon
no hoons/arvo/mar/womb/do-claim.hoon
no hoons/arvo/mar/womb/claim.hoon
no hoons/arvo/mar/womb/bonus.hoon
no hoons/arvo/mar/womb/balance.hoon
no hoons/arvo/mar/will.hoon
no hoons/arvo/mar/urbit.hoon
no hoons/arvo/mar/unicode-data.hoon
no hoons/arvo/mar/umd.hoon
no hoons/arvo/mar/txt-diff.hoon
no hoons/arvo/mar/twit/usel.hoon
no hoons/arvo/mar/twit/req.hoon
no hoons/arvo/mar/twit/post.hoon
no hoons/arvo/mar/twit/feed.hoon
no hoons/arvo/mar/twit/cred.hoon
no hoons/arvo/mar/tree/json.hoon
no hoons/arvo/mar/tree/index.hoon
no hoons/arvo/mar/tree/include.hoon
no hoons/arvo/mar/tree/hymn.hoon
no hoons/arvo/mar/tree/elem.hoon
no hoons/arvo/mar/tree/comments.hoon
no hoons/arvo/mar/tang.hoon
no hoons/arvo/mar/static/action.hoon
no hoons/arvo/mar/sole/effect.hoon
no hoons/arvo/mar/sole/action.hoon
no hoons/arvo/mar/ships.hoon
no hoons/arvo/mar/recoverable-error.hoon
no hoons/arvo/mar/quri.hoon
no hoons/arvo/mar/purl.hoon
no hoons/arvo/mar/plan.hoon
no hoons/arvo/mar/plan-diff.hoon
no hoons/arvo/mar/path.hoon
no hoons/arvo/mar/noun.hoon
no hoons/arvo/mar/mime.hoon
no hoons/arvo/mar/md.hoon
no hoons/arvo/mar/markdown.hoon
no hoons/arvo/mar/lens/json.hoon
no hoons/arvo/mar/lens/command.hoon
no hoons/arvo/mar/json.hoon
no hoons/arvo/mar/jam.hoon
no hoons/arvo/mar/jam-crub.hoon
no hoons/arvo/mar/hymn.hoon
no hoons/arvo/mar/httr.hoon
no hoons/arvo/mar/html.hoon
no hoons/arvo/mar/hall/telegrams.hoon
no hoons/arvo/mar/hall/speeches.hoon
no hoons/arvo/mar/hall/rumor.hoon
no hoons/arvo/mar/hall/prize.hoon
no hoons/arvo/mar/hall/command.hoon
no hoons/arvo/mar/hall/action.hoon
no hoons/arvo/mar/gmail/req.hoon
no hoons/arvo/mar/gh/repository.hoon
no hoons/arvo/mar/gh/poke.hoon
no hoons/arvo/mar/gh/list-issues.hoon
no hoons/arvo/mar/gh/issues.hoon
no hoons/arvo/mar/gh/issue.hoon
no hoons/arvo/mar/gh/issue-comment.hoon
no hoons/arvo/mar/gh/commit.hoon
no hoons/arvo/mar/front.hoon
no hoons/arvo/mar/fora/post.hoon
no hoons/arvo/mar/fora/comment.hoon
no hoons/arvo/mar/email.hoon
no hoons/arvo/mar/drum-put.hoon
no hoons/arvo/mar/dill/blit.hoon
no hoons/arvo/mar/dill/belt.hoon
no hoons/arvo/mar/coffee.hoon
no hoons/arvo/mar/atom.hoon
no hoons/arvo/lib/urb-split.hoon
no hoons/arvo/lib/twitter.hoon
no hoons/arvo/lib/tree.hoon
no hoons/arvo/lib/time-to-id.hoon
no hoons/arvo/lib/tester.hoon
no hoons/arvo/lib/sole.hoon
no hoons/arvo/lib/show-dir.hoon
no hoons/arvo/lib/prey.hoon
no hoons/arvo/lib/pretty-file.hoon
no hoons/arvo/lib/old-zuse.hoon
no hoons/arvo/lib/old-phon.hoon
no hoons/arvo/lib/oauth2.hoon
no hoons/arvo/lib/oauth1.hoon
no hoons/arvo/lib/new-hoon.hoon
no hoons/arvo/lib/map-to-json.hoon
no hoons/arvo/lib/interpolate.hoon
no hoons/arvo/lib/httr-to-json.hoon
no hoons/arvo/lib/http.hoon
no hoons/arvo/lib/hood/write.hoon
no hoons/arvo/lib/hood/womb.hoon
no hoons/arvo/lib/hood/helm.hoon
no hoons/arvo/lib/hood/drum.hoon
no hoons/arvo/lib/hep-to-cab.hoon
no hoons/arvo/lib/hall-legacy.hoon
no hoons/arvo/lib/hall-json.hoon
no hoons/arvo/lib/hall.hoon
no hoons/arvo/lib/gh-parse.hoon
no hoons/arvo/lib/frontmatter.hoon
no hoons/arvo/lib/elem-to-react-json.hoon
no hoons/arvo/lib/down-jet/rend.hoon
no hoons/arvo/lib/cram.hoon
no hoons/arvo/lib/connector.hoon
no hoons/arvo/lib/basic-auth.hoon
no hoons/arvo/gen/womb/stats.hoon
no hoons/arvo/gen/womb/shop.hoon
no hoons/arvo/gen/womb/balances.hoon
no hoons/arvo/gen/womb/balance.hoon
no hoons/arvo/gen/twit/feed.hoon
no hoons/arvo/gen/twit/as.hoon
no hoons/arvo/gen/tree.hoon
no hoons/arvo/gen/ticket.hoon
no hoons/arvo/gen/static/build.hoon
no hoons/arvo/gen/solid.hoon
no hoons/arvo/gen/serving.hoon
no hoons/arvo/gen/pope.hoon
no hoons/arvo/gen/pipe/list.hoon
no hoons/arvo/gen/pipe/connect.hoon
no hoons/arvo/gen/pipe/cancel.hoon
no hoons/arvo/gen/musk.hoon
no hoons/arvo/gen/moon.hoon
no hoons/arvo/gen/metal.hoon
no hoons/arvo/gen/ls.hoon
no hoons/arvo/gen/ivory.hoon
no hoons/arvo/gen/hood/wipe-ford.hoon
no hoons/arvo/gen/hood/verb.hoon
no hoons/arvo/gen/hood/unsync.hoon
no hoons/arvo/gen/hood/unmount.hoon
no hoons/arvo/gen/hood/unlink.hoon
no hoons/arvo/gen/hood/transfer.hoon
no hoons/arvo/gen/hood/track.hoon
no hoons/arvo/gen/hood/tlon/init-stream.hoon
no hoons/arvo/gen/hood/tlon/add-stream.hoon
no hoons/arvo/gen/hood/tlon/add-fora.hoon
no hoons/arvo/gen/hood/syncs.hoon
no hoons/arvo/gen/hood/sync.hoon
no hoons/arvo/gen/hood/start.hoon
no hoons/arvo/gen/hood/serve.hoon
no hoons/arvo/gen/hood/schedule.hoon
no hoons/arvo/gen/hood/save.hoon
no hoons/arvo/gen/hood/rm.hoon
no hoons/arvo/gen/hood/rf.hoon
no hoons/arvo/gen/hood/reset.hoon
no hoons/arvo/gen/hood/report.hoon
no hoons/arvo/gen/hood/replay-womb-log.hoon
no hoons/arvo/gen/hood/reload.hoon
no hoons/arvo/gen/hood/reload-desk.hoon
no hoons/arvo/gen/hood/release-ships.hoon
no hoons/arvo/gen/hood/release.hoon
no hoons/arvo/gen/hood/rekey.hoon
no hoons/arvo/gen/hood/reinvite.hoon
no hoons/arvo/gen/hood/reboot.hoon
no hoons/arvo/gen/hood/rc.hoon
no hoons/arvo/gen/hood/public.hoon
no hoons/arvo/gen/hood/private.hoon
no hoons/arvo/gen/hood/ping.hoon
no hoons/arvo/gen/hood/overload.hoon
no hoons/arvo/gen/hood/obey.hoon
no hoons/arvo/gen/hood/nuke.hoon
no hoons/arvo/gen/hood/mv.hoon
no hoons/arvo/gen/hood/mount.hoon
no hoons/arvo/gen/hood/merge.hoon
no hoons/arvo/gen/hood/mass.hoon
no hoons/arvo/gen/hood/manage-old-key.hoon
no hoons/arvo/gen/hood/manage.hoon
no hoons/arvo/gen/hood/load.hoon
no hoons/arvo/gen/hood/link.hoon
no hoons/arvo/gen/hood/label.hoon
no hoons/arvo/gen/hood/invite.hoon
no hoons/arvo/gen/hood/init-oauth2.hoon
no hoons/arvo/gen/hood/init-oauth2/google.hoon
no hoons/arvo/gen/hood/init-oauth1.hoon
no hoons/arvo/gen/hood/init-auth-basic.hoon
no hoons/arvo/gen/hood/hi.hoon
no hoons/arvo/gen/hood/exit.hoon
no hoons/arvo/gen/hood/deset.hoon
no hoons/arvo/gen/hood/cp.hoon
no hoons/arvo/gen/hood/commit.hoon
no hoons/arvo/gen/hood/claim.hoon
no hoons/arvo/gen/hood/cancel.hoon
no hoons/arvo/gen/hood/breload.hoon
no hoons/arvo/gen/hood/bonus.hoon
no hoons/arvo/gen/hood/begin.hoon
no hoons/arvo/gen/hood/autoload.hoon
no hoons/arvo/gen/hood/ask.hoon
no hoons/arvo/gen/help.hoon
no hoons/arvo/gen/hello.hoon
no hoons/arvo/gen/hall/unlog.hoon
no hoons/arvo/gen/hall/save.hoon
no hoons/arvo/gen/hall/log.hoon
no hoons/arvo/gen/hall/load-legacy.hoon
no hoons/arvo/gen/hall/load.hoon
no hoons/arvo/gen/gmail/send.hoon
no hoons/arvo/gen/gmail/list.hoon
no hoons/arvo/gen/glass.hoon
no hoons/arvo/gen/deco.hoon
no hoons/arvo/gen/curl/url.hoon
no hoons/arvo/gen/curl.hoon
no hoons/arvo/gen/curl-hiss.hoon
no hoons/arvo/gen/code.hoon
no hoons/arvo/gen/cat.hoon
no hoons/arvo/gen/bug.hoon
no hoons/arvo/gen/brass.hoon
no hoons/arvo/gen/ask/admins.hoon
no hoons/arvo/gen/al.hoon
no hoons/arvo/app/twit.hoon
no hoons/arvo/app/time.hoon
no hoons/arvo/app/test.hoon
no hoons/arvo/app/talk.hoon
no hoons/arvo/app/static.hoon
no hoons/arvo/app/pipe.hoon
no hoons/arvo/app/gmail/split.hoon
no hoons/arvo/app/github.hoon
no hoons/arvo/app/gh.hoon
no hoons/arvo/app/fora.hoon
no hoons/arvo/app/dojo.hoon
no hoons/arvo/app/curl.hoon
no hoons/arvo/app/ask.hoon
END_OF_LIST

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

sub doNode {
    my (undef, @stuff) = @_;
    say STDERR "HI";
}

my $semantics = <<'EOS';
:default ::= action => [name,values]
EOS

FILE: for my $fileLine (split "\n", $fileList) {
    my $origLine = $fileLine;
    chomp $fileLine;
    $fileLine =~ s/\s*[#].*$//xmsg; # Eliminate comments
    $fileLine =~ s/^\s*//xmsg; # Eliminate leading space
    $fileLine =~ s/\s*$//xmsg; # Eliminate trailing space
    next FILE unless $fileLine;

    my ($testStatus, $fileName) = split /\s+/, $fileLine;
    $testStatus //= "Misformed line: $origLine";

    next FILE if $testStatus eq 'no';

    open my $fh, '<', $fileName or die "Cannot open $fileName";
    my $testName = $fileName;
    $testName =~ s/^hoons\///;
    $testName = "Test of " . $testName;
    my $hoonSource = do { local $RS = undef; <$fh>; };
    my $parser = MarpaX::YAHC::new(semantics => $semantics);
    my $valueRef = $parser->read(\$hoonSource);
    say STDERR Data::Dumper::Dumper($valueRef);
}

# vim: expandtab shiftwidth=4:
