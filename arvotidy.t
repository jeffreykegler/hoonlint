# Test of hoontidy utility "round trip" option

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => (311*3);

use Text::Diff ();
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

my $files = <<'EOS';
hoons/arvo/sur/gmail-message.hoon ok
hoons/arvo/sur/kyev.hoon ok
hoons/arvo/sur/plan/diff.hoon ok
hoons/arvo/sur/plan/acct.hoon ok
hoons/arvo/sur/static.hoon ok
hoons/arvo/sur/down.hoon ok
hoons/arvo/sur/sole.hoon ok
hoons/arvo/sur/gh.hoon ok
hoons/arvo/sur/tree-include.hoon ok
hoons/arvo/sur/recoverable-error.hoon ok
hoons/arvo/sur/twitter.hoon ok
hoons/arvo/sur/rfc.hoon ok
hoons/arvo/sur/markdown.hoon ok
hoons/arvo/sur/unicode-data.hoon ok
hoons/arvo/sur/hall.hoon ok
hoons/arvo/sur/gmail-label.hoon ok
hoons/arvo/sur/lens.hoon ok
hoons/arvo/tests/new-hoon/mp.hoon ok
hoons/arvo/tests/new-hoon/ls.hoon ok
hoons/arvo/tests/new-hoon/thr.hoon ok
hoons/arvo/tests/new-hoon/myb.hoon ok
hoons/arvo/tests/zuse/crypto/keccak.hoon ok
hoons/arvo/sys/zuse.hoon ok
hoons/arvo/sys/hoon.hoon ok
hoons/arvo/sys/arvo.hoon ok
hoons/arvo/sys/vane/behn.hoon ok
hoons/arvo/sys/vane/dill.hoon ok
hoons/arvo/sys/vane/ford.hoon ok
hoons/arvo/sys/vane/ames.hoon ok
hoons/arvo/sys/vane/gall.hoon ok
hoons/arvo/sys/vane/eyre.hoon todo
hoons/arvo/sys/vane/xmas.hoon ok
hoons/arvo/sys/vane/clay.hoon ok
hoons/arvo/sys/vane/jael.hoon ok
hoons/arvo/app/gmail.hoon ok
hoons/arvo/app/test.hoon ok
hoons/arvo/app/gmail/split.hoon ok
hoons/arvo/app/twit.hoon ok
hoons/arvo/app/fora.hoon ok
hoons/arvo/app/static.hoon ok
hoons/arvo/app/hood.hoon ok
hoons/arvo/app/dojo.hoon ok
hoons/arvo/app/time.hoon ok
hoons/arvo/app/gh.hoon ok
hoons/arvo/app/ask.hoon todo
hoons/arvo/app/pipe.hoon ok
hoons/arvo/app/talk.hoon ok
hoons/arvo/app/curl.hoon ok
hoons/arvo/app/github.hoon ok
hoons/arvo/app/hall.hoon ok
hoons/arvo/sec/com/digitalocean.hoon ok
hoons/arvo/sec/com/asana.hoon ok
hoons/arvo/sec/com/dropboxapi.hoon ok
hoons/arvo/sec/com/googleapis.hoon ok
hoons/arvo/sec/com/instagram.hoon ok
hoons/arvo/sec/com/twitter.hoon ok
hoons/arvo/sec/com/slack.hoon ok
hoons/arvo/sec/com/github.hoon ok
hoons/arvo/sec/com/facebook.hoon ok
hoons/arvo/lib/elem-to-react-json.hoon todo
hoons/arvo/lib/hep-to-cab.hoon ok
hoons/arvo/lib/old-phon.hoon ok
hoons/arvo/lib/interpolate.hoon ok
hoons/arvo/lib/http.hoon ok
hoons/arvo/lib/urb-split.hoon ok
hoons/arvo/lib/connector.hoon ok
hoons/arvo/lib/frontmatter.hoon ok
hoons/arvo/lib/old-zuse.hoon ok
hoons/arvo/lib/tester.hoon ok
hoons/arvo/lib/hall-json.hoon ok
hoons/arvo/lib/prey.hoon ok
hoons/arvo/lib/down-jet/rend.hoon ok
hoons/arvo/lib/down-jet/parse.hoon todo
hoons/arvo/lib/time-to-id.hoon ok
hoons/arvo/lib/httr-to-json.hoon ok
hoons/arvo/lib/sole.hoon ok
hoons/arvo/lib/cram.hoon ok
hoons/arvo/lib/map-to-json.hoon ok
hoons/arvo/lib/tree.hoon ok
hoons/arvo/lib/gh-parse.hoon ok
hoons/arvo/lib/oauth1.hoon todo
hoons/arvo/lib/pretty-file.hoon ok
hoons/arvo/lib/twitter.hoon ok
hoons/arvo/lib/down-jet.hoon ok
hoons/arvo/lib/oauth2.hoon todo
hoons/arvo/lib/basic-auth.hoon ok
hoons/arvo/lib/hall-legacy.hoon ok
hoons/arvo/lib/new-hoon.hoon ok
hoons/arvo/lib/show-dir.hoon ok
hoons/arvo/lib/hood/helm.hoon ok
hoons/arvo/lib/hood/drum.hoon ok
hoons/arvo/lib/hood/kiln.hoon todo
hoons/arvo/lib/hood/write.hoon ok
hoons/arvo/lib/hood/womb.hoon ok
hoons/arvo/lib/hall.hoon ok
hoons/arvo/mar/coffee.hoon ok
hoons/arvo/mar/urb.hoon ok
hoons/arvo/mar/mime.hoon ok
hoons/arvo/mar/quri.hoon ok
hoons/arvo/mar/plan-diff.hoon ok
hoons/arvo/mar/noun.hoon ok
hoons/arvo/mar/gmail/req.hoon ok
hoons/arvo/mar/rss-xml.hoon ok
hoons/arvo/mar/tree/json.hoon ok
hoons/arvo/mar/tree/index.hoon ok
hoons/arvo/mar/tree/include.hoon ok
hoons/arvo/mar/tree/hymn.hoon ok
hoons/arvo/mar/tree/elem.hoon ok
hoons/arvo/mar/tree/comments.hoon ok
hoons/arvo/mar/ships.hoon ok
hoons/arvo/mar/purl.hoon ok
hoons/arvo/mar/hoon.hoon ok
hoons/arvo/mar/lens/command.hoon ok
hoons/arvo/mar/lens/json.hoon ok
hoons/arvo/mar/helm-hi.hoon ok
hoons/arvo/mar/write/wipe.hoon ok
hoons/arvo/mar/write/plan-info.hoon ok
hoons/arvo/mar/write/paste.hoon ok
hoons/arvo/mar/write/tree.hoon ok
hoons/arvo/mar/plan.hoon ok
hoons/arvo/mar/gh/list-issues.hoon ok
hoons/arvo/mar/gh/commit.hoon ok
hoons/arvo/mar/gh/issue-comment.hoon ok
hoons/arvo/mar/gh/issues.hoon ok
hoons/arvo/mar/gh/poke.hoon ok
hoons/arvo/mar/gh/issue.hoon ok
hoons/arvo/mar/gh/repository.hoon ok
hoons/arvo/mar/json.hoon ok
hoons/arvo/mar/email.hoon ok
hoons/arvo/mar/womb/part.hoon ok
hoons/arvo/mar/womb/invite.hoon ok
hoons/arvo/mar/womb/claim.hoon ok
hoons/arvo/mar/womb/bonus.hoon ok
hoons/arvo/mar/womb/do-claim.hoon ok
hoons/arvo/mar/womb/ticket-info.hoon ok
hoons/arvo/mar/womb/balance.hoon ok
hoons/arvo/mar/womb/replay-log.hoon ok
hoons/arvo/mar/womb/stat-all.hoon ok
hoons/arvo/mar/womb/do-ticket.hoon ok
hoons/arvo/mar/womb/recycle.hoon ok
hoons/arvo/mar/dill/belt.hoon ok
hoons/arvo/mar/dill/blit.hoon ok
hoons/arvo/mar/down.hoon ok
hoons/arvo/mar/httr.hoon ok
hoons/arvo/mar/tang.hoon ok
hoons/arvo/mar/atom.hoon ok
hoons/arvo/mar/path.hoon ok
hoons/arvo/mar/snip.hoon ok
hoons/arvo/mar/urbit.hoon ok
hoons/arvo/mar/md.hoon ok
hoons/arvo/mar/twit/cred.hoon ok
hoons/arvo/mar/twit/feed.hoon ok
hoons/arvo/mar/twit/post.hoon ok
hoons/arvo/mar/twit/usel.hoon ok
hoons/arvo/mar/twit/req.hoon ok
hoons/arvo/mar/css.hoon ok
hoons/arvo/mar/hymn.hoon ok
hoons/arvo/mar/ask-mail.hoon ok
hoons/arvo/mar/xml.hoon ok
hoons/arvo/mar/recoverable-error.hoon ok
hoons/arvo/mar/jam.hoon ok
hoons/arvo/mar/txt-diff.hoon ok
hoons/arvo/mar/umd.hoon ok
hoons/arvo/mar/elem.hoon ok
hoons/arvo/mar/sole/action.hoon ok
hoons/arvo/mar/sole/effect.hoon ok
hoons/arvo/mar/js.hoon ok
hoons/arvo/mar/markdown.hoon ok
hoons/arvo/mar/front.hoon ok
hoons/arvo/mar/fora/post.hoon ok
hoons/arvo/mar/fora/comment.hoon ok
hoons/arvo/mar/will.hoon ok
hoons/arvo/mar/jam-crub.hoon ok
hoons/arvo/mar/html.hoon ok
hoons/arvo/mar/txt.hoon ok
hoons/arvo/mar/hall/action.hoon ok
hoons/arvo/mar/hall/rumor.hoon ok
hoons/arvo/mar/hall/telegrams.hoon ok
hoons/arvo/mar/hall/speeches.hoon ok
hoons/arvo/mar/hall/command.hoon ok
hoons/arvo/mar/hall/prize.hoon ok
hoons/arvo/mar/unicode-data.hoon ok
hoons/arvo/mar/drum-put.hoon ok
hoons/arvo/mar/static/action.hoon ok
hoons/arvo/web/unmark/test.hoon todo
hoons/arvo/web/unmark/all.hoon ok
hoons/arvo/web/listen.hoon ok
hoons/arvo/web/404.hoon ok
hoons/arvo/web/pack/js/tree-urb.hoon ok
hoons/arvo/web/pack/css/codemirror-fonts-bootstrap-tree.hoon ok
hoons/arvo/web/dojo.hoon ok
hoons/arvo/web/talk.hoon ok
hoons/arvo/web/womb.hoon ok
hoons/arvo/ren/urb.hoon ok
hoons/arvo/ren/rss-xml.hoon ok
hoons/arvo/ren/tree/body.hoon ok
hoons/arvo/ren/tree/json.hoon ok
hoons/arvo/ren/tree/index.hoon ok
hoons/arvo/ren/tree/include.hoon ok
hoons/arvo/ren/tree/elem.hoon ok
hoons/arvo/ren/tree/head.hoon ok
hoons/arvo/ren/tree/combine.hoon ok
hoons/arvo/ren/tree/comments.hoon ok
hoons/arvo/ren/urb/tree.hoon ok
hoons/arvo/ren/css.hoon ok
hoons/arvo/ren/test-tree.hoon ok
hoons/arvo/ren/run.hoon ok
hoons/arvo/ren/js.hoon ok
hoons/arvo/gen/pipe/cancel.hoon ok
hoons/arvo/gen/pipe/connect.hoon ok
hoons/arvo/gen/pipe/list.hoon ok
hoons/arvo/gen/glass.hoon ok
hoons/arvo/gen/test.hoon ok
hoons/arvo/gen/gmail/list.hoon ok
hoons/arvo/gen/gmail/send.hoon ok
hoons/arvo/gen/hello.hoon ok
hoons/arvo/gen/al.hoon ok
hoons/arvo/gen/solid.hoon ok
hoons/arvo/gen/ticket.hoon ok
hoons/arvo/gen/ivory.hoon ok
hoons/arvo/gen/moon.hoon ok
hoons/arvo/gen/womb/balances.hoon ok
hoons/arvo/gen/womb/balance.hoon ok
hoons/arvo/gen/womb/stats.hoon ok
hoons/arvo/gen/womb/shop.hoon ok
hoons/arvo/gen/metal.hoon ok
hoons/arvo/gen/musk.hoon ok
hoons/arvo/gen/serving.hoon ok
hoons/arvo/gen/ls.hoon ok
hoons/arvo/gen/twit/as.hoon ok
hoons/arvo/gen/twit/feed.hoon ok
hoons/arvo/gen/help.hoon ok
hoons/arvo/gen/ask/admins.hoon ok
hoons/arvo/gen/brass.hoon ok
hoons/arvo/gen/tree.hoon ok
hoons/arvo/gen/capitalize.hoon ok
hoons/arvo/gen/bug.hoon ok
hoons/arvo/gen/code.hoon ok
hoons/arvo/gen/curl-hiss.hoon ok
hoons/arvo/gen/cat.hoon ok
hoons/arvo/gen/curl/url.hoon ok
hoons/arvo/gen/curl.hoon ok
hoons/arvo/gen/hood/sync.hoon ok
hoons/arvo/gen/hood/replay-womb-log.hoon ok
hoons/arvo/gen/hood/mv.hoon ok
hoons/arvo/gen/hood/unlink.hoon ok
hoons/arvo/gen/hood/rekey.hoon ok
hoons/arvo/gen/hood/commit.hoon ok
hoons/arvo/gen/hood/reset.hoon ok
hoons/arvo/gen/hood/cp.hoon ok
hoons/arvo/gen/hood/obey.hoon ok
hoons/arvo/gen/hood/label.hoon ok
hoons/arvo/gen/hood/cancel.hoon ok
hoons/arvo/gen/hood/begin.hoon ok
hoons/arvo/gen/hood/manage-old-key.hoon ok
hoons/arvo/gen/hood/invite.hoon ok
hoons/arvo/gen/hood/tlon/init-stream.hoon ok
hoons/arvo/gen/hood/tlon/add-fora.hoon ok
hoons/arvo/gen/hood/tlon/add-stream.hoon ok
hoons/arvo/gen/hood/ping.hoon ok
hoons/arvo/gen/hood/claim.hoon ok
hoons/arvo/gen/hood/hi.hoon ok
hoons/arvo/gen/hood/serve.hoon ok
hoons/arvo/gen/hood/deset.hoon ok
hoons/arvo/gen/hood/reload.hoon ok
hoons/arvo/gen/hood/init-oauth2.hoon ok
hoons/arvo/gen/hood/rm.hoon ok
hoons/arvo/gen/hood/manage.hoon ok
hoons/arvo/gen/hood/autoload.hoon ok
hoons/arvo/gen/hood/schedule.hoon ok
hoons/arvo/gen/hood/reload-desk.hoon ok
hoons/arvo/gen/hood/bonus.hoon ok
hoons/arvo/gen/hood/unmount.hoon ok
hoons/arvo/gen/hood/release-ships.hoon ok
hoons/arvo/gen/hood/exit.hoon ok
hoons/arvo/gen/hood/merge.hoon ok
hoons/arvo/gen/hood/rf.hoon ok
hoons/arvo/gen/hood/report.hoon ok
hoons/arvo/gen/hood/verb.hoon ok
hoons/arvo/gen/hood/mount.hoon ok
hoons/arvo/gen/hood/breload.hoon ok
hoons/arvo/gen/hood/syncs.hoon ok
hoons/arvo/gen/hood/public.hoon ok
hoons/arvo/gen/hood/private.hoon ok
hoons/arvo/gen/hood/nuke.hoon ok
hoons/arvo/gen/hood/mass.hoon ok
hoons/arvo/gen/hood/overload.hoon ok
hoons/arvo/gen/hood/reinvite.hoon ok
hoons/arvo/gen/hood/ask.hoon ok
hoons/arvo/gen/hood/start.hoon ok
hoons/arvo/gen/hood/transfer.hoon ok
hoons/arvo/gen/hood/load.hoon ok
hoons/arvo/gen/hood/track.hoon ok
hoons/arvo/gen/hood/save.hoon ok
hoons/arvo/gen/hood/rc.hoon ok
hoons/arvo/gen/hood/init-oauth2/google.hoon ok
hoons/arvo/gen/hood/wipe-ford.hoon ok
hoons/arvo/gen/hood/init-oauth1.hoon ok
hoons/arvo/gen/hood/init-auth-basic.hoon ok
hoons/arvo/gen/hood/unsync.hoon ok
hoons/arvo/gen/hood/reboot.hoon ok
hoons/arvo/gen/hood/release.hoon ok
hoons/arvo/gen/hood/link.hoon ok
hoons/arvo/gen/pope.hoon ok
hoons/arvo/gen/deco.hoon ok
hoons/arvo/gen/hall/log.hoon ok
hoons/arvo/gen/hall/load-legacy.hoon ok
hoons/arvo/gen/hall/load.hoon ok
hoons/arvo/gen/hall/save.hoon ok
hoons/arvo/gen/hall/unlog.hoon ok
hoons/arvo/gen/static/build.hoon ok
EOS

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

my $cmd = [ 'perl', 'hoontidy.pl', '--style=roundtrip' ];


sub doTest {
    my ( $testStatus, $fileName ) = @_;
    my @stdout       = ();
    my $gatherStdout = sub {
        push @stdout, @_;
    };

    my @stderr       = ();
    my $gatherStderr = sub {
        push @stderr, @_;
    };

    my $pChildStdin = slurp($fileName);

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
    Test::More::ok( $exitCode eq 0, "exit code of $fileName is $exitCode" );

    my $errMsg = $result->{'err_msg'};
    Test::More::diag($errMsg) if $errMsg;

    my $stderr = join q{}, @stderr;
    Test::More::diag($stderr) if $stderr;
    Test::More::ok( $stderr eq q{}, "STDERR of $fileName" );

    my $stdout = join q{}, @stdout;
    my $isSame = $stdout eq ${$pChildStdin};
    TEST: {
    if ( $testStatus eq 'ok' ) {
        Test::More::ok( $isSame, "STDOUT of $fileName" );
        last TEST;
    }
  TODO: {
        local $TODO = 'NYI';
        Test::More::ok( $isSame, "STDOUT of $fileName" );
    }
    }
    diag(Text::Diff::diff(\$stdout, $pChildStdin)) if not $isSame;
}

FILE: for my $testDesc ( split "\n", $files ) {
    $testDesc =~ s/[#].*$//;
    $testDesc =~ s/\s*$//;
    $testDesc =~ s/^\s//;
    next FILE if $testDesc eq q{};
    my ( $fileName, $testStatus ) = ( $testDesc =~ m/(\S+)\s+(\S+)/ );
    doTest( $testStatus, $fileName );
}

# vim: expandtab shiftwidth=4:
