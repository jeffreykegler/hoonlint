for rune in \
barhep \
bartar \
bartis \
buctis \
cendot \
cenket \
dottis \
ketsig \
sigcab \
siglus \
tisbar \
tiscom \
tisdot \
tisgar \
tislus \
wutdot \
wutgal \
wutgar \
zapdot
do
echo "# $rune"
echo 'if ($runename'
echo "eq q{$rune}) {"
egrep "$rune": Lint/arvo.lint.out |
sh add_lint_context 2>&1 |
egrep '^ *[0-9]*[>]' |
perl -anE 'say $F[1]' |
sort |
uniq -c |
sort -k +1rn |
sed -e 's/^/# /'
echo '( $anchorColumn, $anchorData ) = $policy->reanchorInc(
    $node,
            {
                # TODO: TO HERE
            }
        );
    }'
done
