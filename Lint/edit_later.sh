#!/bin/sh
TMP=junk.$$.1
TARGET=Lint/later.suppressions
sed -e '/### PENDING ISSUES AFTER HERE$/q' $TARGET > $TMP
for f in issues/*.include
do
echo >> $TMP
echo '#  ===' $f === >> $TMP
cat $f >> $TMP
done
mv $TMP $TARGET
