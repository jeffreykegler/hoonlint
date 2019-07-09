perl -I. -ILint Lint/hoonlint.pl -S Lint/anomaly.suppressions -S Lint/later.suppressions hoons/"$1" > junk.$$.1
egrep "$1" Lint/arvo.lint.out | diff junk.$$.1 -
rm junk.$$.1
