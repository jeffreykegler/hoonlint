#!/bin/bash
set +o posix
if ! test -r hoons/"$1"
then
    echo hoons/"$1" is not readable 1>&2
    exit 1
fi
perl -I. -ILint Lint/hoonlint.pl -S Lint/anomaly.suppressions -S Lint/later.suppressions hoons/"$1" |
diff - <(egrep "$1" Lint/arvo.lint.out)
