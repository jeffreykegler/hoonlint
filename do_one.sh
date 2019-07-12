#!/bin/bash
F=hoons/arvo/"$1"
if ! test -r $F
then
    echo $F is not readable 1>&2
    exit 1
fi
(cd Lint; make pre_lint)
perl -I. -ILint Lint/hoonlint.pl -S Lint/anomaly.suppressions -S Lint/later.suppressions $F
