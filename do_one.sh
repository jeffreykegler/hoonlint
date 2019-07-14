#!/bin/bash
F=hoons/arvo/"$1"
if ! test -r $F
then
    echo $F is not readable 1>&2
    exit 1
fi
perl -I. -ILint Lint/hoonlint.pl -S Lint/anomaly.suppressions -S Lint/later.suppressions $F
