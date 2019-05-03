

.PHONY: all arvo_lint

all:
	echo empty default target

arvo_lint: 
	(cd ..; sh edit_later.sh)
	(cd ..; find hoons -name '*.hoon') | \
	while read f; do \
	  echo === $$f ===; \
	  (cd ..; perl -ILint Lint/hoonlint.pl \
	    -S Lint/arvo.suppressions \
	    -S Lint/anomaly.suppressions \
	    -S Lint/later.suppressions \
	  $$f); \
	done > arvo.lint.out

dev:
	echo empty dev target
