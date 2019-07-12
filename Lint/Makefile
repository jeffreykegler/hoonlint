

.PHONY: all arvo_lint pre_lint

all:
	echo empty default target

pre_lint:
	sh edit_later.sh

arvo_lint: 
	(cd ..; find hoons -name '*.hoon') | \
	while read f; do \
	  echo === $$f ===; \
	  (cd ..; perl -ILint Lint/hoonlint.pl \
	    -S Lint/arvo.suppressions \
	    -S Lint/anomaly.suppressions \
	    -S Lint/later.suppressions \
	  $$f); \
	done 2>&1 | tee arvo.lint.out

dev:
	echo empty dev target
