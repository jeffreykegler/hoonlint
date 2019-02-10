

.PHONY: all arvo_lint

all:
	echo empty default target

arvo_lint: 
	find hoons -name '*.hoon' | \
	while read f; do \
	  echo === $$f ===; \
	  perl hoonlint.pl -S arvo.suppressions -S anomaly.suppressions $$f; \
	done > arvo.lint.out

dev:
	echo empty dev target
