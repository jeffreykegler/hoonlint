

.PHONY: all test test_suite undoc_tests \
  msg count_todo

all: test

test: test_suite undoc_tests count_todo

msg: count_todo

test_suite: yahc.pm
	prove --verbose -I. t

yahc.pm: yahc.pm1
	perl yahc.pm1 > yahc.pm

undoc_tests:

count_todo:
	@echo $$(egrep 'Failed .*TODO' errs | wc -l) TODO
	@echo $$(egrep ' Parse is ambiguous' list3.errs | wc -l) Ambiguities

ast_reset: yahc.pm
	perl -I. yahcfilt.pl <t/ast.d/fizzbuzz.hoon >t/ast.d/fizzbuzz.ast
	perl -I. yahcfilt.pl <t/ast.d/sieve_b.hoon >t/ast.d/sieve_b.ast
	perl -I. yahcfilt.pl <t/ast.d/sieve_k.hoon >t/ast.d/sieve_k.ast
	perl -I. yahcfilt.pl <t/ast.d/toe.hoon >t/ast.d/toe.ast

dev:
	echo empty dev target
