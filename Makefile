

.PHONY: all test test_suite undoc_tests

all: test

test: test_suite undoc_tests

test_suite: fizzbuzz sieve_b

yahc.pm: yahc.PM
	perl -I. yahc.PM > yahc.pm

fizzbuzz: yahc.pm fizzbuzz.hoon
	perl -I. yahcfilt.pl <fizzbuzz.hoon >fizzbuzz.ast.try 2>&1
	diff fizzbuzz.ast.try fizzbuzz.ast || echo 'fizzbuzz example !FAILED!'

sieve_b: yahc.pm sieve_b.hoon
	perl -I. yahcfilt.pl <sieve_b.hoon >sieve_b.ast.try 2>&1
	diff sieve_b.ast.try sieve_b.ast || echo 'sieve_b example !FAILED!'

sieve_k: yahc.pm sieve_k.hoon
	perl -I. yahcfilt.pl <sieve_k.hoon
	# perl -I. yahcfilt.pl <sieve_k.hoon >sieve_k.ast.try 2>&1
	# diff sieve_k.ast.try sieve_k.ast || echo 'sieve_k example !FAILED!'

undoc_tests:

ast_reset: yahc.pm
	perl -I. yahcfilt.pl <fizzbuzz.hoon >fizzbuzz.ast
	perl -I. yahcfilt.pl <sieve_b.hoon >sieve_b.ast

dev: sieve_k
