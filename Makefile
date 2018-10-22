

.PHONY: all test test_suite undoc_tests

all: test

test: test_suite undoc_tests

test_suite: fizzbuzz

fizzbuzz: fizzbuzz.hoon
	perl -I. yahcfilt.pl <fizzbuzz.hoon >fizzbuzz.ast.try 2>&1
	diff fizzbuzz.ast.try fizzbuzz.ast || echo 'fizzbuzz example !FAILED!'

undoc_tests:
