
.PHONY: dummy basic_test rebuild single_test full_test install releng \
  cpan_dir_full_test doc

dummy: 

releng: 
	@echo === releng target ===
	@echo === releng: make install ===
	$(MAKE) install
	@echo === releng: make full_test ===
	$(MAKE) full_test 2>&1 | tee full_test.out
	@echo === releng: make distcheck ===
	cd cpan && $(MAKE) distcheck
	@echo === releng: make dist ===
	cd cpan && GZIP=-f $(MAKE) dist
	@echo === releng: make disttest ===
	cd cpan && $(MAKE) disttest
	@echo === xt: prove -v xt ===
	cd cpan && prove -v xt
	@echo === releng: git status ===
	git status

basic_test:
	(cd cpan && $(MAKE) test) 2>&1 | tee basic_test.out

rebuild:
	(cd cpan; \
	    $(MAKE); \
	    $(MAKE) test; \
	) 2>&1 | tee rebuild.out

single_test:
	(cd cpan; \
	    $(MAKE); \
	    $(MAKE) test TEST_FILES='$(TEST)'; \
	) 2>&1 | tee single_test.out

full_test: cpan_dir_full_test

cpan_dir_full_test:
	@echo === cpan_dir_full_test target ===
	@echo === cpan_dir_full_test: make realclean ===
	cd cpan && $(MAKE) realclean
	@echo === cpan_dir_full_test: perl Makefile.PL ===
	cd cpan && perl Makefile.PL
	@echo === cpan_dir_full_test: make ===
	cd cpan && $(MAKE)
	@echo === cpan_dir_full_test: make test ===
	cd cpan && $(MAKE) test
	@echo === cpan_dir_full_test: make disttest ===
	cd cpan && $(MAKE) disttest

install:
	@echo === target: perl Makefile.PL ===
	(cd cpan && perl Makefile.PL)
	@echo === target: make ===
	(cd cpan && $(MAKE))

doc:
	cd misc; make all

