EMACS=emacs

EMACS_CLEAN=-Q
EMACS_BATCH=$(EMACS_CLEAN) --batch
#TESTS=ergoemacs-test-sh
TESTS=ergoemacs-

CURL=curl --silent
TMP1=$(shell pwd)
EMPTY=
BACKSLASH=\$(EMPTY)
SLASH=/
WORK_DIR=$(subst $(BACKSLASH),$(SLASH),$(TMP1))
PACKAGE_NAME=$(shell basename $(TMP1))
AUTOLOADS_FILE=$(PACKAGE_NAME)-autoloads.el
TRAVIS_FILE=.travis.yml
TEST_DIR=$(WORK_DIR)/deps
TEST_DEP_1=ert
TEST_DEP_1_STABLE_URL=http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el?h=emacs-24.3
TEST_DEP_1_LATEST_URL=http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el?h=master

.PHONY : build downloads downloads-latest autoloads test-autoloads test-travis \
         test test-interactive clean edit test-dep-1 test-dep-2 test-dep-3     \
         test-dep-4 test-dep-5 test-dep-6 test-dep-7 test-dep-8 test-dep-9

build :
	$(EMACS) $(EMACS_BATCH) --eval             \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" *.el

test-dep-1 :
	@cd $(TEST_DIR)                                      && \
	$(EMACS) $(EMACS_BATCH)  -L . -L .. -l $(TEST_DEP_1) || \
	(echo "Can't load test dependency $(TEST_DEP_1).el, run 'make downloads' to fetch it" ; exit 1)

downloads :
	@mkdir $(TEST_DIR)
	$(CURL) '$(TEST_DEP_1_STABLE_URL)' > $(TEST_DIR)/$(TEST_DEP_1).el

downloads-latest :
	@mkdir $(TEST_DIR)
	$(CURL) '$(TEST_DEP_1_LATEST_URL)' > $(TEST_DIR)/$(TEST_DEP_1).el

autoloads :
	@cd $(WORK_DIR)
	$(EMACS) $(EMACS_BATCH) --eval                       \
	    "(progn                                          \
	      (setq generated-autoload-file \"$(WORK_DIR)/$(AUTOLOADS_FILE)\") \
	      (update-directory-autoloads \"$(WORK_DIR)\"))"

test-autoloads : autoloads
	@$(EMACS) $(EMACS_BATCH) -L . -l "./$(AUTOLOADS_FILE)"      || \
	 ( echo "failed to load autoloads: $(AUTOLOADS_FILE)" && false )

test-travis :
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis-lint $(TRAVIS_FILE); fi

test : build test-dep-1 test-autoloads
	@cd $(TEST_DIR)                                   && \
	$(EMACS) $(EMACS_BATCH) -L . -L .. -L $(TEST_DIR) -l cl -l $(TEST_DEP_1) -l ergoemacs-mode -l ergoemacs-test --eval \
	    "(progn                                          \
	      (fset 'ert--print-backtrace 'ignore)           \
	      (ert-run-tests-batch-and-exit '(and \"$(TESTS)\" (not (tag :interactive)))))" || exit 1; \

clean :
	@rm -f $(AUTOLOADS_FILE) 
	@rm -f *.elc 
	@rm -f *~ 
	@rm -f */*.elc
	@rm -f */*~
	@rm -f $(TEST_DIR)/$(TEST_DEP_1).el            \
        $(TEST_DIR)/$(TEST_DEP_2).el $(TEST_DIR)/$(TEST_DEP_3).el $(TEST_DIR)/$(TEST_DEP_4).el \
        $(TEST_DIR)/$(TEST_DEP_5).el $(TEST_DIR)/$(TEST_DEP_6).el $(TEST_DIR)/$(TEST_DEP_7).el \
        $(TEST_DIR)/$(TEST_DEP_8).el $(TEST_DIR)/$(TEST_DEP_9).el
