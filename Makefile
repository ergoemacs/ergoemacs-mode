EMACS=emacs

EMACS_CLEAN=-Q
EMACS_BATCH=$(EMACS_CLEAN) --batch
#TESTS=ergoemacs-test-apps-copy
#TESTS=ergoemacs-test-issue-184-paste
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

.PHONY : build downloads downloads-latest autoloads test-autoloads test-travis \
         test test-interactive clean edit test-dep-1 test-dep-2 test-dep-3     \
         test-dep-4 test-dep-5 test-dep-6 test-dep-7 test-dep-8 test-dep-9

build :
	$(EMACS) $(EMACS_BATCH) -L . --eval             \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" *.el

build2 : 
	$(EMACS) $(EMACS_BATCH) -L . -l ergoemacs-mode --eval             \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" *.el

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

test : build build2 test-dep-1 test-autoloads ert

ert :
	$(EMACS) $(EMACS_BATCH) -L . -L .. -l cl -l ergoemacs-mode -l ergoemacs-test --eval \
	    "(progn                                          \
	      (fset 'ert--print-backtrace 'ignore)           \
	      (ert-run-tests-batch-and-exit '(and \"$(TESTS)\" (not (tag :interactive)))))" || exit 1; \

clean :
	@rm -f $(AUTOLOADS_FILE) 
	@rm -f *.elc 
	@rm -f *~ 
