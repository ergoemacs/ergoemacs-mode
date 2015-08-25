EMACS=emacs

unexport EMACSLOADPATH
unexport EMACSDOC
unexport EMACSDATA
unexport EMACS_SERVER_FILE

EMACS_CLEAN=-Q
EMACS_BATCH=$(EMACS_CLEAN) --batch
TESTS=ergoemacs-
SELECT=

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

24.1 :
	$(eval EMACS:="c:\Users\fidlema3\EmacsPortable.App\App\emacs-24.1\bin\emacs.exe")

24.2 :
	$(eval EMACS:="c:\Users\fidlema3\EmacsPortable.App\App\emacs-24.2\bin\emacs.exe")

24.3 :
	$(eval EMACS:="c:\Users\fidlema3\EmacsPortable.App\App\emacs-24.3\bin\emacs.exe")

24.4 :
	$(eval EMACS:="c:\Users\fidlema3\EmacsPortable.App\App\emacs-24.4\bin\emacs.exe")

24.5 :
	$(eval EMACS:="c:\Users\fidlema3\EmacsPortable.App\App\emacs-24.5\bin\emacs.exe")

25.0 :
	$(eval EMACS:="c:\Users\fidlema3\EmacsPortable.App\App\emacs-24.5\bin\emacs.exe")


search :
	$(eval SELECT:=(tag :search))

copy :
	$(eval SELECT:=(tag :copy))

slow :
	$(eval SELECT:=(tag :slow))

shift-select :
	$(eval SELECT:=(tag :shift-select))

translate :
	$(eval SELECT:=(tag :translate))


other :
	$(eval SELECT:=(not (tag :search)) (not (tag :copy)) (not (tag :slow))  (not (tag :shift-select)) (not (tag :translate)))

default-select :
	$(eval SELECT:=)



build :
	$(EMACS) $(EMACS_BATCH) -L . --eval             \
	    "(progn                                \
	      (batch-byte-compile))" *.el

build2 : 
	$(EMACS) $(EMACS_BATCH) -L . -l ergoemacs-mode --eval             \
	    "(progn                                \
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


test: source compile

compile : clean build build2 test-autoloads erti

source : clean ert

start: clean build start0
startel: clean start0

start0:
	$(EMACS) -Q -L . -L .. -l ergoemacs-mode -l ergoemacs-test --eval "(ergoemacs-mode)"

ert :
	$(EMACS) $(EMACS_BATCH) -L . -L .. -l cl -l ergoemacs-mode -l ergoemacs-test --eval \
	    "(progn (setq ergoemacs-command-loop-type nil)                                          \
	      (ert-run-tests-batch-and-exit '(and \"$(TESTS)\" $(SELECT) (not (tag :interactive)))))" || exit 1; \

erti :
	$(EMACS) $(EMACS_BATCH) -L . -L .. -l cl -l ergoemacs-mode -l ergoemacs-test --eval \
	    "(progn (setq ergoemacs-command-loop-type nil)                                         \
	      (fset 'ert--print-backtrace 'ignore)  \
	      (ert-run-tests-batch-and-exit '(and \"$(TESTS)\" $(SELECT) (not (tag :interactive)))))" || exit 1; \

clean :
	@rm -f $(AUTOLOADS_FILE) 
	@rm -f *.elc 
	@rm -f *~ 
	@rm -f \#*\#

clean-global :
	@rm -f ergoemacs-global-*.el 
	@rm -f ergoemacs-global-*.elc

