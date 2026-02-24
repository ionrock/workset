EMACS ?= emacs
EASK ?= eask

HAS_EASK := $(shell command -v $(EASK) >/dev/null 2>&1 && echo yes || echo no)

.PHONY: ci compile test checkdoc lint clean run

ci: clean compile checkdoc lint test

compile:
ifeq ($(HAS_EASK),yes)
	$(EASK) compile
else
	$(EMACS) --batch -L . -f batch-byte-compile workset.el workset-project.el workset-worktree.el workset-vterm.el
endif

test:
ifeq ($(HAS_EASK),yes)
	$(EASK) test ert test/workset-test.el
else
	$(EMACS) --batch -L . -l test/workset-test.el -f ert-run-tests-batch-and-exit
endif

checkdoc:
ifeq ($(HAS_EASK),yes)
	$(EASK) lint checkdoc
else
	$(EMACS) --batch -L . -f checkdoc-file workset.el
	$(EMACS) --batch -L . -f checkdoc-file workset-project.el
	$(EMACS) --batch -L . -f checkdoc-file workset-worktree.el
	$(EMACS) --batch -L . -f checkdoc-file workset-vterm.el
endif

lint:
ifeq ($(HAS_EASK),yes)
	$(EASK) lint package
else
	@echo "lint requires eask (install from https://github.com/emacs-eask/cli)"
	@exit 1
endif

clean:
ifeq ($(HAS_EASK),yes)
	$(EASK) clean all
else
	@rm -f *.elc
	@rm -f *-autoloads.el *-pkg.el
	@rm -rf .eask dist
endif

run:
	$(EMACS) --batch -Q --eval "(progn (require 'package) (setq package-user-dir (expand-file-name \".workset-elpa\" default-directory)) (add-to-list 'package-archives '(\"gnu\" . \"https://elpa.gnu.org/packages/\") t) (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) (package-initialize) (unless (package-installed-p 'vterm) (package-refresh-contents) (package-install 'vterm)))"
	$(EMACS) -Q -L . --eval "(progn (require 'package) (setq package-user-dir (expand-file-name \".workset-elpa\" default-directory)) (add-to-list 'package-archives '(\"gnu\" . \"https://elpa.gnu.org/packages/\") t) (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) (package-initialize))" -l workset.el
