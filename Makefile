EMACS ?= emacs
EASK ?= eask

.PHONY: ci compile test checkdoc lint clean

ci: clean compile checkdoc lint test

compile:
	$(EASK) compile

test:
	$(EASK) test ert test/workset-test.el

checkdoc:
	$(EASK) lint checkdoc

lint:
	$(EASK) lint package

clean:
	$(EASK) clean all
