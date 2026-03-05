EMACS ?= emacs

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch \
	  --eval '(package-initialize)' \
	  -l mullvad-test.el \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch \
	  --eval '(package-initialize)' \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile mullvad.el

clean:
	rm -f *.elc
