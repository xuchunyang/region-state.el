EMACS ?= emacs

compile:
	${EMACS} -Q --batch -L . -f batch-byte-compile region-state.el

test:
	${EMACS} -Q --batch -L . -l region-state-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
