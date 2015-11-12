EMACS ?= emacs

compile:
	${EMACS} -Q --batch -L . -f batch-byte-compile region-state.el

test:
	${EMACS} -Q --batch -L . -f ert-run-tests-batch-and-exit -l region-state-test.el

clean:
	rm -f *.elc
