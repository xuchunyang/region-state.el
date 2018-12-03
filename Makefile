EMACS ?= emacs

all: check

check: compile test

compile:
	${EMACS} -Q --batch -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile region-state.el

test:
	${EMACS} -Q --batch -L . -l region-state-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc

local:
	@for cmd in emacs-24.4 emacs-24.5 emacs-25.1 emacs-25.3 emacs-26.1; do \
	    command -v $$cmd && make EMACS=$$cmd ;\
	done
