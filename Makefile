.POSIX:
EMACS = emacs

compile: lcg128.elc

check: lcg128-tests.elc
	$(EMACS) --batch -Q -L . -l lcg128-tests.elc -f ert-run-tests-batch

bench: lcg128-tests.elc
	$(EMACS) --batch -Q -L . -l lcg128-tests.elc -f lcg128--benchmark

test: check

clean:
	rm -f lcg128.elc lcg128-tests.elc

lcg128-tests.elc: lcg128-tests.el lcg128.elc

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) --batch -Q -L . -f batch-byte-compile $<
