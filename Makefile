EMACS ?= emacs
# package-initialize loads installed packages (transient); tests/stubs
# provides a hermetic ghostel stub (real ghostel needs a native module
# and is not assumed present).  Order matters: stubs must win.
# package-lint treats each file as its own package; for multi-file
# packages the conventional gate lints the MAIN file only (sub-file
# prefix/dependency noise is not meaningful).
#
# `test' runs the fast hermetic unit suite (ghostel stubbed).
# `e2e' runs the full-stack suite against REAL ghostel (from elpa, with
# its native module) and the REAL pi binary, with pi pointed at the
# in-Emacs OpenAI-compatible e2e model server; never mix in
# tests/stubs.  `check' runs both.
.PHONY: test e2e check lint

test:
	$(EMACS) -Q --batch -l package --eval '(package-initialize)' --eval '(setq load-prefer-newer t)' -L tests/stubs -L . -L tests -l tests/pi-mode-tests.el -f ert-run-tests-batch-and-exit

e2e:
	$(EMACS) -Q --batch -l package --eval '(package-initialize)' --eval '(setq load-prefer-newer t)' -L . -L tests/e2e -l tests/e2e/pi-mode-e2e.el -f ert-run-tests-batch-and-exit

check: test e2e

lint:
	$(EMACS) -Q --batch -l package --eval '(package-initialize)' -L . -l package-lint -f package-lint-batch-and-exit pi-mode.el
