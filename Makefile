EMACS ?= emacs
# package-initialize loads installed packages (transient); tests/stubs
# provides a hermetic ghostel stub (real ghostel needs a native module
# and is not assumed present).  Order matters: stubs must win.
# package-lint treats each file as its own package; for multi-file
# packages the conventional gate lints the MAIN file only (sub-file
# prefix/dependency noise is not meaningful).
.PHONY: test lint

test:
	$(EMACS) -Q --batch -l package --eval '(package-initialize)' --eval '(setq load-prefer-newer t)' -L tests/stubs -L . -L tests -l tests/pi-mode-tests.el -f ert-run-tests-batch-and-exit

lint:
	$(EMACS) -Q --batch -l package --eval '(package-initialize)' -L . -l package-lint -f package-lint-batch-and-exit pi-mode.el
