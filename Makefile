EMACS ?= emacs
# package-initialize loads installed packages (transient); tests/stubs
# provides a hermetic ghostel stub (real ghostel needs a native module
# and is not assumed present).  Order matters: stubs must win.
.PHONY: test lint

test:
	$(EMACS) -Q --batch -l package --eval '(package-initialize)' -L tests/stubs -L . -L tests -l tests/pi-mode-tests.el -f ert-run-tests-batch-and-exit

lint:
	$(EMACS) -Q --batch -L . -l package-lint -f package-lint-batch-and-exit pi-mode.el pi-mode-menu.el pi-mode-session.el pi-mode-keys.el
