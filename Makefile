EMACS ?= emacs
ERT_SELECTOR ?= t
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
.PHONY: test coverage compile-tests e2e check lint

test:
	ERT_SELECTOR='$(ERT_SELECTOR)' $(EMACS) -Q --batch -l package --eval '(package-initialize)' --eval '(setq load-prefer-newer t)' -L tests/stubs -L . -L tests -l tests/run-tests.el

coverage:
	$(EMACS) -Q --batch -l package \
		--eval '(package-initialize)' \
		-L tests/stubs -L . -L tests \
		-l tests/testcover-run.el

compile-tests:
	$(EMACS) -Q --batch -l package \
		--eval '(package-initialize)' \
		--eval '(setq load-prefer-newer t)' \
		--eval '(setq byte-compile-error-on-warn t)' \
		-L tests/stubs -L . -L tests -L tests/e2e \
		-f batch-byte-compile \
		tests/pi-mode-tests.el \
		tests/e2e/pi-mode-e2e-server.el \
		tests/e2e/pi-mode-e2e.el

e2e:
	$(EMACS) -Q --batch -l package --eval '(package-initialize)' --eval '(setq load-prefer-newer t)' -L . -L tests/e2e -l tests/e2e/pi-mode-e2e.el -f ert-run-tests-batch-and-exit

check: test e2e

lint:
	$(EMACS) -Q --batch -l package \
		--eval '(package-initialize)' \
		--eval '(add-to-list (quote package-archives) (cons "melpa" "https://melpa.org/packages/") t)' \
		--eval '(package-refresh-contents)' \
		-L . -l package-lint -f package-lint-batch-and-exit pi-mode.el
