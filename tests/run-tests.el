;;; run-tests.el --- Selector-aware ERT runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Load the unit tests relative to this file, then run either the complete
;; suite or the selector supplied through ERT_SELECTOR.

;;; Code:

(require 'ert)

(load (expand-file-name
       "pi-mode-tests.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil t)

(let ((selector (getenv "ERT_SELECTOR")))
  (ert-run-tests-batch-and-exit
   (if (or (null selector) (string= selector ""))
       t
     (read selector))))

;;; run-tests.el ends here
