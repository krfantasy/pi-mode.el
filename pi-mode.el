;;; pi-mode.el --- Emacs interface for the pi coding agent -*- lexical-binding: t; -*-

;; Author: Jay Xu
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.49") (transient "0.7"))
;; Keywords: tools, processes
;; URL: https://github.com/jayxu/pi-mode.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Run the pi coding agent inside a ghostel terminal buffer with project
;; detection, embed-format region sending, session management, and a
;; transient command menu.  Design spec:
;; docs/superpowers/specs/2026-08-11-pi-mode-design.md

;;; Code:

(require 'cl-lib)
(require 'ghostel)
(require 'transient)

(defgroup pi-mode nil
  "Interface for the pi coding agent."
  :group 'tools
  :prefix "pi-mode-")

(defcustom pi-mode-debug t
  "When non-nil, log pi-mode activity to the *pi-mode-debug* buffer."
  :type 'boolean
  :group 'pi-mode)

(defvar pi-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `pi-mode' buffers.")

(define-minor-mode pi-mode
  "Minor mode for buffers running the pi coding agent.

\\{pi-mode-map}"
  :lighter " Pi"
  :keymap pi-mode-map)

(defun pi-mode-log (format-string &rest args)
  "Log a message to the *pi-mode-debug* buffer using FORMAT-STRING and ARGS."
  (when pi-mode-debug
    (with-current-buffer (get-buffer-create "*pi-mode-debug*")
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format-time-string "%H:%M:%S ") (apply #'format format-string args) "\n")))))

(provide 'pi-mode)
;;; pi-mode.el ends here
