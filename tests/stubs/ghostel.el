;;; ghostel.el --- Test stub for the ghostel API -*- lexical-binding: t; -*-

;;; Commentary:
;; Hermetic stand-in for the real ghostel package (which needs a native
;; module).  The pi-mode tests mock the send surface with cl-letf anyway;
;; this stub only satisfies `require' and defines the variables and
;; functions pi-mode references at load/compile time.

;;; Code:

(defgroup ghostel nil "Ghostel test stub." :group 'tools)

(defcustom ghostel-keymap-exceptions
  '("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\\")
  "Key sequences not sent to the terminal (stub)."
  :type '(repeat string))

(defvar ghostel-kill-buffer-on-exit t)
(defvar ghostel-buffer-name-function nil)
(defvar ghostel-exit-functions nil)

(defun ghostel-exec (_buffer _program &optional _args)
  (error "ghostel stub: not implemented"))
(defun ghostel-send-string (_string)
  (error "ghostel stub: not implemented"))
(defun ghostel-send-key (_key-name &optional _mods)
  (error "ghostel stub: not implemented"))
(defun ghostel-paste-string (_string)
  (error "ghostel stub: not implemented"))
(defun ghostel-send-C-c ()
  (error "ghostel stub: not implemented"))
(defvar ghostel--input-mode 'semi-char)
(defvar ghostel--cursor-pos nil)
(defun ghostel--viewport-start ()
  nil)
(defun ghostel-cursor-point ()
  nil)
(defun ghostel--viewport-row-at (_pos)
  nil)
(defun ghostel--rebuild-semi-char-keymap ()
  nil)

(provide 'ghostel)
;;; ghostel.el ends here
