;;; pi-mode-status.el --- Status and version commands for pi-mode -*- lexical-binding: t; -*-

;; Author: Jay Xu
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.49") (transient "0.7"))
;; Keywords: tools, processes
;; URL: https://github.com/jayxu/pi-mode.el
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:
;; CLI status check, version info buffer, and the live session header
;; for the pi-mode transient menu.

;;; Code:

(require 'cl-lib)
(require 'pi-mode)

(defvar pi-mode--cli-cache nil
  "Cached (PATH . VERSION) for the pi CLI, or nil when unknown.")

(defun pi-mode--cli-info ()
  "Return (PATH . VERSION) for the pi CLI, or nil when not found.
VERSION is nil when `pi --version' fails."
  (or pi-mode--cli-cache
      (let* ((path (executable-find "pi"))
             (version (and path
                           (with-temp-buffer
                             (call-process path nil t nil "--version")
                             (string-trim (buffer-string))))))
        (when path
          (setq pi-mode--cli-cache (cons path version)))
        (and path (cons path version)))))

(defun pi-mode--cli-status ()
  "One-line status string for the pi CLI."
  (if-let* ((info (pi-mode--cli-info)))
      (format "pi %s found at %s" (or (cdr info) "?") (car info))
    "pi CLI not found in exec-path"))

;;;###autoload
(defun pi-mode-check-status ()
  "Check whether the pi CLI is installed and report its version."
  (interactive)
  (message "%s" (pi-mode--cli-status)))

;;;###autoload
(defun pi-mode-show-version-info ()
  "Show pi-mode, Emacs and pi CLI version information."
  (interactive)
  (with-current-buffer (get-buffer-create "*pi-mode-status*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "pi-mode %s\n"
                      (or (bound-and-true-p pi-mode-version) "unknown")))
      (insert (format "Emacs %s\n" emacs-version))
      (insert (format "%s\n" (pi-mode--cli-status)))
      (insert "\nSessions:\n")
      (let ((sessions (pi-mode--active-sessions)))
        (if sessions
            (dolist (s sessions)
              (insert (format "  %-24s %-16s %s\n"
                              (pi-mode-session-id s)
                              (file-name-nondirectory
                               (directory-file-name (pi-mode-session-project-root s)))
                              (if (pi-mode--visible-sessions (list s))
                                  "visible" "hidden"))))
          (insert "  (none)\n"))))
    (special-mode)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun pi-mode--session-status ()
  "Menu-header status string for the current project's sessions."
  (let* ((project-dir (pi-mode--project-root))
         (all (pi-mode--active-sessions))
         (sessions (cl-remove-if-not
                    (lambda (s) (equal (pi-mode-session-project-root s) project-dir))
                    all)))
    (cond
     (sessions
      (let* ((project-name (file-name-nondirectory (directory-file-name project-dir)))
             (shown (cl-subseq sessions 0 (min 4 (length sessions))))
             (header (propertize (format "%s — %d session%s (%d visible)"
                                         project-name (length sessions)
                                         (if (cdr sessions) "s" "")
                                         (length (pi-mode--visible-sessions sessions)))
                                 'face 'success))
             (lines (mapcar
                     (lambda (s)
                       (format "  %-20s %s"
                               (or (pi-mode-session-name s) "default")
                               (if (pi-mode--visible-sessions (list s))
                                   "visible" "hidden")))
                     shown)))
        (when (> (length sessions) (length shown))
          (setq lines (append lines
                              (list (propertize
                                     (format "  …and %d more" (- (length sessions) (length shown)))
                                     'face 'transient-inactive-value)))))
        (mapconcat #'identity (cons header lines) "\n")))
     (all
      (propertize (format "No session in this project (%d running elsewhere)" (length all))
                  'face 'transient-inactive-value))
     (t
      (propertize "No active sessions" 'face 'transient-inactive-value)))))

(provide 'pi-mode-status)

;;; pi-mode-status.el ends here
