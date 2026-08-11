;;; pi-mode-session.el --- Session commands for pi-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Session management for pi-mode: continue, resume, fork, rename, stop,
;; list, and the session-directory seam.

;;; Code:

(require 'cl-lib)
(require 'pi-mode)

(defcustom pi-mode-session-dir-function #'pi-mode--session-dir
  "Function returning the pi session directory for a project root.
The test seam for session fixtures."
  :type 'function
  :group 'pi-mode)

(defun pi-mode--session-dir (root)
  "Return pi's session directory for project ROOT.

Matches pi's layout: `sessions/--<path>--/' where the absolute path has
`/' replaced by `-' (the leading slash of an absolute root contributes
no dash, e.g. `/Users/me/proj' -> `--Users-me-proj--')."
  (let ((name (string-replace "/" "-" (directory-file-name root))))
    (when (string-prefix-p "-" name)
      (setq name (substring name 1)))
    (expand-file-name
     (format "--%s--" name)
     (expand-file-name "sessions"
                       (or (getenv "PI_CODING_AGENT_DIR") "~/.pi/agent")))))

(defun pi-mode--session-files (root)
  "Return pi session .jsonl files for project ROOT."
  (let ((dir (funcall pi-mode-session-dir-function root)))
    (when (file-directory-p dir)
      (directory-files dir t "\\.jsonl\\'"))))

(defun pi-mode--session-command-args (flag)
  "Return pi launch args for a session FLAG (e.g. \"-c\")."
  (append pi-mode-cli-args (list flag)))

;;;###autoload
(defun pi-mode-session-continue ()
  "Start a new pi session continuing the most recent one."
  (interactive)
  (pi-mode--launch-buffer (pi-mode--project-root)
                          (pi-mode--session-command-args "-c")))

;;;###autoload
(defun pi-mode-session-resume ()
  "Start pi with the interactive resume picker."
  (interactive)
  (pi-mode--launch-buffer (pi-mode--project-root)
                          (pi-mode--session-command-args "-r")))

;;;###autoload
(defun pi-mode-session-fork ()
  "Fork a past session into a new pi session."
  (interactive)
  (let* ((root (pi-mode--project-root))
         (files (pi-mode--session-files root)))
    (unless files
      (user-error "No past sessions found for %s" root))
    (let ((file (completing-read "Fork session: " files nil t)))
      (pi-mode--launch-buffer root (append pi-mode-cli-args (list "--fork" file))))))

;;;###autoload
(defun pi-mode-session-rename (name)
  "Rename the target session to NAME (sends /name to pi)."
  (interactive
   (list (read-from-minibuffer "Session name: ")))
  (let ((session (pi-mode--resolve-session current-prefix-arg)))
    (when (and name (> (length name) 0))
      (with-current-buffer (pi-mode-session-buffer session)
        (ghostel-send-string (format "/name %s" name))
        (ghostel-send-key "return"))
      (setf (pi-mode-session-name session) name)
      (pi-mode-log "renamed session to %s" name))))

;;;###autoload
(defun pi-mode-session-stop ()
  "Stop the target pi session."
  (interactive)
  (let ((session (pi-mode--resolve-session current-prefix-arg)))
    (when (y-or-n-p (format "Stop pi session %s? " (pi-mode-session-id session)))
      (setf (pi-mode-session-exit-requested session) t)
      (delete-process (pi-mode-session-process session))
      (pi-mode-log "stopped session %s" (pi-mode-session-id session)))))

;;;###autoload
(defun pi-mode-session-stop-all ()
  "Stop all pi sessions in the current project."
  (interactive)
  (let* ((root (pi-mode--project-root))
         (sessions (cl-remove-if-not
                    (lambda (s) (equal (pi-mode-session-project-root s) root))
                    (pi-mode--active-sessions))))
    (when (and sessions (y-or-n-p (format "Stop %d pi session(s) in %s? "
                                          (length sessions) root)))
      (dolist (s sessions)
        (setf (pi-mode-session-exit-requested s) t)
        (delete-process (pi-mode-session-process s)))
      (pi-mode-log "stopped %d sessions" (length sessions)))))

;;;###autoload
(defun pi-mode-list-sessions ()
  "List live pi sessions and switch to the chosen one."
  (interactive)
  (let ((sessions (pi-mode--active-sessions)))
    (unless sessions
      (user-error "No running pi sessions"))
    (let ((session (pi-mode--prompt-session sessions)))
      (switch-to-buffer (pi-mode-session-buffer session)))))

;;;###autoload
(defun pi-mode-switch-buffer ()
  "Switch to the buffer of the target pi session."
  (interactive)
  (let ((session (pi-mode--resolve-session current-prefix-arg)))
    (switch-to-buffer (pi-mode-session-buffer session))))

(provide 'pi-mode-session)
;;; pi-mode-session.el ends here
