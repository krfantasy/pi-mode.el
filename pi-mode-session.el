;;; pi-mode-session.el --- Session commands for pi-mode -*- lexical-binding: t; -*-

;; Author: Jay Xu
;; Version: 0.1.0
;; Keywords: tools, processes
;; URL: https://github.com/krfantasy/pi-mode.el
;; License: The License

;;; Commentary:
;; Session management for pi-mode: continue, resume, fork, rename, stop,
;; list, and the session-directory seam.

;;; Code:

(require 'pi-mode)

(defun pi-mode--session-dir (root)
  "Return pi's session directory for project ROOT.

Matches pi's layout: `sessions/--<path>--/' where the absolute path has
`/' replaced by `-' (the leading slash of an absolute root contributes
no dash, e.g. `/Users/me/proj' -> `--Users-me-proj--').  The root is
resolved with `file-truename' first, because pi names the directory
after its resolved cwd (on macOS `/var/...' is `/private/var/...')."
  (let ((name (string-replace "/" "-" (file-truename (directory-file-name root)))))
    (when (string-prefix-p "-" name)
      (setq name (substring name 1)))
    (expand-file-name
     (format "--%s--" name)
     (expand-file-name "sessions"
                       (or (getenv "PI_CODING_AGENT_DIR") "~/.pi/agent")))))

(defun pi-mode--session-files (root)
  "Return pi session .jsonl files for project ROOT."
  (let ((dir (pi-mode--session-dir root)))
    (when (file-directory-p dir)
      (directory-files dir t "\\.jsonl\\'"))))

;;;###autoload
(defun pi-mode-session-continue ()
  "Start a new pi session continuing the most recent one.
Prompts for an instance name when the project already has running
sessions, or with a prefix argument."
  (interactive)
  (let ((root (pi-mode--project-root)))
    (pi-mode--launch-buffer root (append pi-mode-cli-args (list "-c"))
                            (pi-mode--maybe-read-instance-name root))))

;;;###autoload
(defun pi-mode-session-resume ()
  "Start pi with the interactive resume picker.
Prompts for an instance name when the project already has running
sessions, or with a prefix argument."
  (interactive)
  (let ((root (pi-mode--project-root)))
    (pi-mode--launch-buffer root (append pi-mode-cli-args (list "-r"))
                            (pi-mode--maybe-read-instance-name root))))

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
(defun pi-mode-session-rename ()
  "Rename the target session, validating the name and renaming the buffer.
The new name is read with `pi-mode--read-instance-name': pure-numeric
names and names containing `[', `]', `*', or control characters are
rejected, as are names already used by another live session of the
project; the old name prefills the prompt and empty input auto-names
(the session becomes unnamed).  Sends /name to pi and renames the
terminal buffer so the buffer name and the display name stay in sync."
  (interactive)
  (let* ((session (pi-mode--resolve-session current-prefix-arg nil 'prompt))
         (root (pi-mode-session-project-root session))
         (old-name (pi-mode-session-name session))
         (new-name (pi-mode--read-instance-name
                    root
                    (format "Rename %s to (empty for auto): "
                            (or old-name (pi-mode-session-id session)))
                    session)))
    (when new-name
      (with-current-buffer (pi-mode-session-buffer session)
        (ghostel-send-string (format "/name %s" new-name))
        (ghostel-send-key "return")))
    (setf (pi-mode-session-name session) new-name)
    ;; Rename the terminal buffer and re-key the registry, preserving the
    ;; id = buffer-name invariant `pi-mode--session-by-buffer' relies on.
    (let ((new-buffer-name
           (generate-new-buffer-name
            (pi-mode--session-base-name root new-name)
            (buffer-name (pi-mode-session-buffer session)))))
      (pi-mode--unregister-session (pi-mode-session-id session))
      (setf (pi-mode-session-id session) new-buffer-name)
      (with-current-buffer (pi-mode-session-buffer session)
        (rename-buffer new-buffer-name))
      (pi-mode--register-session session))
    (pi-mode-log "renamed session to %s" (or new-name "auto"))))

;;;###autoload
(defun pi-mode-session-stop ()
  "Stop the target pi session.
Stopping is destructive, so the target is never guessed: the current
session buffer's session, the sole session of the project, or the sole
visible one is stopped directly; otherwise a completing-read picks the
session.  When `pi-mode-confirm-quit' is non-nil a y-or-n prompt asks
before stopping; declining leaves the session untouched (the
resolution already asks when the target is ambiguous, cc-ide parity).
The process is deleted; the sentinel cleanup then removes the buffer
per `pi-mode-kill-buffer-on-exit'."
  (interactive)
  (let ((session (pi-mode--resolve-session current-prefix-arg nil 'prompt)))
    (if (and pi-mode-confirm-quit
             (not (y-or-n-p
                   (format "Stop pi session %s? "
                           (pi-mode-session-id session)))))
        (pi-mode-log "stop declined for %s" (pi-mode-session-id session))
      (setf (pi-mode-session-exit-requested session) t)
      (delete-process (pi-mode-session-process session))
      (pi-mode-log "stopped session %s" (pi-mode-session-id session)))))

;;;###autoload
(defun pi-mode-session-stop-all (&optional all-projects)
  "Stop every pi session in the current project.
With prefix argument ALL-PROJECTS, stop the sessions of all projects."
  (interactive "P")
  (let* ((root (pi-mode--project-root))
         (sessions (if all-projects
                       (pi-mode--active-sessions)
                     (pi-mode--project-sessions root))))
    (if (null sessions)
        (pi-mode-log "No pi sessions to stop")
      (when (y-or-n-p (if all-projects
                          (format "Stop all %d pi session%s? "
                                  (length sessions)
                                  (if (cdr sessions) "s" ""))
                        (format "Stop %d pi session(s) in %s? "
                                (length sessions) root)))
        ;; One session's teardown error must not strand the rest
        (dolist (s sessions)
          (condition-case err
              (progn
                (setf (pi-mode-session-exit-requested s) t)
                (delete-process (pi-mode-session-process s)))
            (error
             (pi-mode-log "Error stopping %s: %s"
                          (pi-mode-session-id s)
                          (error-message-string err)))))
        (pi-mode-log "Stopped %d pi session%s"
                     (length sessions)
                     (if (cdr sessions) "s" ""))))))

;;;###autoload
(defun pi-mode--switch-to-session (session)
  "Switch to SESSION, replacing the current panel instead of splitting.
When SESSION is already displayed in the selected frame, its window
is selected.  Otherwise the session buffer is shown in the selected
window: a plain window is replaced in place, and a pi side panel
(dedicated to another session) is taken over — its dedication stays
and its side-slot parameter is re-keyed to SESSION, so later
displays of SESSION reuse this window instead of creating a
duplicate.  A window dedicated to an unrelated buffer falls back to
`display-buffer' (the normal pi side-window path).  Also stamps
SESSION as most recently used."
  (let* ((buffer (pi-mode-session-buffer session))
         (window (selected-window))
         (existing (get-buffer-window buffer 0)))
    (cond
     (existing
      (select-window existing))
     ((not (window-dedicated-p window))
      (set-window-buffer window buffer))
     ((pi-mode--session-by-buffer (window-buffer window))
      ;; Re-key the panel to the target session in place.  A plain
      ;; `switch-to-buffer' falls back to `display-buffer' inside a
      ;; dedicated window, which opens a NEW side window for the
      ;; target (display-buffer-alist routing), splitting the layout.
      (set-window-dedicated-p window nil)
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t)
      (set-window-parameter window 'window-slot
                            (or (pi-mode-session-window-slot session) 0)))
     (t
      (display-buffer buffer)))
    ;; MRU parity with `pi-mode--display-buffer': switching makes the
    ;; target the most-recently-used session.
    (setf (pi-mode-session-last-used session) (current-time))))

;;;###autoload
(defun pi-mode-list-sessions ()
  "List live pi sessions and switch to the chosen one.
The chosen session replaces the current panel instead of splitting
off a new window (`pi-mode--switch-to-session')."
  (interactive)
  (let ((sessions (pi-mode--active-sessions)))
    (unless sessions
      (user-error "No running pi sessions"))
    (let ((session (pi-mode--prompt-session sessions)))
      (pi-mode--switch-to-session session))))

;;;###autoload
(defun pi-mode-switch-buffer ()
  "Switch to the buffer of the target pi session.
The current panel is replaced with the session buffer instead of
splitting off a new side window (`pi-mode--switch-to-session')."
  (interactive)
  (let ((session (pi-mode--resolve-session current-prefix-arg)))
    (pi-mode--switch-to-session session)))

(provide 'pi-mode-session)
;;; pi-mode-session.el ends here
