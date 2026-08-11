;;; pi-mode.el --- Emacs interface for the pi coding agent -*- lexical-binding: t; -*-

;; Author: Jay Xu
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.49") (transient "0.7"))
;; Keywords: tools, processes
;; URL: https://github.com/jayxu/pi-mode.el
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Run the pi coding agent inside a ghostel terminal buffer with project
;; detection, embed-format region sending, session management, and a
;; transient command menu.  Design spec:
;; docs/superpowers/specs/2026-08-11-pi-mode-design.md

;;; Code:

(require 'cl-lib)
(require 'project)          ; project-root is not autoloaded
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
  :group 'pi-mode
  :lighter " Pi"
  :keymap pi-mode-map)

(defun pi-mode-log (format-string &rest args)
  "Log a message to the *pi-mode-debug* buffer using FORMAT-STRING and ARGS."
  (when pi-mode-debug
    (with-current-buffer (get-buffer-create "*pi-mode-debug*")
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format-time-string "%H:%M:%S ") (apply #'format format-string args) "\n")))))

;;; Project detection

(defcustom pi-mode-project-root-function nil
  "Function returning the project root for the current context.
Called with no arguments; must return a directory string.
When nil, `project-current' is used with `default-directory' as fallback."
  :type '(choice (const :tag "Use project.el" nil) function)
  :group 'pi-mode)

(defun pi-mode--project-root ()
  "Return the project root for the current context."
  (let ((root (if pi-mode-project-root-function
                  (funcall pi-mode-project-root-function)
                (when-let ((proj (project-current)))
                  (project-root proj)))))
    (or root default-directory)))

;;; Sessions

(cl-defstruct pi-mode-session
  "A running pi agent session."
  id name buffer process project-root last-used window-slot
  cleanup-done exit-requested)

(defcustom pi-mode-cli-args '("--tui-mode" "regular")
  "Extra command-line arguments passed to pi at launch."
  :type '(repeat string)
  :group 'pi-mode)

(defcustom pi-mode-kill-buffer-on-exit nil
  "When non-nil, kill the pi buffer when the process exits."
  :type 'boolean
  :group 'pi-mode)

(defcustom pi-mode-confirm-kill t
  "When non-nil, confirm before killing a running pi session."
  :type 'boolean
  :group 'pi-mode)

(defvar pi-mode--sessions (make-hash-table :test #'equal)
  "Hash table of live pi sessions keyed by session id (buffer name).")

(defvar-local pi-mode--session nil
  "The `pi-mode-session' struct for this buffer, or nil.")

(defun pi-mode--register-session (session)
  (puthash (pi-mode-session-id session) session pi-mode--sessions))

(defun pi-mode--unregister-session (id)
  (remhash id pi-mode--sessions))

(defun pi-mode--active-sessions ()
  "Return live sessions sorted by last-used, most recent first."
  (let ((sessions (cl-loop for s being the hash-values of pi-mode--sessions collect s)))
    (setq sessions
          (cl-remove-if-not
           (lambda (s)
             (and (buffer-live-p (pi-mode-session-buffer s))
                  (process-live-p (pi-mode-session-process s))))
           sessions))
    (sort sessions (lambda (a b)
                     (time-less-p (pi-mode-session-last-used b)
                                  (pi-mode-session-last-used a))))))

(defun pi-mode--session-by-buffer (buffer)
  (when buffer
    (gethash (buffer-name buffer) pi-mode--sessions)))

(defun pi-mode--session-by-process (process)
  (cl-loop for s being the hash-values of pi-mode--sessions
           when (eq (pi-mode-session-process s) process) return s))

(defun pi-mode--mru-session (sessions)
  (car sessions))

(defun pi-mode--visible-sessions (sessions)
  (cl-remove-if-not (lambda (s) (get-buffer-window (pi-mode-session-buffer s))) sessions))

;;; Hooks
;; Defined before `pi-mode--cleanup-session', which runs them.

(defvar pi-mode-before-send-hook nil
  "Hook run with (SESSION TEXT) before text is sent to pi.")

(defvar pi-mode-after-start-hook nil
  "Hook run with the session struct after a pi session starts.")

(defvar pi-mode-on-exit-hook nil
  "Hook run with (BUFFER EVENT) when a pi session exits.")

;;; Lifecycle

(defun pi-mode--unique-buffer-name (base)
  "Return BASE, or BASE<N> for the first N that is not in use."
  (let ((name base) (n 2))
    (while (get-buffer name)
      (setq name (format "%s<%d>" base n)
            n (1+ n)))
    name))

(defun pi-mode--ghostel-launch (buffer project-root args)
  "Launch pi in BUFFER for PROJECT-ROOT with ARGS.
Returns the lifecycle process.  Sets ghostel buffer options that
`ghostel-exec' resets."
  (let* ((default-directory project-root)
         (pi (executable-find "pi")))
    (unless pi
      (user-error "pi executable not found in exec-path; install pi first"))
    (with-current-buffer buffer
      (setq-local ghostel-kill-buffer-on-exit nil)
      (prog1 (ghostel-exec buffer pi args)
        (setq-local ghostel-kill-buffer-on-exit nil)
        (setq-local ghostel-buffer-name-function nil)))))

(defun pi-mode--attach-sentinel (process)
  "Chain pi-mode cleanup onto PROCESS's sentinel.
Runs the stashed ghostel sentinel first, then `pi-mode--cleanup-session'
on exit events."
  (let ((orig (process-sentinel process)))
    (process-put process 'pi-mode--ghostel-sentinel orig)
    (set-process-sentinel
     process
     (lambda (proc event)
       (when (functionp (process-get proc 'pi-mode--ghostel-sentinel))
         (funcall (process-get proc 'pi-mode--ghostel-sentinel) proc event))
       (when (string-match-p "finished\\|exited\\|killed\\|terminated" event)
         (pi-mode--cleanup-session proc))))))

(defun pi-mode--cleanup-session (process)
  "Run session cleanup for PROCESS (idempotent)."
  (when-let ((session (pi-mode--session-by-process process)))
    (unless (pi-mode-session-cleanup-done session)
      (setf (pi-mode-session-cleanup-done session) t)
      (let ((buffer (pi-mode-session-buffer session)))
        (pi-mode--unregister-session (pi-mode-session-id session))
        (run-hook-with-args 'pi-mode-on-exit-hook buffer "exited")
        (when pi-mode-kill-buffer-on-exit
          (kill-buffer buffer))))))

(defun pi-mode--make-session (project-root &optional name)
  "Create and register a session struct for PROJECT-ROOT in a new buffer."
  (let* ((base (format "*pi[%s]*"
                       (file-name-nondirectory (directory-file-name project-root))))
         (buffer-name (pi-mode--unique-buffer-name
                       (if name (format "%s:%s" base name) base))))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((session (make-pi-mode-session
                      :id buffer-name :name name :buffer (current-buffer)
                      :project-root project-root :last-used (current-time))))
        (setq-local pi-mode--session session)
        (pi-mode +1)
        (pi-mode--register-session session)
        session))))

(defun pi-mode--launch-buffer (project-root args &optional name)
  "Create a session buffer, launch pi in it, and display it."
  (let* ((session (pi-mode--make-session project-root name))
         (buffer (pi-mode-session-buffer session))
         (process (pi-mode--ghostel-launch buffer project-root args)))
    (setf (pi-mode-session-process session) process)
    (pi-mode--attach-sentinel process)
    (pop-to-buffer buffer)
    (run-hook-with-args 'pi-mode-after-start-hook session)
    session))

;;;###autoload
(defun pi-mode-start ()
  "Start a pi session in the current project."
  (interactive)
  (pi-mode--launch-buffer (pi-mode--project-root) pi-mode-cli-args))

;;;###autoload
(defalias 'pi-mode 'pi-mode-start)

;;; Target resolution

(defun pi-mode--prompt-session (sessions)
  "Prompt for one of SESSIONS and return it."
  (let ((id (completing-read "pi session: "
                             (mapcar #'pi-mode-session-id sessions)
                             nil t)))
    (gethash id pi-mode--sessions)))

(defun pi-mode--resolve-session (&optional prefix no-ask)
  "Resolve the target session for a command.
PREFIX non-nil means the user gave C-u: prompt unless NO-ASK.
Rules: in-buffer self; sole; sole-visible; else MRU with echo."
  (let ((sessions (pi-mode--active-sessions)))
    (cond
     ((null sessions)
      (user-error "No running pi session; start one with `pi-mode'"))
     ((and prefix (not no-ask))
      (pi-mode--prompt-session sessions))
     ((pi-mode--session-by-buffer (current-buffer)))
     ((= (length sessions) 1)
      (car sessions))
     ((= (length (pi-mode--visible-sessions sessions)) 1)
      (car (pi-mode--visible-sessions sessions)))
     (t
      (let ((mru (pi-mode--mru-session sessions)))
        (message "pi-mode: using session %s" (pi-mode-session-id mru))
        mru)))))

;;; Kill-buffer guard

(defun pi-mode--kill-buffer-guard ()
  "Confirm before killing a live pi session buffer."
  (when (and pi-mode--session
             (process-live-p (pi-mode-session-process pi-mode--session))
             pi-mode-confirm-kill
             (not (pi-mode-session-exit-requested pi-mode--session)))
    (unless (y-or-n-p "Kill running pi session? ")
      (error "Aborted"))
    (setf (pi-mode-session-exit-requested pi-mode--session) t)
    (delete-process (pi-mode-session-process pi-mode--session))))

(add-hook 'kill-buffer-hook #'pi-mode--kill-buffer-guard)

(provide 'pi-mode)
;;; pi-mode.el ends here
