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
(require 'compile)          ; compilation--message->loc etc. (batch needs it)
(require 'pi-mode-keys)     ; standalone installer; does not require pi-mode

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
       (when (string-match-p "finished\\|exited\\|killed\\|terminated\\|deleted\\|closed" event)
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
  "Create an unregistered session struct for PROJECT-ROOT in a new buffer.
Registration happens in `pi-mode--launch-buffer' after a successful
launch, so a failed launch leaves nothing behind."
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
        session))))

(defun pi-mode--launch-buffer (project-root args &optional name)
  "Create a session buffer, launch pi in it, and display it.
The session is registered only after a successful launch; when the
launch fails the scratch buffer is removed."
  (let ((session (pi-mode--make-session project-root name)))
    (unwind-protect
        (let* ((buffer (pi-mode-session-buffer session))
               (process (pi-mode--ghostel-launch buffer project-root args)))
          (setf (pi-mode-session-process session) process)
          (pi-mode--register-session session)
          (pi-mode--attach-sentinel process)
          (pop-to-buffer buffer)
          (run-hook-with-args 'pi-mode-after-start-hook session)
          session)
      ;; Launch failed before a process existed: leave no trace.
      (unless (pi-mode-session-process session)
        (let ((buffer (pi-mode-session-buffer session)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

;;;###autoload
(defun pi-mode-start ()
  "Start a pi session in the current project."
  (interactive)
  (pi-mode--launch-buffer (pi-mode--project-root) pi-mode-cli-args))

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
      (user-error "No running pi session; start one with `pi-mode-start'"))
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


;;; Prompt history

(defcustom pi-mode-prompt-history-file
  (locate-user-emacs-file "pi-mode-history")
  "File where the pi-mode prompt history is persisted."
  :type 'file
  :group 'pi-mode)

(defcustom pi-mode-prompt-history-length 200
  "Maximum number of entries kept in the prompt history."
  :type 'integer
  :group 'pi-mode)

(defvar pi-mode-prompt-history nil
  "Prompt history ring (newest first).")

(defvar pi-mode-prompt-history--loaded nil)

(defun pi-mode--prompt-history-load ()
  (unless pi-mode-prompt-history--loaded
    (setq pi-mode-prompt-history--loaded t)
    (when (file-exists-p pi-mode-prompt-history-file)
      (with-temp-buffer
        (insert-file-contents pi-mode-prompt-history-file)
        (setq pi-mode-prompt-history
              (ignore-errors (read (buffer-string))))))))

(defun pi-mode--prompt-history-save ()
  (with-temp-file pi-mode-prompt-history-file
    (prin1 pi-mode-prompt-history (current-buffer))))

(defun pi-mode--prompt-history-push (text)
  "Add TEXT to the prompt history, deduped and trimmed.
Dedupe is membership-based so no prompt appears twice in the ring."
  (pi-mode--prompt-history-load)
  (unless (member text pi-mode-prompt-history)
    (push text pi-mode-prompt-history)
    (when (> (length pi-mode-prompt-history) pi-mode-prompt-history-length)
      (setcdr (nthcdr (1- pi-mode-prompt-history-length) pi-mode-prompt-history) nil)))
  (pi-mode--prompt-history-save))

;;; Sending

(defun pi-mode--session-live-p (session)
  "Return non-nil when SESSION has a live buffer and process."
  (and session
       (buffer-live-p (pi-mode-session-buffer session))
       (process-live-p (pi-mode-session-process session))))

(defun pi-mode--send-text (session text)
  "Send TEXT to SESSION's pi via bracketed paste and return."
  (unless (pi-mode--session-live-p session)
    (user-error "pi session is not running; start one with `pi-mode-start'"))
  (run-hook-with-args 'pi-mode-before-send-hook session text)
  (with-current-buffer (pi-mode-session-buffer session)
    (ghostel-paste-string text)
    (ghostel-send-key "return"))
  (pi-mode-log "sent %S" (substring text 0 (min 80 (length text))))
  text)

(defun pi-mode--read-prompt ()
  "Read a prompt from the minibuffer with pi-mode history navigation."
  (pi-mode--prompt-history-load)
  (let ((history-add-new-input nil))
    (read-from-minibuffer "pi> " nil nil t 'pi-mode-prompt-history)))

;;;###autoload
(defun pi-mode-send-prompt ()
  "Send a prompt to the target pi session."
  (interactive)
  (let* ((session (pi-mode--resolve-session current-prefix-arg))
         (text (pi-mode--read-prompt)))
    (when (and text (> (length text) 0))
      (pi-mode--prompt-history-push text)
      (pi-mode--send-text session text))))

;;;###autoload
(defun pi-mode-interrupt ()
  "Send the escape key to interrupt the target pi session."
  (interactive)
  (let ((session (pi-mode--resolve-session current-prefix-arg)))
    (with-current-buffer (pi-mode-session-buffer session)
      (ghostel-send-key "escape" nil))
    (pi-mode-log "interrupt sent")))

(define-key pi-mode-map (kbd "C-<escape>") #'pi-mode-interrupt)

;;; Region and context sending

(defcustom pi-mode-region-embed-p t
  "When non-nil (default), region sends use the embedded <file> block format.
With a prefix argument (C-u), a send switches to @file#Lstart-Lend
reference mode instead."
  :type 'boolean
  :group 'pi-mode)

(defun pi-mode--embed-file (file content)
  "Wrap CONTENT in pi's <file name=...> embed convention."
  (format "<file name=\"%s\">\n%s\n</file>" file content))

(defun pi-mode--region-line-range (start end)
  "Return (START-LINE END-LINE) for the 1-based lines of START..END."
  (list (line-number-at-pos start) (line-number-at-pos end)))

(defun pi-mode--send-context (session file content start end reference-p)
  "Send FILE's region START..END to SESSION.
When REFERENCE-P, send @file#Lstart-Lend text; otherwise embed CONTENT.
Records a non-empty prompt in the prompt history (spec: all sends do)."
  (let* ((prompt (pi-mode--read-prompt))
         (text (if reference-p
                   (format "@%s#L%d-L%d" file start end)
                 (pi-mode--embed-file file content)))
         (full (if (and prompt (> (length prompt) 0))
                   (concat prompt "\n\n" text)
                 text)))
    (when (and prompt (> (length prompt) 0))
      (pi-mode--prompt-history-push prompt))
    (pi-mode--send-text session full)))

(defun pi-mode--send-region-internal (start end reference-p)
  (unless (< start end)
    (user-error "No active region"))
  (let* ((session (pi-mode--resolve-session nil t))
         (file (or (buffer-file-name)
                   (buffer-name (current-buffer))))
         (content (buffer-substring-no-properties start end))
         (range (pi-mode--region-line-range start end)))
    (pi-mode--send-context session file content (car range) (cadr range) reference-p)))

;;;###autoload
(defun pi-mode-send-region (start end &optional reference-p)
  "Send the region to the target pi session.
With prefix argument, send a @file#Lstart-Lend reference instead of the
embedded region content."
  (interactive "r\nP")
  (pi-mode--send-region-internal start end reference-p))

;;;###autoload
(defun pi-mode-send-file (file)
  "Send FILE (read from disk) to the target pi session."
  (interactive "fFile to send: ")
  (let* ((session (pi-mode--resolve-session nil t))
         (content (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))))
    (pi-mode--send-context session file content 1 1 nil)))

(defun pi-mode--defun-bounds ()
  "Return (START . END) for the defun at point, or nil.

Portable replacement for `bounds-of-defun-at-point', which was removed
in Emacs 30 (obsolete since 29); this mode targets Emacs 28.1+.  Uses
the same end-then-begin order as `mark-defun'."
  (save-excursion
    (ignore-errors
      (let ((end (progn (end-of-defun) (point)))
            (beg (progn (beginning-of-defun) (point))))
        (when (> end beg)
          (cons beg end))))))

;;;###autoload
(defun pi-mode-send-defun ()
  "Send the defun at point to the target pi session."
  (interactive)
  (let* ((bounds (pi-mode--defun-bounds)))
    (unless bounds
      (user-error "No defun at point"))
    (pi-mode--send-region-internal (car bounds) (cdr bounds) nil)))

(defun pi-mode--error-at-point ()
  "Return (FILE LINE COLUMN MESSAGE) for the error at point, or nil."
  (let ((msg (get-text-property (point) 'compilation-message)))
    (if msg
        (let* ((loc (compilation--message->loc msg))
               (file-struct (compilation--loc->file-struct loc))
               (file (caar file-struct)))
          (list file
                (compilation--loc->line loc)
                (compilation--loc->col loc)
                ;; the message struct has no text field; the message text is
                ;; the buffer text at the error position
                (string-trim
                 (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position)))))
      (when-let ((s (thing-at-point 'filename)))
        (when (string-match "\\(.+\\):\\([0-9]+\\):\\([0-9]+\\)" s)
          (list (match-string 1 s)
                (string-to-number (match-string 2 s))
                (string-to-number (match-string 3 s))
                nil))))))

;;;###autoload
(defun pi-mode-send-error ()
  "Send the error at point (compilation message or file:line:col)."
  (interactive)
  (let* ((loc (pi-mode--error-at-point)))
    (unless loc
      (user-error "No error found at point"))
    (let* ((file (nth 0 loc)) (line (nth 1 loc)) (col (nth 2 loc)) (msg (nth 3 loc))
           (session (pi-mode--resolve-session nil t))
           (content (when (file-readable-p file)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (let* ((l (max 1 (- line 5)))
                               (e (min (point-max) (save-excursion
                                                     (goto-char (point-min))
                                                     (forward-line (1+ line))
                                                     (point)))))
                          (buffer-substring-no-properties
                           (save-excursion (goto-char (point-min)) (forward-line (1- l)) (point))
                           e)))))
           (text (format "Error at %s:%s:%s%s\n%s"
                         file line col
                         (if msg (format ": %s" msg) "")
                         (or content ""))))
      (pi-mode--send-text session text))))

;;; Window commands

(defcustom pi-mode-window-height 0.3
  "Height fraction for pi side windows."
  :type 'number
  :group 'pi-mode)

;; Pi buffers dock in a bottom side window when displayed with
;; `display-buffer'.  `window-height' must be a NUMBER or a function
;; that resizes the window itself (a function's return value is ignored
;; in Emacs 28+); the backquote splices the defcustom's numeric value at
;; load time, so changing `pi-mode-window-height' afterwards requires
;; re-adding this entry.
(add-to-list 'display-buffer-alist
             `("\\*pi\\["
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . ,pi-mode-window-height)))

(defvar pi-mode--panel-hidden nil)

;;;###autoload
(defun pi-mode-toggle-panel ()
  "Hide or restore the pi side window."
  (interactive)
  (if pi-mode--panel-hidden
      (progn
        (when-let ((session (car (pi-mode--active-sessions))))
          (display-buffer (pi-mode-session-buffer session)))
        (setq pi-mode--panel-hidden nil)
        (message "pi panel shown")
        :shown)
    (let ((killed nil))
      (dolist (win (window-list))
        (when (string-match-p "\\*pi\\[" (buffer-name (window-buffer win)))
          (delete-window win)
          (setq killed t)))
      (setq pi-mode--panel-hidden killed)
      (message "pi panel hidden")
      :hidden)))

;;;###autoload
(defun pi-mode-show-all ()
  "Display buffers of all live pi sessions."
  (interactive)
  (let ((sessions (pi-mode--active-sessions)))
    (unless sessions (user-error "No running pi sessions"))
    (dolist (s sessions)
      (display-buffer (pi-mode-session-buffer s)))))

;;;###autoload
(defun pi-mode-toggle-recent ()
  "Display the most recently used pi session."
  (interactive)
  (let ((sessions (pi-mode--active-sessions)))
    (unless sessions (user-error "No running pi sessions"))
    (display-buffer (pi-mode-session-buffer (pi-mode--mru-session sessions)))))

(defun pi-mode-show-debug ()
  "Show the pi-mode debug buffer."
  (interactive)
  (display-buffer "*pi-mode-debug*"))

(defun pi-mode-toggle-debug ()
  "Toggle `pi-mode-debug'."
  (interactive)
  (setq pi-mode-debug (not pi-mode-debug))
  (message "pi-mode debug %s" (if pi-mode-debug "on" "off")))

;;; Configure commands (spec 8 Configure group)

;;;###autoload
(defun pi-mode-configure-model (model)
  "Set pi's model via /model."
  (interactive "sModel: ")
  (let ((session (pi-mode--resolve-session current-prefix-arg)))
    (with-current-buffer (pi-mode-session-buffer session)
      (ghostel-send-string (format "/model %s" model))
      (ghostel-send-key "return"))))

;;;###autoload
(defun pi-mode-configure-thinking ()
  "Cycle pi's thinking level (shift+tab)."
  (interactive)
  (let ((session (pi-mode--resolve-session current-prefix-arg)))
    (with-current-buffer (pi-mode-session-buffer session)
      (ghostel-send-key "tab" "shift"))))

;;;###autoload
(defun pi-mode-configure-tui-mode ()
  "Flip --tui-mode regular/fullscreen and relaunch the session."
  (interactive)
  (let* ((session (pi-mode--resolve-session current-prefix-arg))
         (current (if (member "--tui-mode" pi-mode-cli-args)
                      (cadr (member "--tui-mode" pi-mode-cli-args))
                    "regular"))
         (next (if (equal current "fullscreen") "regular" "fullscreen")))
    (when (y-or-n-p (format "Switch TUI mode to %s? The session restarts. " next))
      (setq pi-mode-cli-args
            (cl-remove "--tui-mode" (cl-remove "fullscreen" (cl-remove "regular" pi-mode-cli-args) :test #'equal) :test #'equal))
      (push "--tui-mode" pi-mode-cli-args)
      (push next pi-mode-cli-args)
      (setf (pi-mode-session-exit-requested session) t)
      (delete-process (pi-mode-session-process session))
      (pi-mode--launch-buffer (pi-mode-session-project-root session) pi-mode-cli-args)
      (pi-mode-log "tui-mode switched to %s" next))))

;;;###autoload
(defun pi-mode-configure-cli-args ()
  "Customize `pi-mode-cli-args'."
  (interactive)
  (customize-variable 'pi-mode-cli-args))

;;; Keymap and global binding

(define-key pi-mode-map (kbd "C-c C-'") #'pi-mode-menu)

;;;###autoload
(define-key global-map (kbd "C-c C-'") #'pi-mode-menu)

(provide 'pi-mode)

;; Require after `provide' so that pi-mode-session.el's own
;; `(require 'pi-mode)' resolves via `featurep' — a require before the
;; provide would re-enter pi-mode.el mid-load ("Recursive load").
(require 'pi-mode-session)
;; Same reasoning: pi-mode-menu.el requires both pi-mode and
;; pi-mode-session.
(require 'pi-mode-menu)

;;; pi-mode.el ends here
