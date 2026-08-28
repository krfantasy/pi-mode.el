;;; pi-mode.el --- Emacs interface for the pi coding agent -*- lexical-binding: t; -*-

;; Author: Jay Xu
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.49") (transient "0.7"))
;; Keywords: tools, processes
;; URL: https://github.com/krfantasy/pi-mode.el
;; License: The License

;;; Commentary:
;; Run the pi coding agent inside a ghostel terminal buffer with project
;; detection, prompt-input region sending, prompt editing in a markdown
;; popup, session management, and a transient command menu.

;;; Code:

(require 'cl-lib)
(require 'project)          ; project-root is not autoloaded
(require 'ghostel)
(require 'transient)

(defgroup pi nil
  "Interface for the pi coding agent."
  :group 'tools
  :prefix "pi-mode-")

(defconst pi-mode-version "0.1.0"
  "Version of pi-mode; keep in sync with the `Version' header above.")

(defcustom pi-mode-debug nil
  "When non-nil, log pi-mode activity to the *pi-mode-debug* buffer."
  :type 'boolean
  :group 'pi)

(defvar pi-mode-map (make-sparse-keymap)
  "Keymap for `pi-mode' buffers.")

(define-minor-mode pi-mode
  "Minor mode for buffers running the pi coding agent.

\\{pi-mode-map}"
  :group 'pi
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
  :group 'pi)

(defun pi-mode--project-root ()
  "Return the project root for the current context."
  (let ((root (if pi-mode-project-root-function
                  (funcall pi-mode-project-root-function)
                (when-let ((proj (project-current)))
                  (project-root proj)))))
    (or root default-directory)))

(defcustom pi-mode-buffer-name-function nil
  "Function returning the base buffer name for a pi session.
Called with (DIRECTORY &optional NAME); returns a buffer name string.
The <N> collision uniquifier is applied afterwards.  When nil, the
default \"*pi[project]*\" / \"*pi[project:name]*\" naming is used."
  :type '(choice (const :tag "Default naming" nil) function)
  :group 'pi)

(defun pi-mode--session-base-name (project-root &optional name)
  "Base buffer name for PROJECT-ROOT and optional session NAME."
  (if pi-mode-buffer-name-function
      (funcall pi-mode-buffer-name-function project-root name)
    (let ((project (file-name-nondirectory (directory-file-name project-root))))
      (if name (format "*pi[%s:%s]*" project name)
        (format "*pi[%s]*" project)))))

;; The live-session registry (hash table keyed by session id) is defined
;; in the Sessions section below; the naming predicate references it, so
;; declare it here for the byte-compiler (cc-ide pattern).
(defvar pi-mode--sessions)

(defun pi-mode--session-buffer-p (buffer-or-name &rest _args)
  "Non-nil when BUFFER-OR-NAME hosts a pi session.
Accepts a buffer object or a buffer name string: `buffer-match-p'
calls condition predicates with the buffer name plus the action
alist as ARGS.  The registry lookup covers buffers whose local was
wiped by `ghostel-mode' activation."
  (let ((buffer (if (bufferp buffer-or-name) buffer-or-name
                  (get-buffer buffer-or-name))))
    (and buffer
         (buffer-live-p buffer)
         (or (buffer-local-value 'pi-mode--session buffer)
             (gethash (buffer-name buffer) pi-mode--sessions)))))

;;; Sessions

(cl-defstruct pi-mode-session
  "A running pi agent session."
  id name buffer process project-root last-used window-slot
  cleanup-done exit-requested)

(defcustom pi-mode-cli-args nil
  "Extra command-line arguments passed to pi at launch."
  :type '(repeat string)
  :group 'pi)

(defcustom pi-mode-kill-buffer-on-exit t
  "When non-nil, kill the pi buffer when the process exits.
Matches claude-code-ide.el: exiting a session also removes its
terminal buffer.  Set to nil to keep the buffer (e.g. to review
scrollback) after the process ends."
  :type 'boolean
  :group 'pi)

(defcustom pi-mode-confirm-kill t
  "When non-nil, confirm before killing a running pi session."
  :type 'boolean
  :group 'pi)

(defcustom pi-mode-confirm-quit t
  "When non-nil, confirm before stopping a running pi session.
Guards `pi-mode-session-stop' (menu \"q\"); declining the prompt
leaves the session running.  Distinct from `pi-mode-confirm-kill',
which guards killing the session buffer."
  :type 'boolean
  :group 'pi)

(defcustom pi-mode-launch-settle-delay 0.1
  "Seconds to wait after launching pi before checking the process is alive.
The terminal backend may take a moment to surface an immediate CLI
death; the wait lets it happen before the liveness check, so a dead
process errors instead of displaying a stale terminal buffer.
Parity with cc-ide's `claude-code-ide-terminal-initialization-delay'."
  :type 'number
  :group 'pi)

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

(defun pi-mode--project-sessions (&optional root)
  "Return live sessions of project ROOT, most recently used first.
ROOT defaults to the current project (`pi-mode--project-root');
sessions are matched on their `project-root' slot with `equal'.
Reuses the live filter and sort of `pi-mode--active-sessions'."
  (let ((root (or root (pi-mode--project-root))))
    (cl-remove-if-not
     (lambda (s) (equal (pi-mode-session-project-root s) root))
     (pi-mode--active-sessions))))

(defun pi-mode--session-by-buffer (buffer)
  (when buffer
    (gethash (buffer-name buffer) pi-mode--sessions)))

(defun pi-mode--session-by-process (process)
  (cl-loop for s being the hash-values of pi-mode--sessions
           when (eq (pi-mode-session-process s) process) return s))

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

(defun pi-mode--ghostel-launch (buffer project-root args)
  "Launch pi in BUFFER for PROJECT-ROOT with ARGS.
Returns the lifecycle process.  Sets ghostel buffer options that
`ghostel-exec' resets."
  (let* ((default-directory project-root)
         (pi (executable-find "pi")))
    (unless pi
      (user-error "pi executable not found in exec-path; install pi first"))
    (with-current-buffer buffer
      (prog1 (ghostel-exec buffer pi args)
        (setq-local ghostel-kill-buffer-on-exit nil)
        (setq-local ghostel-buffer-name-function nil)))))

(defun pi-mode--attach-sentinel (process)
  "Chain pi-mode cleanup onto PROCESS's sentinel.
Runs the stashed ghostel sentinel first, reports abnormal exit codes,
then `pi-mode--cleanup-session' on exit events."
  (let ((orig (process-sentinel process)))
    (process-put process 'pi-mode--ghostel-sentinel orig)
    (set-process-sentinel
     process
     (lambda (proc event)
       (when (functionp (process-get proc 'pi-mode--ghostel-sentinel))
         (funcall (process-get proc 'pi-mode--ghostel-sentinel) proc event))
       (when (string-match "exited abnormally with code \\([0-9]+\\)" event)
         (message "pi exited with error code %s" (match-string 1 event)))
       (when (string-match-p "finished\\|exited\\|killed\\|terminated\\|deleted\\|closed" event)
         (pi-mode--cleanup-session proc event))))))

(defun pi-mode--cleanup-session (process event)
  "Run session cleanup for PROCESS (idempotent).
EVENT is the raw sentinel event string; `pi-mode-on-exit-hook'
receives it trimmed."
  (when-let ((session (pi-mode--session-by-process process)))
    (unless (pi-mode-session-cleanup-done session)
      (setf (pi-mode-session-cleanup-done session) t)
      (let ((buffer (pi-mode-session-buffer session)))
        (pi-mode--unregister-session (pi-mode-session-id session))
        (run-hook-with-args 'pi-mode-on-exit-hook buffer (string-trim event))
        (when (and pi-mode-kill-buffer-on-exit (buffer-live-p buffer))
          (kill-buffer buffer))))))

(defvar-local pi-mode--session-setup-done nil
  "Non-nil when this buffer's pi-mode session locals are applied.")

(defun pi-mode--setup-session-buffer (session)
  "Apply the pi-mode buffer-locals for SESSION to the current buffer.
Idempotent per buffer: `ghostel-exec' activates `ghostel-mode',
whose `kill-all-local-variables' wipes these locals, so the launch
path re-applies them after the terminal is created."
  (setq-local default-directory (pi-mode-session-project-root session))
  (setq-local pi-mode--session session)
  (pi-mode +1)
  (unless pi-mode--session-setup-done
    (setq-local pi-mode--session-setup-done t)
    (setq-local mode-line-misc-info
                (append mode-line-misc-info
                        '((:eval (pi-mode--mode-line-segment)))))))

(defun pi-mode--make-session (project-root &optional name)
  "Create an unregistered session struct for PROJECT-ROOT in a new buffer.
Registration happens in `pi-mode--launch-buffer' after a successful
launch, so a failed launch leaves nothing behind."
  (let* ((buffer-name (generate-new-buffer-name
                       (pi-mode--session-base-name project-root name))))
    (with-current-buffer (get-buffer-create buffer-name)
      ;; The session buffer owns the project root as its local
      ;; `default-directory': the dynamic binding in
      ;; `pi-mode--ghostel-launch' is shadowed by the buffer-local
      ;; value inherited at creation, so without this the process would
      ;; launch in whatever directory the caller happened to be in.
      ;; `pi-mode--setup-session-buffer' is re-applied after launch
      ;; because ghostel-mode activation wipes these locals.
      (let ((session (make-pi-mode-session
                      :id buffer-name :name name :buffer (current-buffer)
                      :project-root project-root :last-used (current-time))))
        (pi-mode--setup-session-buffer session)
        session))))

(defun pi-mode--mode-line-segment ()
  "Session-name segment for the mode line.
The \" Pi\" lighter comes from `pi-mode'; this adds the session name."
  (if (and pi-mode--session (pi-mode-session-name pi-mode--session))
      (concat " " (pi-mode-session-name pi-mode--session))
    ""))

(defun pi-mode--launch-buffer (project-root args &optional name)
  "Create a session buffer, launch pi in it, and display it.
The session is registered only after a successful launch; when the
launch fails the scratch buffer is removed."
  (let ((session (pi-mode--make-session project-root name)))
    (unwind-protect
        (let* ((buffer (pi-mode-session-buffer session))
               (process (pi-mode--ghostel-launch buffer project-root args)))
          ;; Give a process that dies right after spawn time to surface,
          ;; then fail with a real explanation instead of displaying a
          ;; dead terminal buffer (cc-ide parity,
          ;; claude-code-ide.el:1477-1484).  The process slot is still
          ;; nil here, so the unwind-protect below removes the buffer.
          (sleep-for pi-mode-launch-settle-delay)
          (unless (process-live-p process)
            (error "pi exited immediately after startup.  Verify that the pi binary (%s) is executable and launches in %s"
                   (executable-find "pi") project-root))
          (setf (pi-mode-session-process session) process)
          ;; ghostel-exec activates ghostel-mode, whose
          ;; kill-all-local-variables wipes the session locals; re-apply
          ;; them so the display-buffer predicate, kill-buffer guard and
          ;; mode-line segment keep working.
          (with-current-buffer buffer
            (pi-mode--setup-session-buffer session))
          (setf (pi-mode-session-window-slot session)
                (pi-mode--launch-window-slot session))
          (pi-mode--register-session session)
          (pi-mode--attach-sentinel process)
          ;; display-buffer, not pop-to-buffer: window selection is
          ;; governed by `pi-mode-focus-on-open' like every other
          ;; display path (cc-ide parity, claude-code-ide.el:1487).
          (display-buffer buffer)
          (run-hook-with-args 'pi-mode-after-start-hook session)
          session)
      ;; Launch failed before a process existed: leave no trace.
      (unless (pi-mode-session-process session)
        (let ((buffer (pi-mode-session-buffer session)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(defun pi-mode--read-instance-name (&optional root prompt exclude-session)
  "Read and validate an instance name for project ROOT.
ROOT defaults to the current project (`pi-mode--project-root').
PROMPT overrides the prompt string.  EXCLUDE-SESSION (a session
struct) is ignored in the duplicate check, so a session can keep its
own name when renamed.
Empty input returns nil (auto-named).  Pure-numeric names are
rejected (reserved for auto-numbering), as are names containing
`[', `]', `*', or control characters, and names already used by a
live session of the same project; each rejection messages the reason
and re-prompts.  Returns the trimmed name string or nil."
  (let ((root (or root (pi-mode--project-root)))
        name done)
    (while (not done)
      (setq name (string-trim
                  (read-string (or prompt "Instance name (empty for auto): "))))
      (cond
       ((string-empty-p name)
        (setq name nil done t))
       ((string-match-p "\\`[0-9]+\\'" name)
        (message "Numeric names are reserved for auto-numbering")
        (sit-for 1))
       ((string-match-p "[][*[:cntrl:]]" name)
        (message "Name cannot contain [, ], or *")
        (sit-for 1))
       ((cl-some (lambda (session)
                   (and (not (eq session exclude-session))
                        (equal name (pi-mode-session-name session))))
                 (pi-mode--project-sessions root))
        (message "Name already used in this project: %s" name)
        (sit-for 1))
       (t (setq done t))))
    name))

(defun pi-mode--maybe-read-instance-name (root)
  "Return the instance name for a new session of project ROOT.
Prompts via `pi-mode--read-instance-name' when ROOT already has live
sessions, or when a prefix argument is given (so C-u offers the prompt
for the first instance too); otherwise returns nil and the session is
auto-named.  Matches cc-ide's trigger in `claude-code-ide--start-session'."
  (when (or current-prefix-arg (pi-mode--project-sessions root))
    (pi-mode--read-instance-name root)))

;;;###autoload
(defun pi-mode-start (&optional _prefix-arg)
  "Start a pi session in the current project.
Prompts for an instance name (empty for auto-naming) when the project
already has running sessions; a prefix argument prompts even for the
first instance.
The optional PREFIX-ARG is unused: it exists so the value produced by the
`(interactive \"P\")' spec can be passed by `call-interactively' and
transient without a wrong-number-of-arguments error."
  (interactive "P")
  (let ((root (pi-mode--project-root)))
    (pi-mode--launch-buffer root pi-mode-cli-args
                            (pi-mode--maybe-read-instance-name root))))

;;; Target resolution

(defun pi-mode--prompt-session (sessions)
  "Prompt for one of SESSIONS and return it.
Candidates carry the display name, abbreviated project path and
visibility state, so sessions of different projects are
distinguishable (cc-ide parity, claude-code-ide.el:1663-1680).
A raw session id also matches (legacy callers)."
  (let* ((candidates
          (mapcar
           (lambda (s)
             (cons (format "%s — %s (%s)"
                           (or (pi-mode-session-name s)
                               (file-name-nondirectory
                                (directory-file-name
                                 (pi-mode-session-project-root s))))
                           (abbreviate-file-name (pi-mode-session-project-root s))
                           (if (pi-mode--visible-sessions (list s))
                               "visible" "hidden"))
                   s))
           sessions))
         (choice (completing-read "pi session: " candidates nil t)))
    (or (gethash choice pi-mode--sessions)
        (cdr (assoc choice candidates)))))

(defun pi-mode--resolve-session (&optional prefix no-ask intent)
  "Resolve the target session for a command.
Resolution is scoped to the current project
(`pi-mode--project-root').  PREFIX non-nil means the user gave C-u:
prompt unless NO-ASK, even from a session buffer.  Without a prefix
the current buffer's session wins, whichever project it belongs to;
then sole; sole-visible; else INTENT `prompt' prompts instead of
guessing, otherwise the MRU session is used with an echo.  Signals
`user-error' when the current project has no live session.
The resolved session's `last-used' is updated (MRU semantics)."
  (let* ((root (pi-mode--project-root))
         (sessions (pi-mode--project-sessions root))
         (session
          (cond
           ((null sessions)
            (user-error
             "No running pi sessions in project %s; start one with `pi-mode-start'"
             root))
           ((and prefix (not no-ask))
            (pi-mode--prompt-session sessions))
           ((pi-mode--session-by-buffer (current-buffer)))
           ((= (length sessions) 1)
            (car sessions))
           ((= (length (pi-mode--visible-sessions sessions)) 1)
            (car (pi-mode--visible-sessions sessions)))
           ((and (eq intent 'prompt) (not no-ask))
            (pi-mode--prompt-session sessions))
           (t
            (let ((mru (car sessions)))
              (message "pi-mode: using session %s" (pi-mode-session-id mru))
              mru)))))
    (when session
      (setf (pi-mode-session-last-used session) (current-time)))
    session))

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


;;; Sending

(defun pi-mode--session-live-p (session)
  "Return non-nil when SESSION has a live buffer and process."
  (and session
       (buffer-live-p (pi-mode-session-buffer session))
       (process-live-p (pi-mode-session-process session))))

(defun pi-mode--insert-text (session text)
  "Insert TEXT into SESSION's pi prompt input without submitting.
The text is pasted into pi's input box; press Return in the pi buffer
to send it."
  (unless (pi-mode--session-live-p session)
    (user-error "pi session is not running; start one with `pi-mode-start'"))
  (run-hook-with-args 'pi-mode-before-send-hook session text)
  (with-current-buffer (pi-mode-session-buffer session)
    (ghostel-paste-string text))
  (pi-mode-log "inserted %S" (substring text 0 (min 80 (length text))))
  text)

;;;###autoload
(defun pi-mode-interrupt ()
  "Send the escape key to interrupt the target pi session."
  (interactive)
  (let ((session (pi-mode--resolve-session current-prefix-arg)))
    (with-current-buffer (pi-mode-session-buffer session)
      (ghostel-send-key "escape" nil))
    (pi-mode-log "interrupt sent")))

(define-key pi-mode-map (kbd "C-<escape>") #'pi-mode-interrupt)
(define-key pi-mode-map (kbd "C-c C-i") #'pi-mode-edit-prompt)
(define-key pi-mode-map (kbd "S-<return>") #'pi-mode-insert-newline)

;;; Prompt sending

;;;###autoload
(defun pi-mode-send-prompt ()
  "Send a prompt typed in the minibuffer to the target pi session.
The text is sent like typed input — the string followed by Return —
so multi-line prompts and commands work.  Blank input sends nothing.
The target is resolved like `pi-mode-interrupt' (prefix argument
prompts)."
  (interactive)
  (let* ((session (pi-mode--resolve-session current-prefix-arg))
         (prompt (read-string "pi prompt: ")))
    (when (not (string-empty-p (string-trim prompt)))
      (with-current-buffer (pi-mode-session-buffer session)
        (ghostel-send-string prompt)
        ;; Let the TUI process the text before Return (cc-ide parity,
        ;; claude-code-ide.el:1750).
        (sit-for 0.1)
        (ghostel-send-key "return"))
      (pi-mode-log "sent prompt %S" (substring prompt 0 (min 80 (length prompt)))))))

;;;###autoload
(defun pi-mode-insert-newline ()
  "Insert a newline in the target pi session's prompt input.
Sends a backslash followed by Return — pi's multiline-input idiom —
so the prompt stays open on a new line instead of submitting."
  (interactive)
  (let ((session (pi-mode--resolve-session current-prefix-arg)))
    (with-current-buffer (pi-mode-session-buffer session)
      (ghostel-send-string "\\")
      ;; Let the TUI process the backslash before Return (cc-ide
      ;; parity, claude-code-ide.el:1717-1718).
      (sit-for 0.1)
      (ghostel-send-key "return"))
    (pi-mode-log "newline inserted")))

;;; Prompt editing

(defcustom pi-mode-prompt-editor-padding-x 0
  "Fallback horizontal padding (in columns) of pi's TUI prompt editor.
Used when pi's own `editorPaddingX' setting cannot be read from its
settings files; see `pi-mode--prompt-editor-padding'."
  :type 'integer
  :group 'pi)

(defun pi-mode--read-editor-padding-file (file)
  "Return pi's `editorPaddingX' from settings FILE, or nil."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (when-let* ((json (ignore-errors (json-parse-string (buffer-string))))
                  (padding (gethash "editorPaddingX" json)))
        (and (integerp padding) (<= 0 padding) padding)))))

(defun pi-mode--prompt-editor-padding (&optional session)
  "pi's prompt-editor side padding for SESSION (columns).
Reads `editorPaddingX' from pi's settings files — the project
`.pi/settings.json' first, then the agent `settings.json' — mirroring
pi's own precedence; falls back to `pi-mode-prompt-editor-padding-x'."
  (let* ((agent-dir (or (getenv "PI_CODING_AGENT_DIR")
                        (expand-file-name "~/.pi/agent")))
         (project-settings
          (and session
               (expand-file-name ".pi/settings.json"
                                 (pi-mode-session-project-root session))))
         (padding (or (and project-settings
                           (pi-mode--read-editor-padding-file project-settings))
                      (pi-mode--read-editor-padding-file
                       (expand-file-name "settings.json" agent-dir)))))
    (or padding pi-mode-prompt-editor-padding-x)))

(defun pi-mode--prompt-border-row-p (row)
  "Non-nil when ROW is a pi TUI prompt-editor border row.
The editor draws a full-width line of `─' above and below the input
box; when the editor is internally scrolled (prompt taller than the
visible area) the border carries a \"─── ↑ N more \" / \"─── ↓ N more \"
indicator instead."
  (or (string-match-p "\\`─+\\'" row)
      (string-match-p "\\`─── [↑↓]" row)))

(defun pi-mode--prompt-border (rows cursor-row step)
  "Index of the editor border row STEP rows away from CURSOR-ROW in ROWS.
STEP is -1 to scan upward or 1 to scan downward; nil when none."
  (let ((i (+ cursor-row step)))
    (while (and (<= 0 i) (< i (length rows))
                (not (pi-mode--prompt-border-row-p (nth i rows))))
      (cl-incf i step))
    (and (<= 0 i) (< i (length rows)) i)))

(defun pi-mode--prompt-join-lines (lines wrap-width)
  "Rejoin pi editor LINES (visual wraps) into logical prompt text.
pi's editor wraps long lines instead of scrolling horizontally, so a
line that exactly fills WRAP-WIDTH columns (pi's layout width) is a
visual continuation of the previous logical line and joins it without
a newline.  (A logical line of exactly WRAP-WIDTH columns followed by
another line is indistinguishable from a wrap and is joined too — a
deliberate, documented approximation.)"
  (let ((parts nil) (prev-full nil))
    (dolist (line lines)
      (when parts
        (push (if prev-full "" "\n") parts))
      (push line parts)
      (setq prev-full (= (string-width line) wrap-width)))
    (apply #'concat (nreverse parts))))

(defun pi-mode--prompt-extract (rows cursor-row &optional padding-x)
  "Extract the prompt text from pi's TUI screen ROWS.
ROWS is the terminal screen as a list of strings; CURSOR-ROW the
0-based index of the row holding the terminal cursor (the prompt
editor keeps the cursor visible, so it anchors the input region).
PADDING-X is the editor's side padding (default
`pi-mode-prompt-editor-padding-x').

Returns (CONTENT . SCROLLED-P), or nil when no editor border frames
the cursor row (the input box cannot be located).  SCROLLED-P is
non-nil when the editor is internally scrolled, meaning ROWS show
only part of the prompt."
  (let* ((padding-x (or padding-x pi-mode-prompt-editor-padding-x))
         (top (pi-mode--prompt-border rows cursor-row -1))
         (bottom (pi-mode--prompt-border rows cursor-row 1)))
    (when (and top bottom)
      (let* ((region (cl-subseq rows (1+ top) bottom))
             (width (apply #'max (mapcar #'string-width
                                         (append region
                                                 (list (nth top rows)
                                                       (nth bottom rows))))))
             (scrolled (not (not (or (string-match-p "\\`─── [↑↓]" (nth top rows))
                                     (string-match-p "\\`─── [↑↓]" (nth bottom rows))))))
             (lines (mapcar (lambda (row)
                              (string-trim-right
                               (if (>= (length row) padding-x)
                                   (substring row padding-x)
                                 row)))
                            region))
             ;; pi wraps at the layout width: the content width, minus one
             ;; column reserved for the cursor when there is no padding.
             (wrap-width (- width (* 2 padding-x)
                            (if (zerop padding-x) 1 0))))
        (cons (pi-mode--prompt-join-lines lines wrap-width)
              scrolled)))))

(defun pi-mode--prompt-screen-rows ()
  "Return (ROWS . CURSOR-ROW) for the current ghostel buffer, or nil.
ROWS is the terminal screen (viewport) as a list of strings;
CURSOR-ROW the 0-based viewport row of the terminal cursor."
  (when-let* ((vp-start (ghostel--viewport-start))
              (cursor-point (ghostel-cursor-point))
              (cursor-row (ghostel--viewport-row-at cursor-point)))
    (let ((rows (split-string
                 (buffer-substring-no-properties vp-start (point-max)) "\n")))
      (when (equal (car (last rows)) "")
        (setq rows (butlast rows)))
      (when (< cursor-row (length rows))
        (cons rows cursor-row)))))

(defun pi-mode--read-prompt (session)
  "Read the prompt currently in SESSION's pi input box.
Signals `user-error' when the input box cannot be located."
  (let* ((padding (pi-mode--prompt-editor-padding session))
         (screen (with-current-buffer (pi-mode-session-buffer session)
                   (pi-mode--prompt-screen-rows)))
         (extracted (and screen
                         (pi-mode--prompt-extract (car screen) (cdr screen)
                                                  padding))))
    (unless extracted
      (user-error "Cannot locate pi's prompt editor; is the pi TUI showing the input box?"))
    (when (cdr extracted)
      (message "pi-mode: prompt editor is scrolled; only the visible part was captured"))
    (car extracted)))

(defvar-local pi-mode--prompt-edit-session nil
  "The pi session this prompt-edit buffer syncs back to.")

(defvar pi-mode-prompt-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pi-mode-prompt-edit-submit)
    (define-key map (kbd "C-c C-k") #'pi-mode-prompt-edit-cancel)
    map)
  "Keymap for `pi-mode-prompt-edit-mode'.")

(define-minor-mode pi-mode-prompt-edit-mode
  "Minor mode for editing a pi prompt in a markdown buffer.

The buffer shows a separedit-style header line with the
instructions: \\<pi-mode-prompt-edit-mode-map>
\\[pi-mode-prompt-edit-submit] finishes (\"Finish\") and replaces the
prompt in pi's input box with this buffer's content;
\\[pi-mode-prompt-edit-cancel] aborts (\"Abort\") and discards the
edit without touching pi."
  :lighter " π✎"
  :keymap pi-mode-prompt-edit-mode-map
  (if pi-mode-prompt-edit-mode
      (setq-local header-line-format
                  (substitute-command-keys
                   (concat "*pi prompt* "
                           (mapconcat #'identity
                                      '("\\[pi-mode-prompt-edit-submit]: Finish"
                                        "\\[pi-mode-prompt-edit-cancel]: Abort")
                                      ", "))))
    (kill-local-variable 'header-line-format)))

(defun pi-mode-prompt-edit-submit ()
  "Replace the pi session's prompt with this buffer's content, then close.
The session's input box is cleared (pi's app.clear) and the buffer
content is pasted in; nothing is submitted, so press Return in the pi
buffer to send the edited prompt."
  (interactive)
  (let ((session pi-mode--prompt-edit-session))
    (unless (pi-mode--session-live-p session)
      (user-error "The pi session is no longer running; the edit is kept in this buffer"))
    (let ((text (string-trim-right (buffer-string))))
      (with-current-buffer (pi-mode-session-buffer session)
        (ghostel-send-key "c" "ctrl")) ; pi's app.clear: wipe the input box
      (pi-mode--insert-text session text)
      (let ((id (pi-mode-session-id session)))
        (kill-buffer)
        (message "Prompt synced to pi session %s" id)))))

(defun pi-mode-prompt-edit-cancel ()
  "Discard the edit and close the prompt-edit buffer without touching pi."
  (interactive)
  (kill-buffer))

;;;###autoload
(defun pi-mode-edit-prompt ()
  "Edit the target pi session's prompt in a markdown popup buffer.

Opens a buffer with the prompt currently in pi's input box, or
switches to it when already open.  \\<pi-mode-prompt-edit-mode-map>
\\[pi-mode-prompt-edit-submit] replaces the prompt in pi's input box
with the edited text and closes the buffer; \\[pi-mode-prompt-edit-cancel]
discards the edit.  With a prefix argument, prompts for the session."
  (interactive)
  (let* ((session (pi-mode--resolve-session current-prefix-arg))
         (name (format "*pi prompt %s*" (pi-mode-session-id session)))
         (buffer (get-buffer name)))
    (if (and (buffer-live-p buffer)
             (buffer-local-value 'pi-mode--prompt-edit-session buffer))
        (pop-to-buffer buffer)
      (let ((text (pi-mode--read-prompt session)))
        (with-current-buffer (get-buffer-create name)
          ;; The major mode runs `kill-all-local-variables', so set the
          ;; session local *after* it, before the minor mode.
          (if (require 'markdown-mode nil t)
              (unless (derived-mode-p 'markdown-mode)
                (markdown-mode))
            (text-mode))
          (setq-local pi-mode--prompt-edit-session session)
          (erase-buffer)
          (insert text)
          (goto-char (point-min))
          (pi-mode-prompt-edit-mode +1)
          (set-buffer-modified-p nil))
        (pop-to-buffer name)
        (pi-mode-log "prompt editor opened for %s" (pi-mode-session-id session))))))

;;; Region and file sending

(defvar pi-mode--last-selection nil
  "Last active region snapshot (BUFFER START END), or nil.
Updated by `pi-mode--track-selection' on every command while a region
is active in a file buffer.  `pi-mode-insert-selection' falls back to
it when the region was deactivated before the command ran.")

(defun pi-mode--track-selection ()
  "Snapshot the current region for `pi-mode-insert-selection'.
Runs on `post-command-hook'; only file buffers are tracked (the pi
terminal and non-file buffers are not selection sources).  This is
pi-mode's stand-in for cc-ide's MCP selection tracking, minus the
MCP plane (claude-code-ide-mcp.el:717-736)."
  (when (and (buffer-file-name) (use-region-p))
    (setq pi-mode--last-selection
          (list (current-buffer) (region-beginning) (region-end)))))

(add-hook 'post-command-hook #'pi-mode--track-selection)

;;;###autoload
(defun pi-mode-insert-selection ()
  "Insert the current or last region into the target pi session's prompt.
The text is pasted without submitting (press Return in the pi buffer
to send it).  An active region wins; otherwise the last region that
was active in a file buffer is used, so a selection that was
deactivated before the command ran (e.g. in transient-selection-mode
workflows) is still inserted.  Signals `user-error' when there is no
region to insert."
  (interactive)
  (let* ((session (pi-mode--resolve-session nil t))
         (content
          (cond
           ((use-region-p)
            (pi-mode--track-selection) ; keep the snapshot fresh
            (buffer-substring-no-properties (region-beginning) (region-end)))
           ((and pi-mode--last-selection
                 (buffer-live-p (car pi-mode--last-selection)))
            (with-current-buffer (car pi-mode--last-selection)
              (buffer-substring-no-properties (nth 1 pi-mode--last-selection)
                                              (nth 2 pi-mode--last-selection))))
           (t (user-error "No active or recent region to insert")))))
      (pi-mode--insert-text session content)))

;;;###autoload
(defun pi-mode-send-region (start end)
  "Insert the region into the target pi session's prompt input.
The region content is pasted without submitting; press Return in the
pi buffer to send it."
  (interactive "r")
  (unless (< start end)
    (user-error "No active region"))
  (let* ((session (pi-mode--resolve-session nil t))
         (content (buffer-substring-no-properties start end)))
    (pi-mode--insert-text session content)))

;;;###autoload
(defun pi-mode-send-file ()
  "Insert an @-reference to the current buffer's file into pi's prompt input.
The reference path is relative to the target session's working
directory.  The text is pasted without submitting; press Return in the
pi buffer to send it."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (let* ((session (pi-mode--resolve-session nil t))
           (text (concat "@"
                         (file-relative-name file
                                             (pi-mode-session-project-root session)))))
      (pi-mode--insert-text session text))))

;;; Window commands

(defcustom pi-mode-window-side 'right
  "Side of the frame where pi buffers are displayed in a side window."
  :type '(choice (const :tag "Bottom" bottom)
                 (const :tag "Top" top)
                 (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'pi)

(defcustom pi-mode-window-height 20
  "Height of the pi side window when opened on top or bottom."
  :type 'integer
  :group 'pi)

(defcustom pi-mode-window-width 100
  "Body width of the pi side window when opened on left or right.
This sets the usable text area width, excluding fringes and margins."
  :type 'integer
  :group 'pi)

(defcustom pi-mode-focus-on-open t
  "Select the pi side window whenever a session is displayed.
When non-nil, every display path (starting a session, restoring the
panel with `w', showing all with `a', and toggling the recent session
with `W') selects the pi window, moving focus to the session.  When
nil those paths keep focus where it is."
  :type 'boolean
  :group 'pi)

(defun pi-mode--display-args (buffer)
  "Return (SIDE SLOT SIZE-KEY SIZE-VALUE) for displaying BUFFER.
SIDE is `pi-mode-window-side'; SLOT is the session's `window-slot'
(or 0).  On left/right sides SIZE-KEY is `window-width' and
SIZE-VALUE a function that resizes the chosen window so its body
width lands exactly on `pi-mode-window-width', compensating the
fringe/margin delta; on top/bottom sides SIZE-KEY is
`window-height' and SIZE-VALUE `pi-mode-window-height'.  Reading
the customization at display time keeps changes live without
re-adding a `display-buffer-alist' entry."
  (let* ((side pi-mode-window-side)
         (slot (or (when-let ((session (pi-mode--session-by-buffer buffer)))
                     (pi-mode-session-window-slot session))
                   0))
         (left-or-right (memq side '(left right))))
    (list side slot
          (if left-or-right 'window-width 'window-height)
          (if left-or-right
              ;; display-buffer calls the function with the chosen
              ;; window and ignores its return value; resize failures
              ;; on undersized frames are swallowed by display-buffer.
              (lambda (win)
                (let ((delta (- pi-mode-window-width
                                (window-body-width win))))
                  (unless (zerop delta)
                    (window-resize win delta t))))
            pi-mode-window-height))))

(defun pi-mode--display-buffer (buffer _alist)
  "Display BUFFER in a side window per `pi-mode-window-side' and size.
Each pi session occupies its own slot, so several sessions can be
visible side by side instead of evicting each other.  Left/right
windows are sized to exactly `pi-mode-window-width' body columns;
top/bottom windows to exactly `pi-mode-window-height' text lines.
The chosen window is dedicated to its session buffer and carries
the `no-delete-other-windows' parameter, so `delete-other-windows'
\(C-x 1) keeps it and unrelated `display-buffer' calls cannot reuse
it.  When `pi-mode-focus-on-open' is non-nil the window is selected
and focus moves to the session.  Every display refreshes the
session's MRU stamp, so a `w' restore makes the shown session the
most-recently-used target."
  (cl-destructuring-bind (side slot size-key size-value)
      (pi-mode--display-args buffer)
    (let* ((display-buffer-alist
            `((,(regexp-quote (buffer-name buffer))
               (display-buffer-in-side-window)
               (side . ,side)
               (slot . ,slot)
               (,size-key . ,size-value)
               (window-parameters . ((no-delete-other-windows . t))))))
           (window (display-buffer buffer)))
      ;; Dedicate the chosen window: display-buffer then never reuses it
      ;; for an unrelated buffer (cc-ide parity, claude-code-ide.el:997).
      (when window
        (set-window-dedicated-p window t)
        ;; On top/bottom sides the alist `window-height' value sizes the
        ;; TOTAL height, which drifts from the text height by the mode
        ;; line; re-set the text height exactly (cc-ide parity,
        ;; claude-code-ide.el:991-995).
        (when (memq side '(top bottom))
          (set-window-text-height window pi-mode-window-height))
        ;; Every display path funnels through this action function, so
        ;; selecting here implements focus-on-open for all of them
        ;; (cc-ide parity, claude-code-ide.el:987-989).
        (when pi-mode-focus-on-open
          (select-window window))
        ;; Refresh MRU on every display: a restore path (`w', `a', `W')
        ;; must make the shown session the most-recently-used target
        ;; (cc-ide parity, claude-code-ide.el:979-985).
        (when-let ((session (pi-mode--session-by-buffer buffer)))
          (setf (pi-mode-session-last-used session) (current-time))))
      window)))

(defconst pi-mode--window-slot-block 16
  "Side-window slots reserved per project.
Slots order windows along a frame side; reserving a contiguous block
per project keeps one project's instances grouped together instead of
interleaved by global creation order (cc-ide parity,
claude-code-ide.el:846-893).")

(defun pi-mode--assign-window-slot (root)
  "Return a side-window slot for a new session of project ROOT.
Each project owns a block of `pi-mode--window-slot-block' slots, so
its windows sort together rather than by creation order.  Within the
block the smallest slot not used by any live session is picked (the
global check also keeps legacy out-of-block slots collision-free); a
block exhausted beyond any plausible session count falls back to a
slot past every live one rather than colliding."
  (let* ((block pi-mode--window-slot-block)
         (siblings (cl-remove-if-not #'pi-mode-session-window-slot
                                     (pi-mode--project-sessions root)))
         (all-slots (cl-remove nil
                               (mapcar #'pi-mode-session-window-slot
                                       (pi-mode--active-sessions))))
         (base (if siblings
                   ;; Join the project's existing block
                   (* block (floor (pi-mode-session-window-slot
                                    (car siblings))
                                   block))
                 ;; New project: smallest block no live session occupies
                 (let ((b 0))
                   (while (memq (* b block) all-slots)
                     (cl-incf b))
                   (* b block)))))
    (let ((slot base))
      (while (and (< slot (+ base block))
                  (memq slot all-slots))
        (cl-incf slot))
      (if (< slot (+ base block))
          slot
        ;; Block exhausted: overflow past every live slot rather than
        ;; spilling into a neighboring project's block.
        (1+ (if all-slots (apply #'max all-slots) -1))))))

(defun pi-mode--visible-pi-window-mru ()
  "Return the visible pi side window showing the most recently used session.
Searches the selected frame's windows on `pi-mode-window-side' whose
buffer hosts a session; a session with no MRU stamp counts as the
oldest.  Return nil when no pi window is visible."
  (let ((candidates
         (cl-remove-if-not
          (lambda (window)
            (and (eq (window-parameter window 'window-side) pi-mode-window-side)
                 (pi-mode--session-by-buffer (window-buffer window))))
          (window-list))))
    (car (cl-sort candidates
                  (lambda (w1 w2)
                    (time-less-p
                     (or (pi-mode-session-last-used
                          (pi-mode--session-by-buffer (window-buffer w2)))
                         0)
                     (or (pi-mode-session-last-used
                          (pi-mode--session-by-buffer (window-buffer w1)))
                         0)))))))

(defun pi-mode--launch-window-slot (session)
  "Return the side-window slot for displaying SESSION at launch.
When a pi side window is visible, take over the slot of the window
showing the most recently used session: the new session replaces the
visible pi window (claude-code-ide's pre-slot single-window behavior)
instead of stacking a new one below it.  The displaced session is
re-homed to a fresh slot in its own project block, so restoring or
showing it later opens side by side rather than evicting the new
session.  Without a visible pi window, fall back to a fresh slot via
`pi-mode--assign-window-slot'."
  (if-let* ((window (pi-mode--visible-pi-window-mru)))
      (let ((displaced (pi-mode--session-by-buffer (window-buffer window))))
        (when displaced
          (setf (pi-mode-session-window-slot displaced)
                (pi-mode--assign-window-slot
                 (pi-mode-session-project-root displaced))))
        (or (window-parameter window 'window-slot) 0))
    (pi-mode--assign-window-slot (pi-mode-session-project-root session))))

;; Pi buffers dock in a side window; the action function reads the
;; window customization at display time, so changing the defcustoms
;; takes effect immediately.  The entry must be a proper list
;; (CONDITION FUNCTIONS...) where CONDITION is the session-buffer
;; predicate `pi-mode--session-buffer-p' — a dotted
;; (CONDITION . FUNCTION) form yields a bare symbol action, which
;; `display-buffer' cannot unwrap.
(add-to-list 'display-buffer-alist
             (list #'pi-mode--session-buffer-p #'pi-mode--display-buffer))

(defun pi-mode--current-tab-key ()
  "Tab-bar tab name for the current tab, or \"none\".
Tab-bar-mode must be enabled: `tab-bar--current-tab' is callable even
when it is not (tab-bar.el is preloaded in Emacs 30) and then returns
a buffer-dependent synthetic tab name, which would fragment the panel
state for users without tab-bar-mode."
  (or (and (bound-and-true-p tab-bar-mode)
           (fboundp 'tab-bar--current-tab)
           (alist-get 'name (tab-bar--current-tab)))
      "none"))

(defun pi-mode--hidden-panel-get (&optional root)
  "Hidden session set for the current tab and ROOT, or nil.
ROOT defaults to the current project (`pi-mode--project-root'); the
symbol `:all' addresses the whole-tab set used by
`pi-mode-toggle-recent'."
  (cdr (assoc (cons (pi-mode--current-tab-key) (or root (pi-mode--project-root)))
              (frame-parameter nil 'pi-mode-hidden-panel))))

(defun pi-mode--hidden-panel-set (sessions &optional root)
  "Remember SESSIONS as the current tab's hidden set for ROOT.
ROOT defaults to the current project (`pi-mode--project-root'); the
symbol `:all' addresses the whole-tab set used by
`pi-mode-toggle-recent'.
A nil SESSIONS drops the entry.  Entries for tabs that no longer
exist are pruned on the way; pruning needs `tab-bar-mode', without
which every buffer reports a synthetic tab and no real tabs exist.
Legacy string-keyed entries (from before the (tab . project)
keying) never match a cons key and are dropped unconditionally."
  (let* ((root (or root (pi-mode--project-root)))
         (key (cons (pi-mode--current-tab-key) root))
         (live-tabs (and (bound-and-true-p tab-bar-mode)
                         (fboundp 'tab-bar-tabs)
                         (mapcar (lambda (tab) (alist-get 'name (cdr tab)))
                                 (tab-bar-tabs))))
         (rest (cl-remove-if (lambda (entry)
                               (or (equal (car entry) key)
                                   ;; Legacy string-keyed entries never match
                                   ;; a cons key; drop them and keep `caar'
                                   ;; off strings (it would signal).
                                   (not (consp (car entry)))
                                   (and live-tabs
                                        (not (member (caar entry) live-tabs)))))
                             (frame-parameter nil 'pi-mode-hidden-panel))))
    (set-frame-parameter nil 'pi-mode-hidden-panel
                         (if sessions
                             (cons (cons key sessions) rest)
                           rest))))

(defun pi-mode--hide-session-windows (&optional root)
  "Delete the windows showing pi sessions of project ROOT.
ROOT nil hides every pi window in the selected frame's tab."
  (dolist (win (window-list))
    (let ((session (or (pi-mode--session-by-buffer (window-buffer win))
                       (buffer-local-value 'pi-mode--session (window-buffer win)))))
      (when (and session
                 (or (null root)
                     (equal (pi-mode-session-project-root session) root)))
        (ignore-errors (delete-window win))))))

;;;###autoload
(defun pi-mode-toggle-panel ()
  "Hide or restore the pi side windows in the current tab.
Only sessions of the current project (`pi-mode--project-root') are
considered.  When any of their windows is visible, hide them all and
remember the set for this tab; otherwise restore the remembered set,
skipping dead and foreign sessions, falling back to the current
project's most recently used session."
  (interactive)
  (let* ((root (pi-mode--project-root))
         (sessions (pi-mode--project-sessions root))
         (visible (pi-mode--visible-sessions sessions)))
    (unless sessions
      (user-error "No running pi sessions in project %s; start one with `pi-mode-start'" root))
    (if visible
        (progn
          (pi-mode--hidden-panel-set visible)
          (pi-mode--hide-session-windows root)
          (message "pi panel hidden")
          :hidden)
      (let* ((hidden (pi-mode--hidden-panel-get))
             (restore (or (cl-remove-if-not
                           (lambda (s)
                             (and (pi-mode--session-live-p s)
                                  (equal (pi-mode-session-project-root s)
                                         root)))
                           hidden)
                          (list (car sessions)))))
        (pi-mode--hidden-panel-set nil)
        (dolist (session restore)
          (display-buffer (pi-mode-session-buffer session)))
        (message "pi panel shown")
        :shown))))

(defun pi-mode--strip-new-tab-pi-windows (&rest _)
  "Remove cloned pi side windows from a freshly created tab.
With `tab-bar-new-tab-choice' t a new tab clones the previous tab's
layout, duplicating session windows across tabs.  New tabs start
pi-free instead; summon sessions there explicitly."
  (dolist (window (window-list))
    (when (and (window-live-p window)
               (pi-mode--session-buffer-p (window-buffer window)))
      (unless (ignore-errors (delete-window window) t)
        ;; The sole window of the tab cannot be deleted — show another
        ;; buffer in it instead.  Pi windows are dedicated, and
        ;; `switch-to-prev-buffer' and `set-window-buffer' signal on
        ;; dedicated windows, so clear the dedication first.
        (when (window-live-p window)
          (set-window-dedicated-p window nil)
          (switch-to-prev-buffer window 'bury)
          (when (pi-mode--session-buffer-p (window-buffer window))
            (set-window-buffer window (get-buffer-create "*scratch*"))))))))

(defun pi-mode--note-window-selection (frame)
  "Stamp MRU state when a pi window gets selected in FRAME.
Without this, clicking into a visible session would not make it the
most recently used one for target resolution."
  (when-let* ((window (frame-selected-window frame))
              (session (pi-mode--session-by-buffer (window-buffer window))))
    (unless (pi-mode-session-cleanup-done session)
      (setf (pi-mode-session-last-used session) (current-time)))))

;; A plain add-hook is void-safe (add-hook on a void variable first sets
;; it to nil): on Emacs 28/29, where tab-bar.el is not preloaded, this
;; installs before tab-bar loads, and tab-bar's later defcustom preserves
;; the existing value.  `with-eval-after-load' would trip package-lint.
(add-hook 'tab-bar-tab-post-open-functions #'pi-mode--strip-new-tab-pi-windows)
(add-hook 'window-selection-change-functions #'pi-mode--note-window-selection)

;;;###autoload
(defun pi-mode-show-all (&optional all-projects)
  "Show every pi session of the current project.
Already visible sessions are left in place.  With prefix argument
ALL-PROJECTS, show the sessions of all projects."
  (interactive "P")
  (let* ((root (pi-mode--project-root))
         (sessions (if all-projects
                       (pi-mode--active-sessions)
                     (pi-mode--project-sessions root)))
         (shown 0))
    (if (null sessions)
        (if all-projects
            (user-error "No running pi sessions")
          (user-error "No running pi sessions in project %s" root))
      (dolist (s sessions)
        (unless (pi-mode--visible-sessions (list s))
          (display-buffer (pi-mode-session-buffer s))
          (cl-incf shown)))
      (message (if (zerop shown)
                   "All pi sessions already visible"
                 (format "Showing %d pi session%s"
                         shown (if (> shown 1) "s" "")))))))

;;;###autoload
(defun pi-mode-toggle-recent ()
  "Toggle the visibility of all pi windows in the current tab.
When any pi window is visible, hide them all and remember the set
for this tab; otherwise restore the remembered set (skipping
stopped sessions), falling back to the most recently used session."
  (interactive)
  (let* ((all (pi-mode--active-sessions))
         (visible (pi-mode--visible-sessions all)))
    (unless all
      (user-error "No running pi sessions"))
    (cond
     (visible
      (pi-mode--hidden-panel-set visible :all)
      (pi-mode--hide-session-windows)
      (message "Closed all pi windows"))
     ((cl-remove-if-not #'pi-mode--session-live-p
                        (pi-mode--hidden-panel-get :all))
      (let ((restore (cl-remove-if-not #'pi-mode--session-live-p
                                       (pi-mode--hidden-panel-get :all))))
        (pi-mode--hidden-panel-set nil :all)
        (dolist (session restore)
          (display-buffer (pi-mode-session-buffer session)))
        (message "Restored %d pi window%s"
                 (length restore) (if (cdr restore) "s" ""))))
     (t
      (let ((session (car all)))
        (display-buffer (pi-mode-session-buffer session))
        (message "Opened most recent pi session"))))))

(defun pi-mode-show-debug ()
  "Show the pi-mode debug buffer."
  (interactive)
  (display-buffer "*pi-mode-debug*"))

(defun pi-mode-toggle-debug ()
  "Toggle `pi-mode-debug'."
  (interactive)
  (setq pi-mode-debug (not pi-mode-debug))
  (message "pi-mode debug %s" (if pi-mode-debug "on" "off")))

(defun pi-mode--clear-debug-log ()
  "Clear the *pi-mode-debug* buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*pi-mode-debug*")
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (message "pi-mode debug log cleared"))

;;;###autoload
(defun pi-mode-install-keybindings (&optional _force)
  "Compatibility shim for the removed pi-side keybinding installer.
pi-mode no longer writes pi's keybindings.json: ghostel semi-char mode
delivers nearly every key to pi, and `C-c C-q' sends any intercepted
key literally.  This stub exists so stale `use-package' `:config'
blocks calling `pi-mode-install-keybindings' do not error; delete the
call from your configuration."
  (interactive "P")
  (message "pi-mode: pi-side keybinding installation was removed; drop (pi-mode-install-keybindings) from your config"))

(make-obsolete 'pi-mode-install-keybindings 'ignore "0.1.0")

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
            (cl-remove "--tui-mode"
                       (cl-remove "fullscreen"
                                  (cl-remove "regular" pi-mode-cli-args :test #'equal)
                                  :test #'equal)
                       :test #'equal))
      ;; order matters: push the VALUE first, then the flag, so the list
      ;; reads ("--tui-mode" next ...)
      (push next pi-mode-cli-args)
      (push "--tui-mode" pi-mode-cli-args)
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

;; Emacs 28.1's autoload machinery does not process cookies on
;; `transient-define-prefix' or `define-key', so provide an explicit
;; autoload for the menu.  The global C-c C-' binding itself still
;; requires pi-mode.el to be loaded first (see README).
(autoload 'pi-mode-menu "pi-mode-menu" "Command menu for pi-mode sessions." t)

;;; Ghostel configuration (global — see design spec section 4.2)

(add-to-list 'ghostel-keymap-exceptions "C-g")
;; Rebuild the semi-char keymap so the change takes effect now; the
;; defcustom :set normally does this only via customize.
(when (fboundp 'ghostel--rebuild-semi-char-keymap)
  (ghostel--rebuild-semi-char-keymap))

(provide 'pi-mode)

;; Require after `provide' so that pi-mode-session.el's own
;; `(require 'pi-mode)' resolves via `featurep' — a require before the
;; provide would re-enter pi-mode.el mid-load ("Recursive load").
(require 'pi-mode-session)
;; Same reasoning: pi-mode-notifications.el requires both pi-mode and
;; pi-mode-session.
(require 'pi-mode-notifications)
;; Same reasoning: pi-mode-menu.el requires both pi-mode and
;; pi-mode-session.
(require 'pi-mode-menu)

;;; pi-mode.el ends here
