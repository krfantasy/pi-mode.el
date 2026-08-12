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
;; detection, prompt-input region sending, session management, and a
;; transient command menu.  Design spec:
;; docs/superpowers/specs/2026-08-11-pi-mode-design.md

;;; Code:

(require 'cl-lib)
(require 'project)          ; project-root is not autoloaded
(require 'ghostel)
(require 'transient)

(defgroup pi nil
  "Interface for the pi coding agent."
  :group 'tools
  :prefix "pi-mode-")

(defcustom pi-mode-debug t
  "When non-nil, log pi-mode activity to the *pi-mode-debug* buffer."
  :type 'boolean
  :group 'pi)

(defvar pi-mode-map
  (let ((map (make-sparse-keymap)))
    map)
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

(defcustom pi-mode-cli-args '("--tui-mode" "regular")
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
  (let* ((base (pi-mode--session-base-name project-root name))
         (buffer-name (pi-mode--unique-buffer-name base)))
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
          (setf (pi-mode-session-process session) process)
          ;; ghostel-exec activates ghostel-mode, whose
          ;; kill-all-local-variables wipes the session locals; re-apply
          ;; them so the display-buffer predicate, kill-buffer guard and
          ;; mode-line segment keep working.
          (with-current-buffer buffer
            (pi-mode--setup-session-buffer session))
          (setf (pi-mode-session-window-slot session) (pi-mode--assign-window-slot))
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
Rules: in-buffer self; sole; sole-visible; else MRU with echo.
The resolved session's `last-used' is updated (MRU semantics)."
  (let* ((sessions (pi-mode--active-sessions))
         (session
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

;;; Region and file sending

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

(defun pi-mode--display-args (buffer)
  "Return (SIDE SLOT SIZE-KEY SIZE-VALUE) for displaying BUFFER.
SIDE is `pi-mode-window-side'; SLOT is the session's `window-slot'
(or 0); SIZE-KEY is `window-width' on left/right sides and
`window-height' otherwise, with the matching defcustom value.
Reading the customization at display time keeps changes live without
re-adding a `display-buffer-alist' entry."
  (let* ((side pi-mode-window-side)
         (slot (or (when-let ((session (pi-mode--session-by-buffer buffer)))
                     (pi-mode-session-window-slot session))
                   0))
         (left-or-right (memq side '(left right))))
    (list side slot
          (if left-or-right 'window-width 'window-height)
          (if left-or-right pi-mode-window-width pi-mode-window-height))))

(defun pi-mode--display-buffer (buffer _alist)
  "Display BUFFER in a side window per `pi-mode-window-side' and size.
Each pi session occupies its own slot, so several sessions can be
visible side by side instead of evicting each other."
  (let* ((args (pi-mode--display-args buffer))
         (side (nth 0 args))
         (slot (nth 1 args))
         (size-key (nth 2 args))
         (size-value (nth 3 args)))
    (let ((display-buffer-alist
           `((,(regexp-quote (buffer-name buffer))
              (display-buffer-in-side-window)
              (side . ,side)
              (slot . ,slot)
              (,size-key . ,size-value)))))
      (display-buffer buffer))))

(defun pi-mode--assign-window-slot ()
  "Return the smallest side-window slot not used by a live session."
  (let ((used (cl-loop for s in (pi-mode--active-sessions)
                       for slot = (pi-mode-session-window-slot s)
                       when slot collect slot))
        (slot 0))
    (while (memq slot used)
      (cl-incf slot))
    slot))

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

(defun pi-mode--hidden-panel-get ()
  "Hidden session set for the current tab, or nil."
  (cdr (assoc (pi-mode--current-tab-key)
              (frame-parameter nil 'pi-mode-hidden-panel))))

(defun pi-mode--hidden-panel-set (sessions)
  "Remember SESSIONS as the current tab's hidden set.
A nil SESSIONS drops the entry.  Entries for tabs that no longer
exist are pruned on the way."
  (let* ((key (pi-mode--current-tab-key))
         (live-tabs (and (fboundp 'tab-bar-tabs)
                         (mapcar (lambda (tab) (alist-get 'name (cdr tab)))
                                 (tab-bar-tabs))))
         (rest (cl-remove-if (lambda (entry)
                               (or (equal (car entry) key)
                                   (and live-tabs
                                        (not (member (car entry) live-tabs)))))
                             (frame-parameter nil 'pi-mode-hidden-panel))))
    (set-frame-parameter nil 'pi-mode-hidden-panel
                         (if sessions
                             (cons (cons key sessions) rest)
                           rest))))

;;;###autoload
(defun pi-mode-toggle-panel ()
  "Hide or restore the pi side windows in the current tab.
When any pi session window is visible, hide them all and remember
the set for this tab; otherwise restore the remembered set (skipping
dead sessions), falling back to the most recently used session.
With no live sessions, reports the panel as hidden (legacy
contract, preserved for `pi-mode-test-window-commands-no-error')."
  (interactive)
  (let* ((sessions (pi-mode--active-sessions))
         (visible (pi-mode--visible-sessions sessions)))
    (if (null sessions)
        (progn (message "pi panel hidden") :hidden)
      (if visible
          (progn
            (pi-mode--hidden-panel-set visible)
            (dolist (win (window-list))
              (when (pi-mode--session-buffer-p (window-buffer win))
                (ignore-errors (delete-window win))))
            (message "pi panel hidden")
            :hidden)
        (let* ((hidden (pi-mode--hidden-panel-get))
               (restore (or (cl-remove-if-not #'pi-mode--session-live-p hidden)
                            (let ((sessions (pi-mode--active-sessions)))
                              (and sessions (list (pi-mode--mru-session sessions)))))))
          (pi-mode--hidden-panel-set nil)
          (dolist (session restore)
            (display-buffer (pi-mode-session-buffer session)))
          (message "pi panel shown")
          :shown)))))

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
        ;; buffer in it instead.
        (when (window-live-p window)
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
    (let ((session (pi-mode--mru-session sessions)))
      (setf (pi-mode-session-last-used session) (current-time))
      (display-buffer (pi-mode-session-buffer session)))))

(defun pi-mode-show-debug ()
  "Show the pi-mode debug buffer."
  (interactive)
  (display-buffer "*pi-mode-debug*"))

(defun pi-mode-toggle-debug ()
  "Toggle `pi-mode-debug'."
  (interactive)
  (setq pi-mode-debug (not pi-mode-debug))
  (message "pi-mode debug %s" (if pi-mode-debug "on" "off")))

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

(make-obsolete 'pi-mode-install-keybindings 'ignore "0.2.0")

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
;; Same reasoning: pi-mode-menu.el requires both pi-mode and
;; pi-mode-session.
(require 'pi-mode-menu)

;;; pi-mode.el ends here
