;;; pi-mode-e2e.el --- End-to-end tests: real ghostel + real pi -*- lexical-binding: t; -*-

;;; Commentary:
;; Full-stack e2e tests: pi-mode drives a REAL pi process in a REAL
;; ghostel terminal inside batch Emacs.  pi is pointed at the e2e model
;; server (`pi-mode-e2e-server.el', an OpenAI-compatible HTTP endpoint)
;; via an isolated temp agent dir, so every test exercises the complete
;; loop Emacs -> ghostel -> pi -> model HTTP -> pi -> ghostel -> Emacs
;; with deterministic, offline assertions.
;;
;; Requirements: real ghostel installed via package.el (native module
;; included), real `pi' on exec-path.  Run with `make e2e' (never with
;; tests/stubs on the load path).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'project)
(require 'pi-mode)
(require 'pi-mode-e2e-server)

;;; Harness

(defvar pi-mode-e2e--agent-dirs nil
  "Temp agent dirs created by the harness, cleaned up at teardown.")

(defun pi-mode-e2e--wait (proc buffer predicate &optional timeout)
  "Poll PREDICATE until true or TIMEOUT seconds pass.
Evaluates PREDICATE in BUFFER while it is live; once the buffer dies,
evaluates it without a buffer so the death itself can be awaited.
Pumps PROCESS output and ghostel's render timer; on timeout prints the
buffer tail for diagnosis and returns nil."
  (let ((deadline (+ (float-time) (or timeout 40))))
    (catch 'done
      (while (< (float-time) deadline)
        (when (if (buffer-live-p buffer)
                  (with-current-buffer buffer (funcall predicate))
                (ignore-errors (funcall predicate)))
          (throw 'done t))
        (accept-process-output proc 0.4)
        (sleep-for 0.05))
      (when (buffer-live-p buffer)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (princ (format "E2E TIMEOUT; buffer (%d chars): %S\n" (length text) text))
          ;; Probe: ask pi to write its debug log (/debug is handled in
          ;; the editor submit path, so the probe also tells whether key
          ;; delivery works at all).
          (ignore-errors
            (with-current-buffer buffer
              (ghostel-paste-string "/debug")
              (ghostel-send-key "return")))
          (sleep-for 2)
          (when (buffer-live-p buffer)
            (let ((text2 (buffer-substring-no-properties (point-min) (point-max))))
              (princ (format "E2E TIMEOUT; buffer after /debug (%d chars): %S\n"
                             (length text2) text2))))
          ;; Diagnostic extras: pi's debug log and the model server's
          ;; request record tell apart key-delivery, HTTP and crash
          ;; failures on CI.
          (let ((dbg (and (getenv "PI_CODING_AGENT_DIR")
                          (expand-file-name "pi-debug.log"
                                            (getenv "PI_CODING_AGENT_DIR")))))
            (when (and dbg (file-exists-p dbg))
              (princ (format "E2E TIMEOUT; pi-debug.log: %S\n"
                             (with-temp-buffer
                               (insert-file-contents dbg)
                               (buffer-string))))))
          (when (boundp 'pi-mode-e2e-server-requests)
            (princ (format "E2E TIMEOUT; requests: %S\n"
                           pi-mode-e2e-server-requests))
            (princ (format "E2E TIMEOUT; responses: %S\n"
                           pi-mode-e2e-server-responses)))
          (when (boundp 'pi-mode-e2e-server-process)
            (princ (format "E2E TIMEOUT; server alive: %S\n"
                           (and pi-mode-e2e-server-process
                                (process-live-p pi-mode-e2e-server-process)))))
          (princ (format "E2E TIMEOUT; pi process: %S\n"
                         (list :live (process-live-p proc)
                               :status (process-status proc)
                               :command (process-command proc))))))
      nil)))

(defun pi-mode-e2e--buffer-text (buffer)
  "Return the plain text of BUFFER."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun pi-mode-e2e--agent-dir ()
  "Create an isolated agent dir with models.json pointing at the server."
  (let ((dir (make-temp-file "pi-e2e-agent-" t)))
    (push dir pi-mode-e2e--agent-dirs)
    (write-region (pi-mode-e2e-server-models-json) nil
                  (expand-file-name "models.json" dir))
    dir))

(defun pi-mode-e2e--launch (project-root &optional args)
  "Launch a real pi session in PROJECT-ROOT with an isolated agent dir.
Returns (SESSION BUFFER PROCESS AGENT-DIR)."
  (setenv "PI_CODING_AGENT_DIR" (pi-mode-e2e--agent-dir))
  (let ((session (pi-mode--launch-buffer
                  project-root (or args '("--tui-mode" "regular")))))
    (list session (pi-mode-session-buffer session)
          (pi-mode-session-process session)
          (getenv "PI_CODING_AGENT_DIR"))))

(defun pi-mode-e2e--teardown (session buffer process)
  "Kill PROCESS, BUFFER and all temp agent dirs."
  (ignore-errors
    (when (and session (process-live-p (pi-mode-session-process session)))
      (delete-process (pi-mode-session-process session))))
  (ignore-errors (when (and process (process-live-p process)) (delete-process process)))
  (ignore-errors (when (and buffer (buffer-live-p buffer)) (kill-buffer buffer)))
  (dolist (dir pi-mode-e2e--agent-dirs)
    (ignore-errors (delete-directory dir t)))
  (setq pi-mode-e2e--agent-dirs nil))

(defun pi-mode-e2e--wait-ready (proc buffer project-root)
  "Wait until pi's TUI rendered AND startup finished.
The status bar must show PROJECT-ROOT and the model, and no startup
status (managed fd/rg download, startup-submit feedback) may remain:
pi installs its real submit handler only after that, so an early
submit would be swallowed by the startup handler and wait-reply would
time out."
  (pi-mode-e2e--wait proc buffer
    (lambda ()
      (let* ((text (pi-mode-e2e--buffer-text buffer))
             ;; pi's footer abbreviates $HOME-relative cwd to "~/...",
             ;; e.g. ~/work/_temp/... on GitHub runners.
             (root (abbreviate-file-name project-root)))
        (and (> (length text) 300)
             (string-match-p (regexp-quote root) text)
             (string-match-p "e2e-model" text)
             (not (string-match-p "Startup is still in progress" text))
             (not (string-match-p "not found. Downloading" text)))))
    60))

(defun pi-mode-e2e--wait-input (proc buffer text)
  "Wait until TEXT is visible in the input box area of pi's TUI."
  (pi-mode-e2e--wait proc buffer
    (lambda () (string-match-p (regexp-quote text)
                               (pi-mode-e2e--buffer-text buffer)))
    15))

(defun pi-mode-e2e--wait-reply (proc buffer)
  "Wait until the canned E2E assistant reply is rendered."
  (pi-mode-e2e--wait proc buffer
    (lambda () (string-match-p "E2E-REPLY" (pi-mode-e2e--buffer-text buffer)))
    40))

(defun pi-mode-e2e--session-jsonl (agent-dir)
  "Return the session .jsonl files under AGENT-DIR (deep search)."
  (directory-files-recursively agent-dir "\\.jsonl\\'" nil t))

(defun pi-mode-e2e--wait-jsonl (proc agent-dir predicate &optional timeout)
  "Poll the session JSONL under AGENT-DIR until PREDICATE holds for any file."
  (let ((deadline (+ (float-time) (or timeout 25))))
    (catch 'done
      (while (< (float-time) deadline)
        (let ((files (pi-mode-e2e--session-jsonl agent-dir))
              (matched nil))
          (dolist (file files)
            (let ((content (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))))
              (when (funcall predicate content)
                (setq matched t))))
          (when matched (throw 'done t)))
        (accept-process-output proc 0.4)
        (sleep-for 0.05))
      nil)))

(defmacro pi-mode-e2e--with-session (bindings project-root &rest body)
  "Launch a real pi session in PROJECT-ROOT and run BODY with the
variables in BINDINGS (SESSION BUFFER PROCESS AGENT-DIR) bound; always
tears down."
  (declare (indent 2))
  (let ((session (nth 0 bindings))
        (buffer (nth 1 bindings))
        (process (nth 2 bindings))
        (agent-dir (nth 3 bindings)))
    `(let* ((launched (pi-mode-e2e--launch ,project-root))
            (,session (nth 0 launched))
            (,buffer (nth 1 launched))
            (,process (nth 2 launched))
            (,agent-dir (nth 3 launched)))
       (unwind-protect
           (progn ,@body)
         (pi-mode-e2e--teardown ,session ,buffer ,process)))))

;;; Tests

(ert-deftest pi-mode-e2e-test-launch-and-cwd ()
  "pi-mode-start launches real pi; the session runs in the project root."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t)))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process _agent-dir) project-root
          (should (process-live-p process))
          (should (eq (pi-mode--session-by-buffer buffer) session))
          ;; The session buffer owns the project root (cwd regression).
          (should (equal (with-current-buffer buffer default-directory)
                         project-root))
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          ;; The TUI status bar shows the project dir (home-relative), not
          ;; the caller's cwd.
          (should (string-match-p (regexp-quote (abbreviate-file-name project-root))
                                  (pi-mode-e2e--buffer-text buffer)))
          ;; The isolated agent dir was honored: models.json provider loaded.
          (should (string-match-p "e2e-model"
                                  (pi-mode-e2e--buffer-text buffer))))
      (delete-directory project-root t))))

(ert-deftest pi-mode-e2e-test-send-region-inserts-into-input ()
  "send-region pastes the region into pi's input box without submitting."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t))
        (marker "e2e-region-marker-42"))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (let ((requests-before (length pi-mode-e2e-server-requests)))
            (with-temp-buffer
              (insert (format "line1\n%s\nline3" marker))
              (pi-mode-send-region (point-min) (point-max)))
            (should (pi-mode-e2e--wait-input process buffer marker))
            ;; Insert-only: nothing was submitted, so no model request.
            (should (= (length pi-mode-e2e-server-requests) requests-before))
            (should-not (string-match-p "E2E-REPLY" (pi-mode-e2e--buffer-text buffer)))
            (should-not (pi-mode-e2e--session-jsonl agent-dir))))
      (delete-directory project-root t))))

(ert-deftest pi-mode-e2e-test-submit-roundtrip ()
  "Submitting sends the text through ghostel to real pi, which calls the
e2e model endpoint and records the round in the session JSONL."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t))
        (marker "e2e-submit-marker-99"))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (with-temp-buffer
            (insert marker)
            (pi-mode-send-region (point-min) (point-max)))
          (should (pi-mode-e2e--wait-input process buffer marker))
          (with-current-buffer buffer
            (ghostel-send-key "return"))
          ;; Real model HTTP roundtrip: canned reply rendered by pi.
          (should (pi-mode-e2e--wait-reply process buffer))
          ;; The model endpoint received the submitted text.
          (let ((chat (cl-find-if
                       (lambda (r) (string-match-p "chat/completions" (nth 1 r)))
                       pi-mode-e2e-server-requests)))
            (should chat)
            (should (string-match-p marker (nth 3 chat))))
          ;; pi recorded the round in the session JSONL (user + assistant).
          (should (pi-mode-e2e--wait-jsonl
                   process agent-dir
                   (lambda (content)
                     (and (string-match-p marker content)
                          (string-match-p "E2E-REPLY" content))))))
      (delete-directory project-root t))))

(ert-deftest pi-mode-e2e-test-send-file-inserts-reference ()
  "send-file inserts @rel/path (relative to the pi session cwd)."
  (pi-mode-e2e-server-start)
  (let* ((project-root (make-temp-file "pi-e2e-proj-" t))
         (file (expand-file-name "sub/deep/thing.txt" project-root))
         (marker (concat "@sub/deep/thing.txt")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (write-region "content" nil file)
          (pi-mode-e2e--with-session (session buffer process _agent-dir) project-root
            (should (pi-mode-e2e--wait-ready process buffer project-root))
            (with-temp-buffer
              (setq buffer-file-name file)
              (pi-mode-send-file))
            (should (pi-mode-e2e--wait-input process buffer marker))
            (should-not (string-match-p "E2E-REPLY" (pi-mode-e2e--buffer-text buffer)))))
      (delete-directory project-root t))))

(ert-deftest pi-mode-e2e-test-interrupt-keeps-session-alive ()
  "pi-mode-interrupt sends escape; the pi session stays alive."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t)))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process _agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (pi-mode-interrupt)
          (should (process-live-p process))
          ;; The TUI keeps rendering after the interrupt.
          (should (pi-mode-e2e--wait process buffer
                    (lambda ()
                      (> (length (pi-mode-e2e--buffer-text buffer)) 300))
                    10)))
      (delete-directory project-root t))))

(ert-deftest pi-mode-e2e-test-exit-kills-buffer ()
  "Exiting pi (ctrl+d on empty input) ends the process AND kills the buffer."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t)))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process _agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (with-current-buffer buffer
            (ghostel-send-key "d" "ctrl"))
          ;; Real exit: process dies, cleanup unregisters and kills the
          ;; buffer (claude-code-ide behavior, pi-mode-kill-buffer-on-exit).
          (should (pi-mode-e2e--wait process buffer
                    (lambda ()
                      (or (not (process-live-p process))
                          (not (buffer-live-p buffer))))
                    20))
          (should-not (process-live-p process))
          (should-not (buffer-live-p buffer))
          (should-not (pi-mode--session-by-buffer buffer)))
      (delete-directory project-root t))))

(ert-deftest pi-mode-e2e-test-window-side-and-panel ()
  "Pi buffers dock in a side window on `pi-mode-window-side'; toggle-panel
hides and restores them."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t)))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process _agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (let ((win (get-buffer-window buffer)))
            (should win)
            (should (eq (window-parameter win 'window-side)
                        pi-mode-window-side)))
          (cl-letf (((symbol-function 'pi-mode--project-root)
                     (lambda () project-root)))
            (should (eq (pi-mode-toggle-panel) :hidden))
            (should-not (get-buffer-window buffer))
            (should (eq (pi-mode-toggle-panel) :shown)))
          (let ((win (get-buffer-window buffer)))
            (should win)
            (should (eq (window-parameter win 'window-side)
                        pi-mode-window-side))))
      (delete-directory project-root t))))

(ert-deftest pi-mode-e2e-test-configure-model ()
  "pi-mode-configure-model sends /model; pi reports an unknown model."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t))
        (bogus "e2e-no-such-model"))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process _agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (pi-mode-configure-model bogus)
          ;; Real pi processes the /model command and renders feedback
          ;; about the unknown model in the TUI.
          (should (pi-mode-e2e--wait process buffer
                    (lambda ()
                      (string-match-p
                       "not found\\|unknown\\|e2e-no-such-model"
                       (pi-mode-e2e--buffer-text buffer)))
                    25))
          (should (process-live-p process)))
      (delete-directory project-root t))))

(ert-deftest pi-mode-e2e-test-session-continue-and-stop ()
  "continue spawns a second real pi (-c); stop kills it and unregisters."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t)))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process _agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (let ((second
                 (cl-letf (((symbol-function 'pi-mode--project-root)
                            (lambda () project-root))
                           ;; the project already has one instance, so
                           ;; continue prompts for a name; auto-name it
                           ((symbol-function 'pi-mode--read-instance-name)
                            (lambda (&optional _root) nil)))
                   (pi-mode-session-continue))))
            (unwind-protect
                (progn
                  (should second)
                  (should (process-live-p (pi-mode-session-process second)))
                  (should-not (eq (pi-mode-session-buffer second) buffer))
                  (let ((b2 (pi-mode-session-buffer second))
                        (p2 (pi-mode-session-process second)))
                    (should (pi-mode-e2e--wait-ready p2 b2 project-root))
                    ;; stop from the second session's buffer: the target
                    ;; is unambiguous; skip the confirm prompt (covered by
                    ;; the unit suite) so batch mode does not read stdin
                    (let ((pi-mode-confirm-quit nil))
                      (with-current-buffer b2
                        (pi-mode-session-stop)))
                    (pi-mode-e2e--wait p2 b2
                      (lambda () (not (process-live-p p2)))
                      15)
                    (should-not (process-live-p p2))
                    (should-not (pi-mode--session-by-buffer b2))))
              (ignore-errors
                (when (process-live-p (pi-mode-session-process second))
                  (delete-process (pi-mode-session-process second))))
              (ignore-errors (kill-buffer (pi-mode-session-buffer second))))))
      (delete-directory project-root t))))

;;; Prompt editing e2e

(defun pi-mode-e2e--prompt-edit-popup (session)
  "The prompt-edit popup buffer for SESSION."
  (get-buffer (format "*pi prompt %s*" (pi-mode-session-id session))))

(ert-deftest pi-mode-e2e-test-edit-prompt-roundtrip ()
  "edit-prompt captures pi's input box into a markdown popup; C-c C-c
syncs the edited text back into pi's input box without submitting."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t))
        (marker "e2e-edit-prompt-marker"))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (with-temp-buffer
            (insert (format "line1 %s\nline2" marker))
            (pi-mode-send-region (point-min) (point-max)))
          (should (pi-mode-e2e--wait-input process buffer marker))
          (with-current-buffer buffer
            (pi-mode-edit-prompt))
          (let ((popup (pi-mode-e2e--prompt-edit-popup session)))
            (should popup)
            (should (equal (with-current-buffer popup (buffer-string))
                           (format "line1 %s\nline2" marker)))
            (should (with-current-buffer popup pi-mode-prompt-edit-mode))
            (with-current-buffer popup
              (goto-char (point-max))
              (insert " EDITED")
              (pi-mode-prompt-edit-submit))
            ;; The popup closed and the edited prompt is in pi's input box.
            (should-not (buffer-live-p popup))
            (should (pi-mode-e2e--wait-input process buffer "EDITED"))
            (should (pi-mode-e2e--wait-input process buffer marker))
            ;; Nothing was submitted to the model.
            (should-not (pi-mode-e2e--session-jsonl agent-dir))))
      (delete-directory project-root t))))

(ert-deftest pi-mode-e2e-test-edit-prompt-long-line-wrap ()
  "A prompt line wider than the editor wraps on screen; extraction
rejoins the wrap so the popup and the synced input box hold the
original single line."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t))
        (long (concat "e2e-edit-wrap-" (make-string 100 ?x))))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (with-temp-buffer
            (insert (format "%s\ntail-line" long))
            (pi-mode-send-region (point-min) (point-max)))
          (should (pi-mode-e2e--wait-input process buffer "e2e-edit-wrap-"))
          (with-current-buffer buffer
            (pi-mode-edit-prompt))
          (let ((popup (pi-mode-e2e--prompt-edit-popup session)))
            (should popup)
            ;; The wrap is rejoined: one logical line plus the tail.
            (should (equal (with-current-buffer popup (buffer-string))
                           (format "%s\ntail-line" long)))
            (with-current-buffer popup
              (pi-mode-prompt-edit-submit))
            (should-not (buffer-live-p popup))
            ;; Both the head and the tail of the synced prompt are in
            ;; pi's input box (rendered wrapped, so compare in pieces).
            (should (pi-mode-e2e--wait-input process buffer "e2e-edit-wrap-"))
            (should (pi-mode-e2e--wait-input process buffer "tail-line"))
            (should-not (pi-mode-e2e--session-jsonl agent-dir))))
      (delete-directory project-root t))))

(ert-deftest pi-mode-e2e-test-edit-prompt-cancel ()
  "C-c C-k discards the edit: the popup closes and pi's input box
keeps the original prompt."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t))
        (marker "e2e-edit-cancel-marker"))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process _agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (with-temp-buffer
            (insert marker)
            (pi-mode-send-region (point-min) (point-max)))
          (should (pi-mode-e2e--wait-input process buffer marker))
          (with-current-buffer buffer
            (pi-mode-edit-prompt))
          (let ((popup (pi-mode-e2e--prompt-edit-popup session)))
            (should popup)
            (with-current-buffer popup
              (goto-char (point-max))
              (insert " NOPE")
              (pi-mode-prompt-edit-cancel))
            (should-not (buffer-live-p popup))
            ;; pi's input box still holds the original prompt.
            (should (pi-mode-e2e--wait-input process buffer marker))
            (should-not (string-match-p "NOPE" (pi-mode-e2e--buffer-text buffer)))))
      (delete-directory project-root t))))

(ert-deftest pi-mode-e2e-test-notifications-on-completion ()
  "pi-mode notifications fire once per completed turn against real pi.
The session JSONL is the only signal (pi records no agent_end event
outside RPC mode).  Round 1 primes the file: the first observation sees
a complete round and must stay silent (stale completion).  Round 2's
completion is then detected through the incremental scan — exactly one
notification, carrying the project name; an insert-only round adds
none."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t))
        (marker1 "e2e-notif-marker-1")
        (marker2 "e2e-notif-marker-2"))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          ;; Round 1: a full roundtrip while notifications are still off.
          (with-temp-buffer
            (insert marker1)
            (pi-mode-send-region (point-min) (point-max)))
          (should (pi-mode-e2e--wait-input process buffer marker1))
          (with-current-buffer buffer
            (ghostel-send-key "return"))
          (should (pi-mode-e2e--wait-reply process buffer))
          (should (pi-mode-e2e--wait-jsonl
                   process agent-dir
                   (lambda (content)
                     (and (string-match-p marker1 content)
                          (string-match-p "E2E-REPLY" content)))))
          ;; Watch the live session now.
          (let ((pi-mode-notifications t)
                (pi-mode-notifications-when-visible t)
                (delivered nil))
            (cl-letf (((symbol-function 'pi-mode-notifications--deliver)
                       (lambda (session) (push session delivered))))
              ;; First observation: round 1 is complete → the inference
              ;; marks it handled; no stale notification.
              (pi-mode-notifications--poll)
              (should-not delivered)
              ;; Round 2: the incremental scan processes the user message
              ;; then the terminal stop in file order → one delivery.
              (with-temp-buffer
                (insert marker2)
                (pi-mode-send-region (point-min) (point-max)))
              (should (pi-mode-e2e--wait-input process buffer marker2))
              (with-current-buffer buffer
                (ghostel-send-key "return"))
              (let ((deadline (+ (float-time) 40)))
                (while (and (null delivered) (< (float-time) deadline))
                  (pi-mode-notifications--poll)
                  (accept-process-output process 0.2)
                  (sleep-for 0.05)))
              (should delivered)
              (should (= 1 (length delivered)))
              (should (eq (car delivered) session))
              (let ((msg (pi-mode-notifications--message (car delivered))))
                (should (string-match-p "pi finished:" msg))
                (should (string-match-p "pi-e2e-proj" msg)))
              ;; The repeating timer keeps polling without duplicating.
              (sleep-for 2.5)
              (should (= 1 (length delivered)))
              ;; An insert-only round (nothing submitted) must not notify.
              (with-temp-buffer
                (insert "e2e-notif-marker-3")
                (pi-mode-send-region (point-min) (point-max)))
              (should (pi-mode-e2e--wait-input process buffer "e2e-notif-marker-3"))
              (pi-mode-notifications--poll)
              (should (= 1 (length delivered))))))
      (delete-directory project-root t))))

(provide 'pi-mode-e2e)
;;; pi-mode-e2e.el ends here
