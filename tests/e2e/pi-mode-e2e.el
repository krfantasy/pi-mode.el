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
  "Poll PREDICATE (called in BUFFER) until true or TIMEOUT seconds pass.
Pumps PROCESS output and ghostel's render timer; on timeout prints the
buffer tail for diagnosis and returns nil."
  (let ((deadline (+ (float-time) (or timeout 40))))
    (catch 'done
      (while (< (float-time) deadline)
        (when (with-current-buffer buffer (funcall predicate))
          (throw 'done t))
        (accept-process-output proc 0.4)
        (sleep-for 0.05))
      (when (buffer-live-p buffer)
        (princ (format "E2E TIMEOUT; buffer tail: %S\n"
                       (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                         (substring text (max 0 (- (length text) 400)))))))
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
  "Wait until pi's TUI rendered: status bar shows PROJECT-ROOT and model."
  (pi-mode-e2e--wait proc buffer
    (lambda ()
      (let ((text (pi-mode-e2e--buffer-text buffer)))
        (and (> (length text) 300)
             (string-match-p (regexp-quote project-root) text)
             (string-match-p "e2e-model" text))))
    45))

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
        (pi-mode-e2e--with-session (session buffer process agent-dir) project-root
          (should (process-live-p process))
          (should (eq (pi-mode--session-by-buffer buffer) session))
          ;; The session buffer owns the project root (cwd regression).
          (should (equal (with-current-buffer buffer default-directory)
                         project-root))
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          ;; The TUI status bar shows the project dir, not the caller's cwd.
          (should (string-match-p (regexp-quote project-root)
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
          (pi-mode-e2e--with-session (session buffer process agent-dir) project-root
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
        (pi-mode-e2e--with-session (session buffer process agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (pi-mode-interrupt)
          (should (process-live-p process))
          ;; The TUI keeps rendering after the interrupt.
          (should (pi-mode-e2e--wait process buffer
                    (lambda ()
                      (> (length (pi-mode-e2e--buffer-text buffer)) 300))
                    10)))
      (delete-directory project-root t))))

(ert-deftest pi-mode-e2e-test-window-side-and-panel ()
  "Pi buffers dock in a side window on `pi-mode-window-side'; toggle-panel
hides and restores them."
  (pi-mode-e2e-server-start)
  (let ((project-root (make-temp-file "pi-e2e-proj-" t)))
    (unwind-protect
        (pi-mode-e2e--with-session (session buffer process agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (let ((win (get-buffer-window buffer)))
            (should win)
            (should (eq (window-parameter win 'window-side)
                        pi-mode-window-side)))
          (should (eq (pi-mode-toggle-panel) :hidden))
          (should-not (get-buffer-window buffer))
          (should (eq (pi-mode-toggle-panel) :shown))
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
        (pi-mode-e2e--with-session (session buffer process agent-dir) project-root
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
        (pi-mode-e2e--with-session (session buffer process agent-dir) project-root
          (should (pi-mode-e2e--wait-ready process buffer project-root))
          (let ((second
                 (cl-letf (((symbol-function 'pi-mode--project-root)
                            (lambda () project-root)))
                   (pi-mode-session-continue))))
            (unwind-protect
                (progn
                  (should second)
                  (should (process-live-p (pi-mode-session-process second)))
                  (should-not (eq (pi-mode-session-buffer second) buffer))
                  (let ((b2 (pi-mode-session-buffer second))
                        (p2 (pi-mode-session-process second)))
                    (should (pi-mode-e2e--wait-ready p2 b2 project-root))
                    ;; stop with confirmation
                    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
                      (pi-mode-session-stop))
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

(provide 'pi-mode-e2e)
;;; pi-mode-e2e.el ends here
