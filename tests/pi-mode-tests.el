;;; pi-mode-tests.el --- Tests for pi-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests.  The ghostel API is mocked with cl-letf; fake processes are
;; real pipe processes (they work in batch mode).

;;; Code:

(require 'ert)
(require 'project)          ; project-root is not autoloaded; the cl-letf
                            ; mock of project-current bypasses its autoload
(require 'pi-mode)

(defvar pi-mode-test--calls nil
  "Alist of recorded ghostel calls: ((FUNCTION . ARGS)...).")

(defun pi-mode-test--record-call (fn &rest args)
  (push (cons fn args) pi-mode-test--calls))

(defun pi-mode-test--fake-process ()
  "Return a live pipe process usable as a fake pi process."
  (make-pipe-process :name "pi-mode-test-proc" :buffer nil))

(defun pi-mode-test--with-mock-ghostel (body)
  "Run BODY with the ghostel exec/send surface replaced by recorders."
  (let ((pi-mode-test--calls nil)
        (pi-mode-confirm-kill nil))   ; keep kill-buffer hooks inert in batch
    (clrhash pi-mode--sessions)      ; hermetic: no cross-test session leaks
    (cl-letf (((symbol-function 'ghostel-exec)
               (lambda (&rest args)
                 (apply #'pi-mode-test--record-call 'ghostel-exec args)
                 (pi-mode-test--fake-process)))
              ((symbol-function 'ghostel-send-string)
               (lambda (&rest args) (apply #'pi-mode-test--record-call 'ghostel-send-string args)))
              ((symbol-function 'ghostel-send-key)
               (lambda (&rest args) (apply #'pi-mode-test--record-call 'ghostel-send-key args)))
              ((symbol-function 'ghostel-paste-string)
               (lambda (&rest args) (apply #'pi-mode-test--record-call 'ghostel-paste-string args)))
              ((symbol-function 'ghostel-send-C-c)
               (lambda (&rest args) (apply #'pi-mode-test--record-call 'ghostel-send-C-c args))))
      (funcall body))))

(defmacro pi-mode-test-with-mock-ghostel (&rest body)
  "Run BODY with the ghostel API mocked."
  (declare (indent 0))
  `(pi-mode-test--with-mock-ghostel (lambda () ,@body)))

(ert-deftest pi-mode-test-basic-smoke ()
  "The package loads and the minor mode is defined."
  (should (boundp 'pi-mode))
  (should (functionp 'pi-mode))
  (should (boundp 'pi-mode-map)))

(ert-deftest pi-mode-test-debug-log ()
  "pi-mode-log appends to the debug buffer."
  (let ((pi-mode-debug t))
    (with-current-buffer (get-buffer-create "*pi-mode-debug*") (erase-buffer))
    (pi-mode-log "hello %s" "world")
    (with-current-buffer "*pi-mode-debug*"
      (should (string-match-p "hello world" (buffer-string))))))

(ert-deftest pi-mode-test-project-root-override ()
  "The override function wins when set."
  (let ((pi-mode-project-root-function (lambda () "/tmp/override/")))
    (should (equal (pi-mode--project-root) "/tmp/override/"))))

(ert-deftest pi-mode-test-project-root-fallback ()
  "Falls back to default-directory when no project is found."
  (let ((pi-mode-project-root-function nil)
        (default-directory "/tmp/fallback-dir/"))
    (cl-letf (((symbol-function 'project-current) (lambda () nil)))
      (should (equal (pi-mode--project-root) "/tmp/fallback-dir/")))))

(ert-deftest pi-mode-test-project-root-from-project ()
  "Uses project-root when project.el finds a project."
  (let ((pi-mode-project-root-function nil))
    ;; Real project-current returns (list 'vc BACKEND ROOT); the
    ;; (transient DIR) shape's project-root is a LIST in Emacs 30.
    (cl-letf (((symbol-function 'project-current)
               (lambda () (list 'vc 'Git "/tmp/proj-root/"))))
      (should (equal (pi-mode--project-root) "/tmp/proj-root/")))))

(ert-deftest pi-mode-test-unique-buffer-name ()
  (let ((buf (get-buffer-create "*pi[proj]*")))
    (unwind-protect
        (progn
          (should (equal (pi-mode--unique-buffer-name "*pi[proj]*") "*pi[proj]*<2>"))
          (should (equal (pi-mode--unique-buffer-name "*pi[other]*") "*pi[other]*")))
      (kill-buffer buf))))

(ert-deftest pi-mode-test-ghostel-launch-args ()
  "Launch resolves pi via exec-path and passes args to ghostel-exec."
  (pi-mode-test-with-mock-ghostel
   (with-temp-buffer
     (cl-letf (((symbol-function 'executable-find) (lambda (s) (and (equal s "pi") "/fake/bin/pi"))))
       (pi-mode--ghostel-launch (current-buffer) "/tmp/proj/" '("--tui-mode" "regular"))
       (let ((call (assq 'ghostel-exec pi-mode-test--calls)))
         (should call)
         (should (equal (nth 1 (cdr call)) "/fake/bin/pi"))
         (should (equal (nth 2 (cdr call)) '("--tui-mode" "regular"))))))))

(ert-deftest pi-mode-test-launch-missing-pi ()
  "Missing pi binary signals user-error."
  (pi-mode-test-with-mock-ghostel
   (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
     (with-temp-buffer
       (should-error (pi-mode--ghostel-launch (current-buffer) "/tmp/proj/" nil)
                     :type 'user-error)))))

(ert-deftest pi-mode-test-registry-and-sessions ()
  "Register, unregister, active, by-buffer, mru."
  (let ((b1 (get-buffer-create "*pi[a]*"))
        (b2 (get-buffer-create "*pi[b]*"))
        (p1 (pi-mode-test--fake-process))
        (p2 (pi-mode-test--fake-process)))
    (unwind-protect
        (let ((s1 (make-pi-mode-session :id "*pi[a]*" :buffer b1 :process p1
                                        :project-root "/tmp/a/" :last-used (current-time)))
              (s2 (make-pi-mode-session :id "*pi[b]*" :buffer b2 :process p2
                                        :project-root "/tmp/b/"
                                        :last-used (time-add (current-time) 10))))
          (pi-mode--register-session s1)
          (pi-mode--register-session s2)
          (should (= (length (pi-mode--active-sessions)) 2))
          (should (eq (pi-mode--session-by-buffer b1) s1))
          (should (eq (pi-mode--session-by-process p2) s2))
          (should (eq (pi-mode--mru-session (pi-mode--active-sessions)) s2))
          (pi-mode--unregister-session "*pi[a]*")
          (should (= (length (pi-mode--active-sessions)) 1))
          (pi-mode--unregister-session "*pi[b]*")
          (should (= (length (pi-mode--active-sessions)) 0)))
      (kill-buffer b1) (kill-buffer b2)
      (delete-process p1) (delete-process p2))))

(ert-deftest pi-mode-test-sentinel-chains-and-cleans-up ()
  "pi-mode sentinel runs the stashed sentinel first, then cleanup."
  (let ((b (get-buffer-create "*pi[sentinel]*"))
        (p (pi-mode-test--fake-process))
        (stashed-run nil) (cleaned nil))
    (unwind-protect
        (let ((s (make-pi-mode-session :id "*pi[sentinel]*" :buffer b :process p
                                       :project-root "/tmp/")))
          (pi-mode--register-session s)
          (set-process-sentinel p (lambda (_proc event) (setq stashed-run event)))
          (pi-mode--attach-sentinel p)
          (cl-letf (((symbol-function 'pi-mode--cleanup-session)
                     (lambda (_proc) (setq cleaned t))))
            (funcall (process-sentinel p) p "finished\n")
            (should (equal stashed-run "finished\n"))
            (should cleaned)))
      (kill-buffer b) (delete-process p)
      (pi-mode--unregister-session "*pi[sentinel]*"))))

(ert-deftest pi-mode-test-launch-failure-leaves-no-session ()
  "A failed launch (missing pi) registers nothing and leaves no buffer."
  (pi-mode-test-with-mock-ghostel
   (let ((sessions-before (hash-table-count pi-mode--sessions))
         (buffers-before (length (cl-remove-if-not
                                  (lambda (b) (string-match-p "\\*pi\\[" (buffer-name b)))
                                  (buffer-list)))))
     (cl-letf (((symbol-function 'executable-find) (lambda (_s) nil)))
       (should-error (pi-mode--launch-buffer "/tmp/proj/" nil)
                     :type 'user-error))
     (should (= (hash-table-count pi-mode--sessions) sessions-before))
     (should (= (length (cl-remove-if-not
                         (lambda (b) (string-match-p "\\*pi\\[" (buffer-name b)))
                         (buffer-list)))
                buffers-before)))))

(ert-deftest pi-mode-test-start-command-launches ()
  "pi-mode-start launches; the pi-mode minor mode stays intact."
  (pi-mode-test-with-mock-ghostel
   (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/proj/"))
             ((symbol-function 'executable-find) (lambda (_s) "/fake/pi"))
             ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
             ((symbol-function 'run-hook-with-args) (lambda (&rest _) nil)))
     (let ((session (pi-mode-start)))
       (unwind-protect
           (progn
             (should (pi-mode-session-process session))
             (should (eq (pi-mode--session-by-buffer (pi-mode-session-buffer session))
                         session))
             (with-current-buffer (pi-mode-session-buffer session)
               (should pi-mode)             ; minor mode enabled
               (pi-mode -1)
               (should (not pi-mode))
               (pi-mode +1)
               (should pi-mode)))           ; minor mode function intact
         (pi-mode--unregister-session (pi-mode-session-id session))
         (when (buffer-live-p (pi-mode-session-buffer session))
           (kill-buffer (pi-mode-session-buffer session)))
         (when (process-live-p (pi-mode-session-process session))
           (delete-process (pi-mode-session-process session))))))))

(ert-deftest pi-mode-test-resolve-session-rules ()
  "Resolution: in-buffer self; sole; mru with echo; C-u prompts; no-ask."
  (let ((b1 (get-buffer-create "*pi[r1]*"))
        (b2 (get-buffer-create "*pi[r2]*"))
        (p1 (pi-mode-test--fake-process))
        (p2 (pi-mode-test--fake-process)))
    (unwind-protect
        (let ((s1 (make-pi-mode-session :id "*pi[r1]*" :buffer b1 :process p1
                                        :project-root "/tmp/" :last-used (current-time)))
              (s2 (make-pi-mode-session :id "*pi[r2]*" :buffer b2 :process p2
                                        :project-root "/tmp/"
                                        :last-used (time-add (current-time) 10))))
          (pi-mode--register-session s1)
          (pi-mode--register-session s2)
          ;; in-buffer self → s1 (current buffer is b1)
          (with-current-buffer b1
            (should (eq (pi-mode--resolve-session nil) s1)))
          ;; C-u prompts (mock completing-read → s2)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "*pi[r2]*")))
            (should (eq (pi-mode--resolve-session t) s2)))
          ;; no-ask: skips prompt even with prefix, uses mru
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) (error "should not prompt"))))
            (should (eq (pi-mode--resolve-session t t) s2)))
          (pi-mode--unregister-session "*pi[r1]*")
          ;; rule 3: sole session → it (from an unrelated buffer)
          (with-temp-buffer
            (should (eq (pi-mode--resolve-session nil) s2)))
          ;; no sessions → user-error
          (pi-mode--unregister-session "*pi[r2]*")
          (should-error (pi-mode--resolve-session nil) :type 'user-error))
      (kill-buffer b1) (kill-buffer b2)
      (delete-process p1) (delete-process p2))))


(ert-deftest pi-mode-test-send-text ()
  "send-text pastes, presses return, runs before-send hook."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[send]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[send]*" :buffer b :process p
                                   :project-root "/tmp/"))
          (hook-args nil))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (let ((pi-mode-before-send-hook
                  (list (lambda (sess txt) (setq hook-args (list sess txt))))))
             (pi-mode--send-text s "hello pi"))
           (should (equal hook-args (list s "hello pi")))
           (should (assq 'ghostel-paste-string pi-mode-test--calls))
           (should (equal (cdr (assq 'ghostel-paste-string pi-mode-test--calls))
                          '("hello pi")))
           (should (equal (cdr (assq 'ghostel-send-key pi-mode-test--calls))
                          '("return"))))
       (pi-mode--unregister-session "*pi[send]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-send-text-dead-session ()
  "send-text to a dead session signals user-error."
  (pi-mode-test-with-mock-ghostel
   (let ((s (make-pi-mode-session :id "*pi[dead]*" :buffer (get-buffer-create "*pi[dead]*")
                                  :process (pi-mode-test--fake-process)
                                  :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (delete-process (pi-mode-session-process s))
           (should-error (pi-mode--send-text s "x") :type 'user-error))
       (pi-mode--unregister-session "*pi[dead]*")
       (kill-buffer (pi-mode-session-buffer s))))))

(ert-deftest pi-mode-test-prompt-history ()
  "Push dedupes, trims, persists; load restores."
  (let ((pi-mode-prompt-history-file
         (make-temp-file "pi-mode-history-" nil ".el"))
        (pi-mode-prompt-history-length 3)
        (pi-mode-prompt-history nil))
    (pi-mode--prompt-history-push "one")
    (pi-mode--prompt-history-push "two")
    (pi-mode--prompt-history-push "one")   ; dedupe vs car
    (pi-mode--prompt-history-push "three")
    (pi-mode--prompt-history-push "four")  ; trim to 3
    (should (equal pi-mode-prompt-history '("four" "three" "two")))
    (pi-mode--prompt-history-save)
    (setq pi-mode-prompt-history--loaded nil) ; force a fresh file read
    (let ((pi-mode-prompt-history nil))
      (pi-mode--prompt-history-load)
      (should (equal pi-mode-prompt-history '("four" "three" "two"))))
    (delete-file pi-mode-prompt-history-file)))

(ert-deftest pi-mode-test-send-prompt-flow ()
  "send-prompt reads, pushes history, sends."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[sp]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[sp]*" :buffer b :process p
                                   :project-root "/tmp/"))
          (pi-mode-prompt-history nil)
          (pi-mode-prompt-history-file
           (make-temp-file "pi-mode-history-" nil ".el")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-current-buffer b
             (cl-letf (((symbol-function 'pi-mode--read-prompt)
                        (lambda () "my prompt"))
                       ((symbol-function 'completing-read)
                        (lambda (&rest _) (error "no prompt needed"))))
               (pi-mode-send-prompt)))
           (should (equal pi-mode-prompt-history '("my prompt")))
           (should (assq 'ghostel-paste-string pi-mode-test--calls)))
       (pi-mode--unregister-session "*pi[sp]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-interrupt-sends-escape ()
  "pi-mode-interrupt sends the escape key."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[int]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[int]*" :buffer b :process p
                                   :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-current-buffer b (pi-mode-interrupt))
           (should (equal (cdr (assq 'ghostel-send-key pi-mode-test--calls))
                          '("escape" nil))))
       (pi-mode--unregister-session "*pi[int]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-embed-file-format ()
  "Embed format matches pi's <file name=...> convention."
  (should (equal (pi-mode--embed-file "/tmp/a.ts" "line1\nline2")
                 "<file name=\"/tmp/a.ts\">\nline1\nline2\n</file>")))

(ert-deftest pi-mode-test-region-line-range ()
  (with-temp-buffer
    (insert "a\nb\nc\nd\n")
    (should (equal (pi-mode--region-line-range 1 8) '(1 4)))))

(ert-deftest pi-mode-test-send-region-embed ()
  "send-region embeds region content and records the prompt in history."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[re]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[re]*" :buffer b :process p
                                   :project-root "/tmp/"))
          (pi-mode-region-embed-p t)
          (pi-mode-prompt-history nil)
          (pi-mode-prompt-history-file
           (make-temp-file "pi-mode-history-" nil ".el")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-temp-buffer
             (insert "hello\nworld")
             (let ((file "/tmp/hello.txt"))
               (cl-letf (((symbol-function 'buffer-file-name) (lambda () file))
                         ((symbol-function 'pi-mode--read-prompt) (lambda () "review this")))
                 (pi-mode-send-region (point-min) (point-max) nil))))
           (let ((call (assq 'ghostel-paste-string pi-mode-test--calls))
                 (text (car (cdr (assq 'ghostel-paste-string pi-mode-test--calls)))))
             (should call)
             (should (string-match-p
                      "review this\n\n<file name=\"/tmp/hello.txt\">\nhello\nworld\n</file>"
                      text)))
           (should (equal pi-mode-prompt-history '("review this"))))
       (pi-mode--unregister-session "*pi[re]*")
       (kill-buffer b) (delete-process p)
       (ignore-errors (delete-file pi-mode-prompt-history-file))))))

(ert-deftest pi-mode-test-send-region-reference ()
  "C-u (reference-p) sends @path#Lstart-Lend instead of content."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[rr]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[rr]*" :buffer b :process p
                                   :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-temp-buffer
             (insert "a\nb\nc\n")
             (cl-letf (((symbol-function 'buffer-file-name) (lambda () "/tmp/r.ts"))
                       ((symbol-function 'pi-mode--read-prompt) (lambda () "")))
               (pi-mode-send-region (point-min) (point-max) t)))
           (let ((call (assq 'ghostel-paste-string pi-mode-test--calls)))
             (should call)
             (should (string-match-p "@/tmp/r.ts#L1-L4"
                                     (car (cdr (assq 'ghostel-paste-string pi-mode-test--calls)))))))
       (pi-mode--unregister-session "*pi[rr]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-send-region-requires-region ()
  "send-region without a region (point-min = point-max) errors."
  (pi-mode-test-with-mock-ghostel
   (should-error (pi-mode-send-region 1 1 nil) :type 'user-error)))

(ert-deftest pi-mode-test-send-defun-embed ()
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[rd]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[rd]*" :buffer b :process p
                                   :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-temp-buffer
             (insert "(defun foo ()\n  t)\n\n(defun bar ()\n  nil)\n")
             (goto-char (point-min))
             (cl-letf (((symbol-function 'buffer-file-name) (lambda () "/tmp/x.el"))
                       ((symbol-function 'pi-mode--read-prompt) (lambda () "")))
               (pi-mode-send-defun)))
           (let ((call (assq 'ghostel-paste-string pi-mode-test--calls)))
             (should call)
             (should (string-match-p
                      "<file name=\"/tmp/x.el\">\n(defun foo ()\n  t)"
                      (car (cdr (assq 'ghostel-paste-string pi-mode-test--calls)))))))
       (pi-mode--unregister-session "*pi[rd]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-send-file-embed ()
  "send-file embeds the file content read from disk."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[sf]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[sf]*" :buffer b :process p
                                   :project-root "/tmp/"))
          (file (make-temp-file "pi-mode-send-file-" nil ".txt")))
     (unwind-protect
         (progn
           (write-region "alpha\nbeta" nil file)
           (pi-mode--register-session s)
           (cl-letf (((symbol-function 'pi-mode--read-prompt) (lambda () "")))
             (pi-mode-send-file file))
           (let ((text (car (cdr (assq 'ghostel-paste-string pi-mode-test--calls)))))
             (should (string-match-p
                      (format "<file name=\"%s\">\nalpha\nbeta\n</file>"
                              (regexp-quote file))
                      text))))
       (pi-mode--unregister-session "*pi[sf]*")
       (kill-buffer b) (delete-process p)
       (delete-file file)))))

(ert-deftest pi-mode-test-send-defun-no-defun ()
  "send-defun in an empty buffer signals user-error."
  (pi-mode-test-with-mock-ghostel
   (with-temp-buffer
     (should-error (pi-mode-send-defun) :type 'user-error))))

(ert-deftest pi-mode-test-send-error-fallback-parse ()
  "Error-at-point parses file:line:col from thing at point."
  (with-temp-buffer
    (insert "/tmp/broken.ts:12:5")
    (goto-char 1)
    (cl-letf (((symbol-function 'thing-at-point)
               (lambda (thing) (when (eq thing 'filename)
                                 "/tmp/broken.ts:12:5"))))
      (should (equal (pi-mode--error-at-point)
                     '("/tmp/broken.ts" 12 5 nil))))))

;;; Task 6: session commands

(ert-deftest pi-mode-test-session-dir ()
  "Session dir mirrors pi's --path-- layout and honors PI_CODING_AGENT_DIR."
  (let ((agent-dir (make-temp-file "pi-agent-" t)))
    (unwind-protect
        (cl-letf (((getenv "PI_CODING_AGENT_DIR") agent-dir))
          (should (equal (pi-mode--session-dir "/Users/me/proj")
                         (expand-file-name "--Users-me-proj--"
                                           (expand-file-name "sessions" agent-dir)))))
      (delete-directory agent-dir t))))

(ert-deftest pi-mode-test-session-files ()
  (let ((dir (make-temp-file "pi-sessions-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "a.jsonl" dir))
          (write-region "" nil (expand-file-name "b.jsonl" dir))
          (write-region "" nil (expand-file-name "ignore.txt" dir))
          (let ((pi-mode-session-dir-function (lambda (_root) dir)))
            (should (= (length (pi-mode--session-files "/tmp/x")) 2))))
      (delete-directory dir t))))

(ert-deftest pi-mode-test-session-continue-launch ()
  "continue launches pi -c in a new buffer."
  (pi-mode-test-with-mock-ghostel
   (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/proj/"))
             ((symbol-function 'executable-find) (lambda (_s) "/fake/pi"))
             ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
             ((symbol-function 'run-hook-with-args) (lambda (&rest _) nil)))
     (let ((session (pi-mode-session-continue)))
       (unwind-protect
           (let ((call (assq 'ghostel-exec pi-mode-test--calls)))
             (should call)
             (should (member "-c" (nth 2 (cdr call)))))
         (pi-mode--unregister-session (pi-mode-session-id session))
         (when (buffer-live-p (pi-mode-session-buffer session))
           (kill-buffer (pi-mode-session-buffer session)))
         (when (process-live-p (pi-mode-session-process session))
           (delete-process (pi-mode-session-process session))))))))

(ert-deftest pi-mode-test-session-rename-sends ()
  "rename sends /name and updates the struct."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[rn]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[rn]*" :buffer b :process p
                                   :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-current-buffer b
             (pi-mode-session-rename "refactor"))
           (should (equal (pi-mode-session-name s) "refactor"))
           (let ((call (assq 'ghostel-send-string pi-mode-test--calls)))
             (should call)
             (should (equal (car (cdr call)) "/name refactor"))))
       (pi-mode--unregister-session "*pi[rn]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-session-stop-prompts ()
  "stop requires confirmation then deletes the process."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[st]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[st]*" :buffer b :process p
                                   :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-current-buffer b
             (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
               (pi-mode-session-stop))
             (should (not (process-live-p p))))
           ;; second half: decline with a fresh live process
           (let ((p2 (pi-mode-test--fake-process)))
             (setf (pi-mode-session-process s) p2)
             (pi-mode--register-session s)
             (with-current-buffer b
               (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
                 (pi-mode-session-stop))
               (should (process-live-p p2))
               (delete-process p2)))
           (pi-mode--unregister-session "*pi[st]*"))
       (kill-buffer b)
       (ignore-errors (delete-process p))))))

(ert-deftest pi-mode-test-menu-defined ()
  "The transient menu and its suffix commands exist."
  (should (commandp 'pi-mode-menu))
  (dolist (cmd '(pi-mode-session-continue pi-mode-session-resume
                 pi-mode-session-fork pi-mode-session-rename
                 pi-mode-session-stop pi-mode-session-stop-all
                 pi-mode-list-sessions pi-mode-send-prompt
                 pi-mode-send-region pi-mode-send-file
                 pi-mode-send-defun pi-mode-send-error
                 pi-mode-switch-buffer pi-mode-toggle-panel
                 pi-mode-show-all pi-mode-toggle-recent
                 pi-mode-interrupt))
    (should (commandp cmd))))

(ert-deftest pi-mode-test-global-menu-binding ()
  "C-c C-' is bound to pi-mode-menu globally."
  (should (eq (lookup-key (current-global-map) (kbd "C-c C-'"))
              #'pi-mode-menu)))

(ert-deftest pi-mode-test-window-commands-no-error ()
  "Window commands handle the no-session case gracefully."
  (should-error (pi-mode-toggle-recent) :type 'user-error)
  (should-error (pi-mode-show-all) :type 'user-error)
  (should (equal (pi-mode-toggle-panel) :hidden)))

(provide 'pi-mode-tests)
;;; pi-mode-tests.el ends here
