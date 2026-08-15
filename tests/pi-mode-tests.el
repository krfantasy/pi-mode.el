;;; pi-mode-tests.el --- Tests for pi-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests.  The ghostel API is mocked with cl-letf; fake processes are
;; real pipe processes (they work in batch mode).

;;; Code:

(require 'ert)
(require 'json)            ; json-encode for the notification test fixtures
(require 'project)          ; project-root is not autoloaded; the cl-letf
                            ; mock of project-current bypasses its autoload
(require 'pi-mode)
(require 'pi-mode-status)

;; tab-bar.el is preloaded in Emacs 30 but not in 28/29; the
;; tab-isolation tests bind it via cl-letf, so keep it bound.
(defvar tab-bar-mode nil)

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

(ert-deftest pi-mode-test-session-buffer-p ()
  "The predicate matches session buffers by buffer-local var, any name."
  (let ((b (get-buffer-create "*pi[pred]*")))
    (unwind-protect
        (progn
          (should-not (pi-mode--session-buffer-p b))
          ;; display-buffer-alist passes buffer NAME strings
          (should-not (pi-mode--session-buffer-p "*pi[pred]*"))
          (with-current-buffer b
            (setq-local pi-mode--session (make-pi-mode-session :id "*pi[pred]*")))
          (should (pi-mode--session-buffer-p b))
          (should (pi-mode--session-buffer-p "*pi[pred]*")))
      (kill-buffer b))))

(ert-deftest pi-mode-test-session-buffer-p-killed ()
  "The predicate is nil for killed buffers."
  (let ((b (get-buffer-create "*pi[dead]*")))
    (with-current-buffer b
      (setq-local pi-mode--session (make-pi-mode-session :id "*pi[dead]*")))
    (kill-buffer b)
    (should-not (pi-mode--session-buffer-p b))
    (should-not (pi-mode--session-buffer-p "*pi[dead]*"))))

(ert-deftest pi-mode-test-session-base-name-default ()
  "Default naming without a custom function."
  (let ((pi-mode-buffer-name-function nil))
    (should (equal (pi-mode--session-base-name "/tmp/proj/") "*pi[proj]*"))
    (should (equal (pi-mode--session-base-name "/tmp/proj/" "refactor")
                   "*pi[proj:refactor]*"))))

(ert-deftest pi-mode-test-session-base-name-custom ()
  "The custom function receives (DIRECTORY &optional NAME)."
  (let ((pi-mode-buffer-name-function
         (lambda (directory &optional name)
           (let ((project (file-name-nondirectory (directory-file-name directory))))
             (if name (format "*Pi:%s/%s*" project name)
               (format "*Pi:%s*" project))))))
    (should (equal (pi-mode--session-base-name "/tmp/proj/") "*Pi:proj*"))
    (should (equal (pi-mode--session-base-name "/tmp/proj/" "refactor")
                   "*Pi:proj/refactor*"))))

(ert-deftest pi-mode-test-make-session-custom-name ()
  "pi-mode--make-session honors the custom buffer-name function."
  (let* ((pi-mode-buffer-name-function
         (lambda (directory &optional name)
           (format "*Custom[%s:%s]*" (file-name-nondirectory (directory-file-name directory))
                   (or name "default"))))
        (s (pi-mode--make-session "/tmp/cproj/" "refactor")))
    (unwind-protect
        (progn
          (should (equal (buffer-name (pi-mode-session-buffer s))
                         "*Custom[cproj:refactor]*"))
          (should (equal (pi-mode-session-id s) "*Custom[cproj:refactor]*")))
      (kill-buffer (pi-mode-session-buffer s)))))

(ert-deftest pi-mode-test-make-session-custom-name-unique ()
  "Custom-named session buffers still get <N> uniquification."
  (let* ((pi-mode-buffer-name-function
         (lambda (_directory &optional name) (format "*Custom[%s]*" (or name "x"))))
        (s1 (pi-mode--make-session "/tmp/u1/" "a")))
    (unwind-protect
        (let ((s2 (pi-mode--make-session "/tmp/u1/" "a")))
          (unwind-protect
              (should (equal (buffer-name (pi-mode-session-buffer s2)) "*Custom[a]*<2>"))
            (kill-buffer (pi-mode-session-buffer s2))))
      (kill-buffer (pi-mode-session-buffer s1)))))

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
        (stashed-run nil) (hook-args nil))
    (unwind-protect
        (let ((s (make-pi-mode-session :id "*pi[sentinel]*" :buffer b :process p
                                       :project-root "/tmp/")))
          (pi-mode--register-session s)
          (set-process-sentinel p (lambda (_proc event) (setq stashed-run event)))
          (let ((pi-mode-on-exit-hook
                 (list (lambda (buf event) (setq hook-args (list buf event))))))
            (pi-mode--attach-sentinel p)
            (funcall (process-sentinel p) p "finished\n")
            (should (equal stashed-run "finished\n"))
            (should (equal hook-args (list b "finished")))
            ;; claude-code-ide behavior: exit also kills the buffer.
            (should-not (buffer-live-p b))
            (should-not (pi-mode--session-by-buffer b))))
      (kill-buffer b) (delete-process p)
      (pi-mode--unregister-session "*pi[sentinel]*"))))

(ert-deftest pi-mode-test-exit-kills-buffer-default ()
  "The default matches claude-code-ide.el: exit kills the session buffer.
`pi-mode-kill-buffer-on-exit' defaults to t; nil keeps the buffer."
  (should pi-mode-kill-buffer-on-exit)
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[exitk]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[exitk]*" :buffer b :process p
                                   :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (let ((pi-mode-kill-buffer-on-exit nil))
             (pi-mode--cleanup-session p "finished\n")
             (should (buffer-live-p b)))
           (setf (pi-mode-session-cleanup-done s) nil)
           (pi-mode--register-session s)
           (pi-mode--cleanup-session p "finished\n")
           (should-not (buffer-live-p b)))
       (when (buffer-live-p b) (kill-buffer b))
       (pi-mode--unregister-session "*pi[exitk]*")))))

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

(ert-deftest pi-mode-test-read-instance-name-empty ()
  "Empty (or blank) input returns nil (auto-named)."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
    (should-not (pi-mode--read-instance-name "/tmp/proj/"))))

(ert-deftest pi-mode-test-read-instance-name-valid ()
  "A valid name is trimmed and returned."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  refactor  ")))
    (should (equal (pi-mode--read-instance-name "/tmp/proj/") "refactor"))))

(ert-deftest pi-mode-test-read-instance-name-rejects-numeric ()
  "Pure-numeric names are rejected (reserved) and re-prompted."
  (let ((answers '("2" "refactor")))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (pop answers))))
      (should (equal (pi-mode--read-instance-name "/tmp/proj/") "refactor"))
      (should-not answers))))

(ert-deftest pi-mode-test-read-instance-name-rejects-brackets ()
  "Names with [, ], *, or control chars are rejected and re-prompted."
  (let ((answers '("bad*name" "[nope]" "\C-a" "ok")))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (pop answers))))
      (should (equal (pi-mode--read-instance-name "/tmp/proj/") "ok"))
      (should-not answers))))

(ert-deftest pi-mode-test-read-instance-name-rejects-duplicate ()
  "Names already used by a live session of the same project are rejected."
  (let ((b (get-buffer-create "*pi[dup]*"))
        (p (pi-mode-test--fake-process)))
    (unwind-protect
        (let ((s (make-pi-mode-session :id "*pi[dup]*" :buffer b :process p
                                       :project-root "/tmp/proj/"
                                       :name "refactor" :last-used (current-time)))
              (answers '("refactor" "other")))
          (pi-mode--register-session s)
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (pop answers))))
            (should (equal (pi-mode--read-instance-name "/tmp/proj/") "other"))
            (should-not answers)))
      (pi-mode--unregister-session "*pi[dup]*")
      (kill-buffer b)
      (delete-process p))))

(ert-deftest pi-mode-test-read-instance-name-allows-same-name-other-project ()
  "A name used by a live session in ANOTHER project is allowed."
  (let ((b (get-buffer-create "*pi[dupfor]*"))
        (p (pi-mode-test--fake-process)))
    (unwind-protect
        (let ((s (make-pi-mode-session :id "*pi[dupfor]*" :buffer b :process p
                                       :project-root "/tmp/other/"
                                       :name "refactor" :last-used (current-time))))
          (pi-mode--register-session s)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) "refactor")))
            (should (equal (pi-mode--read-instance-name "/tmp/proj/") "refactor"))))
      (pi-mode--unregister-session "*pi[dupfor]*")
      (kill-buffer b)
      (delete-process p))))

(ert-deftest pi-mode-test-read-instance-name-allows-name-of-dead-session ()
  "A name used only by a DEAD session of the same project is allowed."
  (let ((b (get-buffer-create "*pi[dupdead]*"))
        (p (pi-mode-test--fake-process)))
    (unwind-protect
        (let ((s (make-pi-mode-session :id "*pi[dupdead]*" :buffer b :process p
                                       :project-root "/tmp/proj/"
                                       :name "refactor" :last-used (current-time))))
          (pi-mode--register-session s)
          (delete-process p)              ; s's process dies: no longer live
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) "refactor")))
            (should (equal (pi-mode--read-instance-name "/tmp/proj/") "refactor"))))
      (pi-mode--unregister-session "*pi[dupdead]*")
      (kill-buffer b)
      (ignore-errors (delete-process p)))))

(ert-deftest pi-mode-test-start-with-prefix-launches-named ()
  "C-u start prompts for an instance name and launches with it."
  (pi-mode-test-with-mock-ghostel
   (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/proj/"))
             ((symbol-function 'executable-find) (lambda (_s) "/fake/pi"))
             ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
             ((symbol-function 'run-hook-with-args) (lambda (&rest _) nil))
             ((symbol-function 'pi-mode--read-instance-name)
              (lambda (&optional _root) "refactor"))
             (current-prefix-arg '(4)))
     (let ((session (pi-mode-start)))
       (unwind-protect
           (progn
             (should (equal (pi-mode-session-name session) "refactor"))
             (should (equal (buffer-name (pi-mode-session-buffer session))
                            "*pi[proj:refactor]*")))
         (pi-mode--unregister-session (pi-mode-session-id session))
         (when (buffer-live-p (pi-mode-session-buffer session))
           (kill-buffer (pi-mode-session-buffer session)))
         (when (process-live-p (pi-mode-session-process session))
           (delete-process (pi-mode-session-process session))))))))

(ert-deftest pi-mode-test-start-without-prefix-never-prompts ()
  "Start without a prefix never prompts and launches unnamed."
  (pi-mode-test-with-mock-ghostel
   (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/proj/"))
             ((symbol-function 'executable-find) (lambda (_s) "/fake/pi"))
             ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
             ((symbol-function 'run-hook-with-args) (lambda (&rest _) nil))
             ((symbol-function 'read-string)
              (lambda (&rest _) (error "must not prompt without a prefix"))))
     (let ((session (pi-mode-start)))
       (unwind-protect
           (progn
             (should-not (pi-mode-session-name session))
             (should (equal (buffer-name (pi-mode-session-buffer session))
                            "*pi[proj]*")))
         (pi-mode--unregister-session (pi-mode-session-id session))
         (when (buffer-live-p (pi-mode-session-buffer session))
           (kill-buffer (pi-mode-session-buffer session)))
         (when (process-live-p (pi-mode-session-process session))
           (delete-process (pi-mode-session-process session))))))))

(ert-deftest pi-mode-test-start-with-prefix-empty-name-auto ()
  "C-u start with empty input auto-names (nil name, plain buffer)."
  (pi-mode-test-with-mock-ghostel
   (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/proj/"))
             ((symbol-function 'executable-find) (lambda (_s) "/fake/pi"))
             ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
             ((symbol-function 'run-hook-with-args) (lambda (&rest _) nil))
             ((symbol-function 'pi-mode--read-instance-name) (lambda (&optional _root) nil))
             (current-prefix-arg '(4)))
     (let ((session (pi-mode-start)))
       (unwind-protect
           (progn
             (should-not (pi-mode-session-name session))
             (should (equal (buffer-name (pi-mode-session-buffer session))
                            "*pi[proj]*")))
         (pi-mode--unregister-session (pi-mode-session-id session))
         (when (buffer-live-p (pi-mode-session-buffer session))
           (kill-buffer (pi-mode-session-buffer session)))
         (when (process-live-p (pi-mode-session-process session))
           (delete-process (pi-mode-session-process session))))))))

(ert-deftest pi-mode-test-resolve-session-rules ()
  "Resolution: in-buffer self; sole; mru with echo; C-u prompts; no-ask."
  (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/")))
    (let ((b1 (get-buffer-create "*pi[r1]*"))
          (b2 (get-buffer-create "*pi[r2]*"))
          (b3 (get-buffer-create "*pi[r3]*"))
          (p1 (pi-mode-test--fake-process))
          (p2 (pi-mode-test--fake-process))
          (p3 (pi-mode-test--fake-process)))
      (unwind-protect
          (let ((s1 (make-pi-mode-session :id "*pi[r1]*" :buffer b1 :process p1
                                          :project-root "/tmp/" :last-used (current-time)))
                (s2 (make-pi-mode-session :id "*pi[r2]*" :buffer b2 :process p2
                                          :project-root "/tmp/"
                                          :last-used (time-add (current-time) 10)))
                ;; foreign project, most recent of all: never resolved here
                (s3 (make-pi-mode-session :id "*pi[r3]*" :buffer b3 :process p3
                                          :project-root "/tmp/other/"
                                          :last-used (time-add (current-time) 100))))
            (pi-mode--register-session s1)
            (pi-mode--register-session s2)
            (pi-mode--register-session s3)
            ;; in-buffer self → s1 (current buffer is b1), whichever project
            (with-current-buffer b1
              (should (eq (pi-mode--resolve-session nil) s1)))
            ;; in-buffer self wins for the foreign project too
            (with-current-buffer b3
              (should (eq (pi-mode--resolve-session nil) s3)))
            ;; C-u always asks, even from inside a session buffer
            (with-current-buffer b1
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "*pi[r2]*")))
                (should (eq (pi-mode--resolve-session t) s2))))
            ;; C-u prompts (mock completing-read → s2)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "*pi[r2]*")))
              (should (eq (pi-mode--resolve-session t) s2)))
            ;; no-ask: skips prompt even with prefix, uses mru
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) (error "should not prompt"))))
              (should (eq (pi-mode--resolve-session t t) s2))
              ;; ambiguous, no prefix: current project's MRU, not foreign s3
              (should (eq (pi-mode--resolve-session nil nil) s2)))
            (pi-mode--unregister-session "*pi[r1]*")
            ;; rule 3: sole session → it (from an unrelated buffer)
            (with-temp-buffer
              (should (eq (pi-mode--resolve-session nil) s2)))
            ;; no project sessions (only foreign s3) → user-error
            ;; naming the current project
            (pi-mode--unregister-session "*pi[r2]*")
            (let ((err (should-error (pi-mode--resolve-session nil)
                                      :type 'user-error)))
              (should (string-match-p (regexp-quote "/tmp/") (cadr err)))))
        (kill-buffer b1) (kill-buffer b2) (kill-buffer b3)
        (pi-mode--unregister-session "*pi[r3]*")
        (delete-process p1) (delete-process p2) (delete-process p3)))))

(ert-deftest pi-mode-test-resolve-updates-mru ()
  "Resolving a session updates last-used so it becomes the MRU session."
  (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/")))
    (let ((b1 (get-buffer-create "*pi[m1]*"))
          (b2 (get-buffer-create "*pi[m2]*"))
          (p1 (pi-mode-test--fake-process))
          (p2 (pi-mode-test--fake-process)))
      (unwind-protect
          (let ((s1 (make-pi-mode-session :id "*pi[m1]*" :buffer b1 :process p1
                                          :project-root "/tmp/"
                                          :last-used (current-time)))
                (s2 (make-pi-mode-session :id "*pi[m2]*" :buffer b2 :process p2
                                          :project-root "/tmp/"
                                          :last-used (time-add (current-time) -100))))
            (pi-mode--register-session s1)
            (pi-mode--register-session s2)
            ;; s1 is newest; resolve s2 via the C-u prompt branch
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "*pi[m2]*")))
              (should (eq (pi-mode--resolve-session t) s2)))
            (should (eq (pi-mode--mru-session (pi-mode--active-sessions)) s2))
            ;; resolving s1 again makes it MRU again
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "*pi[m1]*")))
              (should (eq (pi-mode--resolve-session t) s1)))
            (should (eq (pi-mode--mru-session (pi-mode--active-sessions)) s1))
            ;; in-buffer branch also updates
            (with-current-buffer b2
              (should (eq (pi-mode--resolve-session nil) s2)))
            (should (eq (pi-mode--mru-session (pi-mode--active-sessions)) s2))
            (pi-mode--unregister-session "*pi[m1]*")
            (pi-mode--unregister-session "*pi[m2]*"))
        (kill-buffer b1) (kill-buffer b2)
        (delete-process p1) (delete-process p2)))))

(ert-deftest pi-mode-test-resolve-sole-foreign-errors ()
  "A live session in another project is never resolved from this one."
  (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/")))
    (let ((b (get-buffer-create "*pi[sf]*"))
          (b2 (get-buffer-create "*pi[sl]*"))
          (p (pi-mode-test--fake-process))
          (p2 (pi-mode-test--fake-process)))
      (unwind-protect
          (let ((s (make-pi-mode-session :id "*pi[sf]*" :buffer b :process p
                                         :project-root "/tmp/other/"
                                         :last-used (current-time))))
            (pi-mode--register-session s)
            ;; sole foreign session: the current project has none
            (with-temp-buffer
              (let ((err (should-error (pi-mode--resolve-session nil)
                                        :type 'user-error)))
                (should (string-match-p (regexp-quote "/tmp/") (cadr err)))))
            ;; the empty-project check wins even inside the foreign buffer
            (with-current-buffer b
              (should-error (pi-mode--resolve-session nil) :type 'user-error))
            ;; once the current project has a session, the in-buffer
            ;; branch targets this instance, whichever project it is in
            (let ((s2 (make-pi-mode-session :id "*pi[sl]*" :buffer b2 :process p2
                                            :project-root "/tmp/"
                                            :last-used (current-time))))
              (pi-mode--register-session s2)
              (with-current-buffer b
                (should (eq (pi-mode--resolve-session nil) s)))
              (pi-mode--unregister-session "*pi[sl]*")))
        (pi-mode--unregister-session "*pi[sf]*")
        (kill-buffer b) (kill-buffer b2)
        (delete-process p) (delete-process p2)))))

(ert-deftest pi-mode-test-resolve-mru-ignores-foreign ()
  "The MRU branch picks the current project's MRU, never a foreign one."
  (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/")))
    (let ((b1 (get-buffer-create "*pi[f1]*"))
          (b2 (get-buffer-create "*pi[f2]*"))
          (bf (get-buffer-create "*pi[ff]*"))
          (p1 (pi-mode-test--fake-process))
          (p2 (pi-mode-test--fake-process))
          (pf (pi-mode-test--fake-process)))
      (unwind-protect
          (let ((s1 (make-pi-mode-session :id "*pi[f1]*" :buffer b1 :process p1
                                          :project-root "/tmp/"
                                          :last-used (current-time)))
                (s2 (make-pi-mode-session :id "*pi[f2]*" :buffer b2 :process p2
                                          :project-root "/tmp/"
                                          :last-used (time-add (current-time) 10)))
                ;; more recent than both, but foreign: must be ignored
                (sf (make-pi-mode-session :id "*pi[ff]*" :buffer bf :process pf
                                          :project-root "/tmp/other/"
                                          :last-used (time-add (current-time) 100))))
            (pi-mode--register-session s1)
            (pi-mode--register-session s2)
            (pi-mode--register-session sf)
            (with-temp-buffer
              (should (eq (pi-mode--resolve-session nil nil) s2))))
        (pi-mode--unregister-session "*pi[f1]*")
        (pi-mode--unregister-session "*pi[f2]*")
        (pi-mode--unregister-session "*pi[ff]*")
        (kill-buffer b1) (kill-buffer b2) (kill-buffer bf)
        (delete-process p1) (delete-process p2) (delete-process pf)))))

(ert-deftest pi-mode-test-resolve-intent-prompt ()
  "INTENT prompt: ambiguous resolution prompts instead of MRU-guessing."
  (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/")))
    (let ((b1 (get-buffer-create "*pi[ip1]*"))
          (b2 (get-buffer-create "*pi[ip2]*"))
          (p1 (pi-mode-test--fake-process))
          (p2 (pi-mode-test--fake-process)))
      (unwind-protect
          (let ((s1 (make-pi-mode-session :id "*pi[ip1]*" :buffer b1 :process p1
                                          :project-root "/tmp/"
                                          :last-used (current-time)))
                (s2 (make-pi-mode-session :id "*pi[ip2]*" :buffer b2 :process p2
                                          :project-root "/tmp/"
                                          :last-used (time-add (current-time) 10))))
            (pi-mode--register-session s1)
            (pi-mode--register-session s2)
            ;; two sessions, neither in-buffer nor visible: prompt (mock
            ;; completing-read → s1, overriding the MRU s2)
            (with-temp-buffer
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "*pi[ip1]*")))
                (should (eq (pi-mode--resolve-session nil nil 'prompt) s1))))
            ;; sole session: no prompt even with intent prompt
            (pi-mode--unregister-session "*pi[ip2]*")
            (with-temp-buffer
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) (error "should not prompt"))))
                (should (eq (pi-mode--resolve-session nil nil 'prompt) s1))))
            ;; no-ask suppresses the intent prompt: MRU
            (pi-mode--register-session s2)
            (with-temp-buffer
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) (error "should not prompt"))))
                (should (eq (pi-mode--resolve-session nil t 'prompt) s2)))))
        (pi-mode--unregister-session "*pi[ip1]*")
        (pi-mode--unregister-session "*pi[ip2]*")
        (kill-buffer b1) (kill-buffer b2)
        (delete-process p1) (delete-process p2)))))


(ert-deftest pi-mode-test-insert-text ()
  "insert-text pastes without pressing return and runs before-send hook."
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
             (pi-mode--insert-text s "hello pi"))
           (should (equal hook-args (list s "hello pi")))
           (should (equal (cdr (assq 'ghostel-paste-string pi-mode-test--calls))
                          '("hello pi")))
           ;; insert-only: the text lands in the input box, nothing submitted
           (should-not (assq 'ghostel-send-key pi-mode-test--calls)))
       (pi-mode--unregister-session "*pi[send]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-insert-text-dead-session ()
  "insert-text to a dead session signals user-error."
  (pi-mode-test-with-mock-ghostel
   (let ((s (make-pi-mode-session :id "*pi[dead]*" :buffer (get-buffer-create "*pi[dead]*")
                                  :process (pi-mode-test--fake-process)
                                  :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (delete-process (pi-mode-session-process s))
           (should-error (pi-mode--insert-text s "x") :type 'user-error))
       (pi-mode--unregister-session "*pi[dead]*")
       (kill-buffer (pi-mode-session-buffer s))))))



(ert-deftest pi-mode-test-mode-line-session-name ()
  "Sessions contribute their name to the buffer's mode-line-misc-info."
  (let ((s (pi-mode--make-session "/tmp/ml-proj/" "refactor")))
    (unwind-protect
        (with-current-buffer (pi-mode-session-buffer s)
          (should (member '(:eval (pi-mode--mode-line-segment)) mode-line-misc-info))
          (should (equal (pi-mode--mode-line-segment) " refactor")))
      (kill-buffer (pi-mode-session-buffer s)))))

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
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/")))
             (with-current-buffer b (pi-mode-interrupt)))
           (should (equal (cdr (assq 'ghostel-send-key pi-mode-test--calls))
                          '("escape" nil))))
       (pi-mode--unregister-session "*pi[int]*")
       (kill-buffer b) (delete-process p)))))



(ert-deftest pi-mode-test-send-region-inserts-raw-content ()
  "send-region pastes the raw region content: no minibuffer prompt, no submit."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[sr]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[sr]*" :buffer b :process p
                                   :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-temp-buffer
             (insert "line1\nline2")
             (cl-letf (((symbol-function 'pi-mode--project-root)
                        (lambda () "/tmp/"))
                       ((symbol-function 'read-from-minibuffer)
                        (lambda (&rest _) (error "must not prompt"))))
               (pi-mode-send-region (point-min) (point-max))))
           (should (equal (cdr (assq 'ghostel-paste-string pi-mode-test--calls))
                          '("line1\nline2")))
           ;; insert-only: no return key was sent
           (should-not (assq 'ghostel-send-key pi-mode-test--calls)))
       (pi-mode--unregister-session "*pi[sr]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-send-region-requires-region ()
  "send-region without a region (point-min = point-max) errors."
  (pi-mode-test-with-mock-ghostel
   (should-error (pi-mode-send-region 1 1) :type 'user-error)))

(ert-deftest pi-mode-test-send-file-inserts-reference ()
  "send-file inserts an @-reference relative to the session's cwd."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[sf]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[sf]*" :buffer b :process p
                                   :project-root "/tmp/proj")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-temp-buffer
             (cl-letf (((symbol-function 'buffer-file-name)
                        (lambda () "/tmp/proj/sub/a.ts"))
                       ((symbol-function 'pi-mode--project-root)
                        (lambda () "/tmp/proj"))
                       ((symbol-function 'read-from-minibuffer)
                        (lambda (&rest _) (error "must not prompt"))))
               (pi-mode-send-file)))
           (should (equal (cdr (assq 'ghostel-paste-string pi-mode-test--calls))
                          '("@sub/a.ts")))
           ;; insert-only: no return key was sent
           (should-not (assq 'ghostel-send-key pi-mode-test--calls)))
       (pi-mode--unregister-session "*pi[sf]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-send-file-no-file ()
  "send-file in a buffer not visiting a file signals user-error."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[sfn]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[sfn]*" :buffer b :process p
                                   :project-root "/tmp/proj")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-temp-buffer
             (cl-letf (((symbol-function 'buffer-file-name) (lambda () nil)))
               (should-error (pi-mode-send-file) :type 'user-error))))
       (pi-mode--unregister-session "*pi[sfn]*")
       (kill-buffer b) (delete-process p)))))

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
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/")))
             (with-current-buffer b
               (pi-mode-session-rename "refactor")))
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
             (cl-letf (((symbol-function 'pi-mode--project-root)
                        (lambda () "/tmp/"))
                       ((symbol-function 'y-or-n-p) (lambda (_) t)))
               (pi-mode-session-stop))
             (should (not (process-live-p p))))
           ;; second half: decline with a fresh live process
           (let ((p2 (pi-mode-test--fake-process)))
             (setf (pi-mode-session-process s) p2)
             (pi-mode--register-session s)
             (with-current-buffer b
               (cl-letf (((symbol-function 'pi-mode--project-root)
                          (lambda () "/tmp/"))
                         ((symbol-function 'y-or-n-p) (lambda (_) nil)))
                 (pi-mode-session-stop))
               (should (process-live-p p2))
               (delete-process p2)))
           (pi-mode--unregister-session "*pi[st]*"))
       (kill-buffer b)
       (ignore-errors (delete-process p))))))

(ert-deftest pi-mode-test-session-stop-intent-prompt ()
  "stop prompts between project sessions and stops the chosen one."
  (pi-mode-test-with-mock-ghostel
   (let* ((b1 (get-buffer-create "*pi[sp1]*"))
          (b2 (get-buffer-create "*pi[sp2]*"))
          (p1 (pi-mode-test--fake-process))
          (p2 (pi-mode-test--fake-process))
          (s1 (make-pi-mode-session :id "*pi[sp1]*" :buffer b1 :process p1
                                    :project-root "/tmp/"
                                    :last-used (current-time)))
          (s2 (make-pi-mode-session :id "*pi[sp2]*" :buffer b2 :process p2
                                    :project-root "/tmp/"
                                    :last-used (time-add (current-time) 10))))
     (unwind-protect
         (progn
           (pi-mode--register-session s1)
           (pi-mode--register-session s2)
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/"))
                     ((symbol-function 'completing-read)
                      (lambda (&rest _) "*pi[sp1]*"))
                     ((symbol-function 'y-or-n-p) (lambda (_) t)))
             (with-temp-buffer
               (pi-mode-session-stop)))
           ;; the prompted-for s1 was stopped, not the MRU s2
           (should-not (process-live-p p1))
           (should (process-live-p p2)))
       (pi-mode--unregister-session "*pi[sp1]*")
       (pi-mode--unregister-session "*pi[sp2]*")
       (kill-buffer b1) (kill-buffer b2)
       (ignore-errors (delete-process p1))
       (ignore-errors (delete-process p2))))))

(ert-deftest pi-mode-test-session-rename-intent-prompt ()
  "rename prompts between project sessions and renames the chosen one."
  (pi-mode-test-with-mock-ghostel
   (let* ((b1 (get-buffer-create "*pi[rp1]*"))
          (b2 (get-buffer-create "*pi[rp2]*"))
          (p1 (pi-mode-test--fake-process))
          (p2 (pi-mode-test--fake-process))
          (s1 (make-pi-mode-session :id "*pi[rp1]*" :buffer b1 :process p1
                                    :project-root "/tmp/"
                                    :last-used (current-time)))
          (s2 (make-pi-mode-session :id "*pi[rp2]*" :buffer b2 :process p2
                                    :project-root "/tmp/"
                                    :last-used (time-add (current-time) 10))))
     (unwind-protect
         (progn
           (pi-mode--register-session s1)
           (pi-mode--register-session s2)
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/"))
                     ((symbol-function 'completing-read)
                      (lambda (&rest _) "*pi[rp1]*")))
             (with-temp-buffer
               (pi-mode-session-rename "refactor")))
           ;; the prompted-for s1 was renamed, not the MRU s2
           (should (equal (pi-mode-session-name s1) "refactor"))
           (should-not (pi-mode-session-name s2))
           (let ((call (assq 'ghostel-send-string pi-mode-test--calls)))
             (should call)
             (should (equal (car (cdr call)) "/name refactor"))))
       (pi-mode--unregister-session "*pi[rp1]*")
       (pi-mode--unregister-session "*pi[rp2]*")
       (kill-buffer b1) (kill-buffer b2)
       (delete-process p1) (delete-process p2)))))

(ert-deftest pi-mode-test-menu-defined ()
  "The transient menu and its suffix commands exist."
  (should (commandp 'pi-mode-menu))
  (should (commandp 'pi-mode-config-menu))
  (should (commandp 'pi-mode-debug-menu))
  (should (commandp 'pi-mode-status-menu))
  (should (commandp 'pi-mode-check-status))
  (should (commandp 'pi-mode-show-version-info))
  (dolist (cmd '(pi-mode-session-continue pi-mode-session-resume
                 pi-mode-session-fork pi-mode-session-rename
                 pi-mode-session-stop pi-mode-session-stop-all
                 pi-mode-list-sessions
                 pi-mode-send-region pi-mode-send-file
                 pi-mode-switch-buffer pi-mode-toggle-panel
                 pi-mode-show-all pi-mode-toggle-recent
                 pi-mode-edit-prompt pi-mode-prompt-edit-submit
                 pi-mode-prompt-edit-cancel
                 pi-mode-interrupt pi-mode-configure-model
                 pi-mode-configure-thinking pi-mode-configure-tui-mode
                 pi-mode-configure-cli-args
                 pi-mode-toggle-notifications))
    (should (commandp cmd))))

(ert-deftest pi-mode-test-install-keybindings-shim ()
  "The removed keybinding installer is an obsolete no-op compat shim.
Regression: stale use-package :config blocks calling
`pi-mode-install-keybindings' must not error (void-function)."
  (should (functionp 'pi-mode-install-keybindings))
  (should (get 'pi-mode-install-keybindings 'byte-obsolete-info))
  ;; The shim must return without signaling.
  (should (condition-case nil
              (progn (pi-mode-install-keybindings t) t)
            (error nil))))

(ert-deftest pi-mode-test-global-menu-binding ()
  "C-c C-' is bound to pi-mode-menu globally."
  (should (eq (lookup-key (current-global-map) (kbd "C-c C-'"))
              #'pi-mode-menu)))

(ert-deftest pi-mode-test-assign-window-slot ()
  "Slot assignment picks the smallest slot not used by live sessions."
  (let ((b1 (get-buffer-create "*pi[w1]*"))
        (b2 (get-buffer-create "*pi[w2]*"))
        (p1 (pi-mode-test--fake-process))
        (p2 (pi-mode-test--fake-process)))
    (unwind-protect
        (progn
          (should (= (pi-mode--assign-window-slot) 0))
          (let ((s1 (make-pi-mode-session :id "*pi[w1]*" :buffer b1 :process p1
                                          :project-root "/tmp/" :window-slot 0))
                (s2 (make-pi-mode-session :id "*pi[w2]*" :buffer b2 :process p2
                                          :project-root "/tmp/" :window-slot 1)))
            (pi-mode--register-session s1)
            (pi-mode--register-session s2)
            (should (= (pi-mode--assign-window-slot) 2))
            (pi-mode--unregister-session "*pi[w2]*")
            (should (= (pi-mode--assign-window-slot) 1))))
      (kill-buffer b1) (kill-buffer b2)
      (delete-process p1) (delete-process p2))))

(ert-deftest pi-mode-test-window-defaults ()
  "Window defaults match claude-code-ide.el: right side, 20 lines, 100 columns."
  (should (eq pi-mode-window-side 'right))
  (should (= pi-mode-window-height 20))
  (should (= pi-mode-window-width 100)))

(ert-deftest pi-mode-test-display-args ()
  "display-args resolves side, slot, and size from customization."
  (let ((b (get-buffer-create "*pi[da]*"))
        (p (pi-mode-test--fake-process)))
    (unwind-protect
        (let ((pi-mode-window-side 'bottom)
              (pi-mode-window-height 20)
              (pi-mode-window-width 100))
          (should (equal (pi-mode--display-args b)
                         '(bottom 0 window-height 20)))
          (let ((pi-mode-window-side 'right))
            (should (equal (pi-mode--display-args b)
                           '(right 0 window-width 100))))
          (let ((s (make-pi-mode-session :id "*pi[da]*" :buffer b :process p
                                         :project-root "/tmp/" :window-slot 3)))
            (pi-mode--register-session s)
            (should (equal (pi-mode--display-args b)
                           '(bottom 3 window-height 20)))
            (pi-mode--unregister-session "*pi[da]*")))
      (kill-buffer b) (delete-process p))))

(ert-deftest pi-mode-test-display-buffer-entry ()
  "Session buffers are displayed in a side window via the predicate condition.
Regression: a dotted (REGEXP . FUNCTION) entry makes `display-buffer'
error with `wrong type argument: listp' — the entry must be a proper
list so the action unwraps to a function list."
  (let ((b (get-buffer-create "*pi[entry]*")))
    (unwind-protect
        (with-current-buffer b
          (setq-local pi-mode--session (make-pi-mode-session :id "*pi[entry]*"))
          (let ((window (display-buffer b)))
            (should (windowp window))
            (should (eq (window-buffer window) b))
            (should (eq (window-parameter window 'window-side)
                        pi-mode-window-side))))
      (kill-buffer b))))

(ert-deftest pi-mode-test-display-buffer-non-session-ignored ()
  "Non-session buffers are not affected by the pi-mode alist entry."
  (let ((b (get-buffer-create "*plain*")))
    (unwind-protect
        (let ((window (display-buffer b)))
          (should (windowp window))
          (should-not (window-parameter window 'window-side)))
      (kill-buffer b))))

(ert-deftest pi-mode-test-window-commands-no-sessions ()
  "Window commands handle the no-sessions case by signaling `user-error'."
  (should-error (pi-mode-toggle-recent) :type 'user-error)
  (should-error (pi-mode-show-all) :type 'user-error)
  (should-error (pi-mode-toggle-panel) :type 'user-error))

(ert-deftest pi-mode-test-hidden-panel-set-get ()
  "Hidden sets are stored per (tab × project) key; nil drops the entry."
  (unwind-protect
      (progn
        (set-frame-parameter nil 'pi-mode-hidden-panel nil)
        (should-not (pi-mode--hidden-panel-get "/tmp/proj-a/"))
        (pi-mode--hidden-panel-set '(s1 s2) "/tmp/proj-a/")
        (should (equal (pi-mode--hidden-panel-get "/tmp/proj-a/") '(s1 s2)))
        (should-not (pi-mode--hidden-panel-get "/tmp/proj-b/")) ; other project
        (pi-mode--hidden-panel-set nil "/tmp/proj-a/")
        (should-not (pi-mode--hidden-panel-get "/tmp/proj-a/")))
    (set-frame-parameter nil 'pi-mode-hidden-panel nil)))

(ert-deftest pi-mode-test-hidden-panel-tab-isolation ()
  "Hidden sets are isolated per tab for the same project."
  (unwind-protect
      (progn
        (set-frame-parameter nil 'pi-mode-hidden-panel nil)
        (cl-letf (((symbol-function 'tab-bar--current-tab)
                   (lambda () '((name . "tab-a"))))
                  ((symbol-value 'tab-bar-mode) t))
          (pi-mode--hidden-panel-set '(s1) "/tmp/proj-a/")
          (should (equal (pi-mode--hidden-panel-get "/tmp/proj-a/") '(s1))))
        (cl-letf (((symbol-function 'tab-bar--current-tab)
                   (lambda () '((name . "tab-b"))))
                  ((symbol-value 'tab-bar-mode) t))
          (should-not (pi-mode--hidden-panel-get "/tmp/proj-a/")))
        (cl-letf (((symbol-function 'tab-bar--current-tab)
                   (lambda () '((name . "tab-a"))))
                  ((symbol-value 'tab-bar-mode) t))
          (should (equal (pi-mode--hidden-panel-get "/tmp/proj-a/") '(s1)))))
    (set-frame-parameter nil 'pi-mode-hidden-panel nil)))

(ert-deftest pi-mode-test-hidden-panel-per-project ()
  "Same tab: each project's hidden set is independent; nil drops only one."
  (unwind-protect
      (progn
        (set-frame-parameter nil 'pi-mode-hidden-panel nil)
        (pi-mode--hidden-panel-set '(a1 a2) "/tmp/proj-a/")
        (pi-mode--hidden-panel-set '(b1) "/tmp/proj-b/")
        ;; each project sees its own set in the same tab
        (should (equal (pi-mode--hidden-panel-get "/tmp/proj-b/") '(b1)))
        (should (equal (pi-mode--hidden-panel-get "/tmp/proj-a/") '(a1 a2)))
        ;; nil for B drops only B's entry; A's remains
        (pi-mode--hidden-panel-set nil "/tmp/proj-b/")
        (should-not (pi-mode--hidden-panel-get "/tmp/proj-b/"))
        (should (equal (pi-mode--hidden-panel-get "/tmp/proj-a/") '(a1 a2)))
        (let ((entries (frame-parameter nil 'pi-mode-hidden-panel)))
          (should (= (length entries) 1))))
    (set-frame-parameter nil 'pi-mode-hidden-panel nil)))

(ert-deftest pi-mode-test-hidden-panel-prunes-dead-tabs ()
  "Writing a new entry drops keys for tabs that no longer exist."
  (unwind-protect
      (cl-letf (((symbol-function 'tab-bar-tabs)
                 (lambda (&optional _frame)
                   ;; real tab-bar-tabs shape: (TAB-ID (name . NAME) ...)
                   '((1 (name . "live-1")) (2 (name . "live-2")))))
                ((symbol-function 'tab-bar--current-tab)
                 (lambda () '((name . "none"))))
                ((symbol-value 'tab-bar-mode) t))
        (set-frame-parameter nil 'pi-mode-hidden-panel
                             '((("dead-tab" . "/tmp/proj-a/") . (s1))
                               (("live-1" . "/tmp/proj-a/") . (s-old))))
        (pi-mode--hidden-panel-set '(s2) "/tmp/proj-a/")
        (let ((entries (frame-parameter nil 'pi-mode-hidden-panel)))
          ;; keys are (TAB . PROJECT) conses; the stubbed tab is "none"
          (should (assoc '("none" . "/tmp/proj-a/") entries))
          (should (assoc '("live-1" . "/tmp/proj-a/") entries)) ; still live
          (should-not (assoc '("dead-tab" . "/tmp/proj-a/") entries)) ; pruned
          ;; the current-tab write replaced any same-key entry only if the
          ;; key matches — current key is ("none" . root), so no clash
          (should (equal (cdr (assoc '("none" . "/tmp/proj-a/") entries))
                         '(s2)))))
    (set-frame-parameter nil 'pi-mode-hidden-panel nil)))

(ert-deftest pi-mode-test-hidden-panel-prunes-legacy-string-keys ()
  "Legacy string-keyed entries are dropped without signalling."
  (unwind-protect
      (cl-letf (((symbol-function 'tab-bar-tabs)
                 (lambda (&optional _frame)
                   '((1 (name . "live-1")))))
                ((symbol-function 'tab-bar--current-tab)
                 (lambda () '((name . "none"))))
                ((symbol-value 'tab-bar-mode) t))
        (set-frame-parameter nil 'pi-mode-hidden-panel
                             '(("old-tab" . (s-legacy))
                               (("live-1" . "/tmp/proj-a/") . (s-old))))
        ;; must not signal wrong-type-argument on (caar "old-tab")
        (pi-mode--hidden-panel-set '(s1) "/tmp/proj-a/")
        (let ((entries (frame-parameter nil 'pi-mode-hidden-panel)))
          (should-not (assoc "old-tab" entries)) ; legacy string key dropped
          (should (assoc '("live-1" . "/tmp/proj-a/") entries))
          (should (equal (cdr (assoc '("none" . "/tmp/proj-a/") entries))
                         '(s1)))))
    (set-frame-parameter nil 'pi-mode-hidden-panel nil)))

(ert-deftest pi-mode-test-toggle-panel-roundtrip ()
  "toggle-panel hides visible sessions and restores the same set."
  (pi-mode-test-with-mock-ghostel
   (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/")))
     (let* ((b1 (get-buffer-create "*pi[t1]*"))
            (b2 (get-buffer-create "*pi[t2]*"))
            (p1 (pi-mode-test--fake-process))
            (p2 (pi-mode-test--fake-process))
            (s1 (make-pi-mode-session :id "*pi[t1]*" :buffer b1 :process p1
                                      :project-root "/tmp/" :window-slot 0
                                      ;; distinct timestamps: active-sessions sorts
                                      ;; most-recent-first (s2), keeping the
                                      ;; expected set order deterministic
                                      :last-used (time-subtract (current-time) 10)))
            (s2 (make-pi-mode-session :id "*pi[t2]*" :buffer b2 :process p2
                                      :project-root "/tmp/" :window-slot 1
                                      :last-used (current-time)))
            (window-sides-slots '(nil nil 4 nil)))
       (unwind-protect
           (progn
             (pi-mode--register-session s1)
             (pi-mode--register-session s2)
             (with-current-buffer b1 (setq-local pi-mode--session s1))
             (with-current-buffer b2 (setq-local pi-mode--session s2))
             (display-buffer b1)
             (display-buffer b2)
             (should (get-buffer-window b1))
             (should (get-buffer-window b2))
             (should (equal (pi-mode-toggle-panel) :hidden))
             (should-not (get-buffer-window b1))
             (should-not (get-buffer-window b2))
             (should (equal (pi-mode--hidden-panel-get) (list s2 s1)))
             (should (equal (pi-mode-toggle-panel) :shown))
             (should (get-buffer-window b1))
             (should (get-buffer-window b2)))
         (pi-mode--unregister-session "*pi[t1]*")
         (pi-mode--unregister-session "*pi[t2]*")
         (kill-buffer b1) (kill-buffer b2)
         (delete-process p1) (delete-process p2))))))

(ert-deftest pi-mode-test-toggle-panel-restore-skips-dead ()
  "Restore drops dead remembered sessions and falls back to the MRU live one."
  (pi-mode-test-with-mock-ghostel
   (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/")))
     (let* ((b1 (get-buffer-create "*pi[td1]*"))
            (b2 (get-buffer-create "*pi[td2]*"))
            (p1 (pi-mode-test--fake-process))
            (p2 (pi-mode-test--fake-process))
            (s1 (make-pi-mode-session :id "*pi[td1]*" :buffer b1 :process p1
                                      :project-root "/tmp/" :window-slot 0
                                      :last-used (current-time)))
            (s2 (make-pi-mode-session :id "*pi[td2]*" :buffer b2 :process p2
                                      :project-root "/tmp/" :window-slot 1
                                      :last-used (current-time)))
            (window-sides-slots '(nil nil 4 nil)))
       (unwind-protect
           (progn
             (pi-mode--register-session s1)
             (pi-mode--register-session s2)
             (with-current-buffer b1 (setq-local pi-mode--session s1))
             (with-current-buffer b2 (setq-local pi-mode--session s2))
             (display-buffer b1)
             (pi-mode-toggle-panel)               ; hide s1, remembered set = (s1)
             (delete-process p1)                  ; s1 dies while hidden
             ;; the remembered set holds the now-dead s1: restore must skip it
             ;; via the live-session filter and fall back to the MRU live
             ;; session (s2)
             (pi-mode--hidden-panel-set (list s1))
             (should (equal (pi-mode-toggle-panel) :shown))
             (should (get-buffer-window b2))      ; s2 restored via MRU fallback
             (should-not (get-buffer-window b1))) ; dead s1 skipped — no window
         (pi-mode--unregister-session "*pi[td1]*")
         (pi-mode--unregister-session "*pi[td2]*")
         (kill-buffer b1) (kill-buffer b2)
         (ignore-errors (delete-process p1))
         (ignore-errors (delete-process p2)))))))

(ert-deftest pi-mode-test-project-sessions-scope-and-order ()
  "project-sessions returns the project's live sessions, MRU-first."
  (pi-mode-test-with-mock-ghostel
   (let* ((b1 (get-buffer-create "*pi[ps1]*"))
          (b2 (get-buffer-create "*pi[ps2]*"))
          (b3 (get-buffer-create "*pi[ps3]*"))
          (p1 (pi-mode-test--fake-process))
          (p2 (pi-mode-test--fake-process))
          (p3 (pi-mode-test--fake-process))
          (s1 (make-pi-mode-session :id "*pi[ps1]*" :buffer b1 :process p1
                                    :project-root "/tmp/ps-a/" :window-slot 0
                                    :last-used (time-subtract (current-time) 10)))
          (s2 (make-pi-mode-session :id "*pi[ps2]*" :buffer b2 :process p2
                                    :project-root "/tmp/ps-a/" :window-slot 1
                                    :last-used (current-time)))
          (s3 (make-pi-mode-session :id "*pi[ps3]*" :buffer b3 :process p3
                                    :project-root "/tmp/ps-b/" :window-slot 2
                                    :last-used (current-time))))
     (unwind-protect
         (progn
           (pi-mode--register-session s1)
           (pi-mode--register-session s2)
           (pi-mode--register-session s3)
           (should (equal (pi-mode--project-sessions "/tmp/ps-a/")
                          (list s2 s1)))
           (should (equal (pi-mode--project-sessions "/tmp/ps-b/") (list s3)))
           (should-not (pi-mode--project-sessions "/tmp/ps-c/"))
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/ps-a/")))
             (should (equal (pi-mode--project-sessions) (list s2 s1)))))
       (pi-mode--unregister-session "*pi[ps1]*")
       (pi-mode--unregister-session "*pi[ps2]*")
       (pi-mode--unregister-session "*pi[ps3]*")
       (kill-buffer b1) (kill-buffer b2) (kill-buffer b3)
       (delete-process p1) (delete-process p2) (delete-process p3)))))

(ert-deftest pi-mode-test-toggle-panel-hides-only-current-project ()
  "Hiding scopes to the current project; foreign windows stay."
  (pi-mode-test-with-mock-ghostel
   (let* ((b1 (get-buffer-create "*pi[pa1]*"))
          (b2 (get-buffer-create "*pi[pa2]*"))
          (bf (get-buffer-create "*pi[pb]*"))
          (p1 (pi-mode-test--fake-process))
          (p2 (pi-mode-test--fake-process))
          (pf (pi-mode-test--fake-process))
          (s1 (make-pi-mode-session :id "*pi[pa1]*" :buffer b1 :process p1
                                    :project-root "/tmp/proj-a/" :window-slot 0
                                    ;; distinct timestamps: active-sessions sorts
                                    ;; most-recent-first (s2), keeping the
                                    ;; expected set order deterministic
                                    :last-used (time-subtract (current-time) 10)))
          (s2 (make-pi-mode-session :id "*pi[pa2]*" :buffer b2 :process p2
                                    :project-root "/tmp/proj-a/" :window-slot 1
                                    :last-used (current-time)))
          (sf (make-pi-mode-session :id "*pi[pb]*" :buffer bf :process pf
                                    :project-root "/tmp/proj-b/" :window-slot 2
                                    :last-used (current-time)))
          (window-sides-slots '(nil nil 4 nil)))
     (unwind-protect
         (progn
           (pi-mode--register-session s1)
           (pi-mode--register-session s2)
           (pi-mode--register-session sf)
           (with-current-buffer b1 (setq-local pi-mode--session s1))
           (with-current-buffer b2 (setq-local pi-mode--session s2))
           (with-current-buffer bf (setq-local pi-mode--session sf))
           (display-buffer b1)
           (display-buffer b2)
           (display-buffer bf)
           (should (get-buffer-window b1))
           (should (get-buffer-window b2))
           (should (get-buffer-window bf))
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/proj-a/")))
             (should (equal (pi-mode-toggle-panel) :hidden))
             (should-not (get-buffer-window b1))
             (should-not (get-buffer-window b2))
             (should (get-buffer-window bf))      ; foreign window untouched
             (should (equal (pi-mode--hidden-panel-get) (list s2 s1)))
             (should (equal (pi-mode-toggle-panel) :shown))
             (should (get-buffer-window b1))
             (should (get-buffer-window b2))
             (should (get-buffer-window bf))))
       (pi-mode--unregister-session "*pi[pa1]*")
       (pi-mode--unregister-session "*pi[pa2]*")
       (pi-mode--unregister-session "*pi[pb]*")
       (kill-buffer b1) (kill-buffer b2) (kill-buffer bf)
       (delete-process p1) (delete-process p2) (delete-process pf)))))

(ert-deftest pi-mode-test-toggle-panel-restore-scoped-fallback ()
  "Restore with only dead/foreign remembered sessions falls back to the
current project's MRU; a foreign buffer is never displayed."
  (pi-mode-test-with-mock-ghostel
   (let* ((b-dead (get-buffer-create "*pi[pdead]*"))
          (b-mru (get-buffer-create "*pi[pmru]*"))
          (b-foreign (get-buffer-create "*pi[pfor]*"))
          (p-dead (pi-mode-test--fake-process))
          (p-mru (pi-mode-test--fake-process))
          (p-foreign (pi-mode-test--fake-process))
          (s-dead (make-pi-mode-session :id "*pi[pdead]*" :buffer b-dead
                                        :process p-dead
                                        :project-root "/tmp/proj-a/" :window-slot 0
                                        :last-used (current-time)))
          (s-mru (make-pi-mode-session :id "*pi[pmru]*" :buffer b-mru
                                       :process p-mru
                                       :project-root "/tmp/proj-a/" :window-slot 1
                                       :last-used (current-time)))
          (s-foreign (make-pi-mode-session :id "*pi[pfor]*" :buffer b-foreign
                                           :process p-foreign
                                           :project-root "/tmp/proj-b/" :window-slot 2
                                           ;; newest session overall: a global-MRU
                                           ;; restore fallback would pick it
                                           :last-used (time-add (current-time) 5)))
          (window-sides-slots '(nil nil 4 nil)))
     (unwind-protect
         (progn
           (pi-mode--register-session s-dead)
           (pi-mode--register-session s-mru)
           (pi-mode--register-session s-foreign)
           (with-current-buffer b-dead (setq-local pi-mode--session s-dead))
           (with-current-buffer b-mru (setq-local pi-mode--session s-mru))
           (with-current-buffer b-foreign (setq-local pi-mode--session s-foreign))
           (delete-process p-dead)                 ; s-dead dies while hidden
           (pi-mode--hidden-panel-set (list s-dead s-foreign) "/tmp/proj-a/")
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/proj-a/")))
             (should (equal (pi-mode-toggle-panel) :shown))
             (should (get-buffer-window b-mru))  ; project MRU restored
             (should-not (get-buffer-window b-dead)) ; dead skipped
             (should-not (get-buffer-window b-foreign)))) ; foreign never shown
       (pi-mode--unregister-session "*pi[pdead]*")
       (pi-mode--unregister-session "*pi[pmru]*")
       (pi-mode--unregister-session "*pi[pfor]*")
       (kill-buffer b-dead) (kill-buffer b-mru) (kill-buffer b-foreign)
       (ignore-errors (delete-process p-dead))
       (ignore-errors (delete-process p-mru))
       (ignore-errors (delete-process p-foreign))))))

(ert-deftest pi-mode-test-toggle-panel-per-project-hidden-sets ()
  "Same tab: hiding A then B keeps both sets; each project restores its own."
  (pi-mode-test-with-mock-ghostel
   (let* ((b-a1 (get-buffer-create "*pi[ppA1]*"))
          (b-a2 (get-buffer-create "*pi[ppA2]*"))
          (b-b1 (get-buffer-create "*pi[ppB1]*"))
          (p-a1 (pi-mode-test--fake-process))
          (p-a2 (pi-mode-test--fake-process))
          (p-b1 (pi-mode-test--fake-process))
          (s-a1 (make-pi-mode-session :id "*pi[ppA1]*" :buffer b-a1 :process p-a1
                                      :project-root "/tmp/proj-a/" :window-slot 0
                                      ;; distinct timestamps: active-sessions sorts
                                      ;; most-recent-first (s-a2)
                                      :last-used (time-subtract (current-time) 10)))
          (s-a2 (make-pi-mode-session :id "*pi[ppA2]*" :buffer b-a2 :process p-a2
                                      :project-root "/tmp/proj-a/" :window-slot 1
                                      :last-used (current-time)))
          (s-b1 (make-pi-mode-session :id "*pi[ppB1]*" :buffer b-b1 :process p-b1
                                      :project-root "/tmp/proj-b/" :window-slot 2
                                      :last-used (current-time)))
          (window-sides-slots '(nil nil 4 nil)))
     (unwind-protect
         (progn
           (pi-mode--register-session s-a1)
           (pi-mode--register-session s-a2)
           (pi-mode--register-session s-b1)
           (with-current-buffer b-a1 (setq-local pi-mode--session s-a1))
           (with-current-buffer b-a2 (setq-local pi-mode--session s-a2))
           (with-current-buffer b-b1 (setq-local pi-mode--session s-b1))
           (display-buffer b-a1)
           (display-buffer b-a2)
           (display-buffer b-b1)
           ;; hide A's panel: A's two windows go, B's window stays
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/proj-a/")))
             (should (equal (pi-mode-toggle-panel) :hidden))
             (should-not (get-buffer-window b-a1))
             (should-not (get-buffer-window b-a2))
             (should (get-buffer-window b-b1)))
           ;; hide B's panel in the same tab: must not clobber A's set
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/proj-b/")))
             (should (equal (pi-mode-toggle-panel) :hidden))
             (should-not (get-buffer-window b-b1)))
           ;; B restores B's remembered set, not A's
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/proj-b/")))
             (should (equal (pi-mode-toggle-panel) :shown))
             (should (get-buffer-window b-b1))
             (should-not (get-buffer-window b-a1)))
           ;; A restores A's remembered set (s-a2 s-a1) — not merely B's MRU
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/proj-a/")))
             (should (equal (pi-mode-toggle-panel) :shown))
             (should (get-buffer-window b-a1))
             (should (get-buffer-window b-a2))
             (should (get-buffer-window b-b1))))
       (pi-mode--unregister-session "*pi[ppA1]*")
       (pi-mode--unregister-session "*pi[ppA2]*")
       (pi-mode--unregister-session "*pi[ppB1]*")
       (kill-buffer b-a1) (kill-buffer b-a2) (kill-buffer b-b1)
       (delete-process p-a1) (delete-process p-a2) (delete-process p-b1)))))

(ert-deftest pi-mode-test-toggle-panel-no-current-project-sessions ()
  "Toggle errors for a project with no live sessions, foreign ones
notwithstanding; the foreign window stays."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[pnop]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[pnop]*" :buffer b :process p
                                   :project-root "/tmp/proj-b/" :window-slot 0))
          (window-sides-slots '(nil nil 4 nil)))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-current-buffer b (setq-local pi-mode--session s))
           (display-buffer b)
           (should (get-buffer-window b))
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/proj-a/")))
             (let ((err (should-error (pi-mode-toggle-panel)
                                       :type 'user-error)))
               (should (string-match-p "proj-a" (cadr err))))
             (should (get-buffer-window b))))     ; foreign window untouched
       (pi-mode--unregister-session "*pi[pnop]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-strip-new-tab ()
  "New-tab stripping removes pi windows from the fresh tab."
  ;; pi-mode-confirm-kill nil: the kill-buffer guard would prompt (and
  ;; fail on stdin EOF in batch), masking the real test condition
  (let* ((pi-mode-confirm-kill nil)
         (b (get-buffer-create "*pi[strip]*"))
         (p (pi-mode-test--fake-process))
         (s (make-pi-mode-session :id "*pi[strip]*" :buffer b :process p
                                  :project-root "/tmp/")))
    (unwind-protect
        (progn
          (with-current-buffer b (setq-local pi-mode--session s))
          (display-buffer b)
          (should (get-buffer-window b))
          (pi-mode--strip-new-tab-pi-windows)
          (should-not (get-buffer-window b)))
      (kill-buffer b) (delete-process p))))

(ert-deftest pi-mode-test-strip-new-tab-leaves-others ()
  "New-tab stripping leaves non-pi windows alone."
  (let ((b (get-buffer-create "*plain*")))
    (unwind-protect
        (progn
          (display-buffer b)
          (pi-mode--strip-new-tab-pi-windows)
          (should (get-buffer-window b)))
      (kill-buffer b))))

(ert-deftest pi-mode-test-note-window-selection-stamps-mru ()
  "Selecting a pi window makes its session most-recently-used."
  (let* ((pi-mode-confirm-kill nil)
         (b (get-buffer-create "*pi[mru]*"))
         (p (pi-mode-test--fake-process))
         (s (make-pi-mode-session :id "*pi[mru]*" :buffer b :process p
                                  :project-root "/tmp/"
                                  :last-used (time-subtract (current-time) 5))))
    (unwind-protect
        (progn
          ;; pi-mode--note-window-selection resolves sessions through the
          ;; pi-mode--sessions registry, so the session must be registered
          (pi-mode--register-session s)
          (with-current-buffer b (setq-local pi-mode--session s))
          (let ((win (display-buffer b)))
            (select-window win)
            (pi-mode--note-window-selection (selected-frame))
            (should (time-less-p (time-subtract (current-time) 5)
                                 (pi-mode-session-last-used s)))))
      (pi-mode--unregister-session "*pi[mru]*")
      (kill-buffer b) (delete-process p))))

(ert-deftest pi-mode-test-note-window-selection-ignores-others ()
  "Selecting a non-pi window does not touch session MRU."
  (let* ((pi-mode-confirm-kill nil)
         (b (get-buffer-create "*plain*"))
         (p (pi-mode-test--fake-process))
         (sb (get-buffer-create "*pi[mru2]*"))
         (s (make-pi-mode-session :id "*pi[mru2]*" :buffer sb :process p
                                  :project-root "/tmp/" :last-used (current-time))))
    (unwind-protect
        (progn
          (let ((old (pi-mode-session-last-used s)))
            (select-window (display-buffer b))
            (pi-mode--note-window-selection (selected-frame))
            (should (equal old (pi-mode-session-last-used s)))))
      (kill-buffer sb) (kill-buffer b) (delete-process p))))

(ert-deftest pi-mode-test-launch-restores-locals-after-ghostel-mode ()
  "Launch re-applies session locals wiped by ghostel-mode activation.
Regression: real `ghostel-exec' activates `ghostel-mode', whose
`kill-all-local-variables' wipes `pi-mode--session' and the mode-line
segment; the display-buffer predicate then misses and sessions lose
their side window (e2e pi-mode-e2e-test-window-side-and-panel)."
  (pi-mode-test-with-mock-ghostel
   ;; Simulate the real ghostel wipe inside the mocked exec
   (cl-letf (((symbol-function 'ghostel-exec)
              (lambda (buffer _program &optional _args)
                (with-current-buffer buffer
                  (kill-all-local-variables))
                (pi-mode-test--fake-process))))
     (let* ((session (pi-mode--launch-buffer "/tmp/wipe-proj/" pi-mode-cli-args))
            (buffer (pi-mode-session-buffer session)))
       (unwind-protect
           (progn
             (should (eq (buffer-local-value 'pi-mode--session buffer) session))
             (should (buffer-local-value 'pi-mode buffer))
             (should (pi-mode--session-buffer-p buffer))
             (should (pi-mode--session-buffer-p (buffer-name buffer)))
             (let ((win (get-buffer-window buffer)))
               (should win)
               (should (eq (window-parameter win 'window-side)
                           pi-mode-window-side)))
             (should (string-match-p "pi-mode--mode-line-segment"
                                     (format "%S" (buffer-local-value
                                                   'mode-line-misc-info buffer)))))
         (kill-buffer buffer))))))

(ert-deftest pi-mode-test-session-buffer-p-registry-fallback ()
  "The predicate matches registered sessions even when the local was wiped."
  (let* ((b (get-buffer-create "*pi[reg]*"))
         (p (pi-mode-test--fake-process))
         (s (make-pi-mode-session :id "*pi[reg]*" :buffer b :process p
                                  :project-root "/tmp/")))
    (unwind-protect
        (progn
          (pi-mode--register-session s)
          (should (pi-mode--session-buffer-p b))
          (should (pi-mode--session-buffer-p "*pi[reg]*")))
      (pi-mode--unregister-session "*pi[reg]*")
      (kill-buffer b) (delete-process p))))

;;; Configure commands

(ert-deftest pi-mode-test-configure-model ()
  "pi-mode-configure-model sends /model."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[cm]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[cm]*" :buffer b :process p
                                   :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/")))
             (with-current-buffer b (pi-mode-configure-model "gpt-5.1")))
           (let ((call (assq 'ghostel-send-string pi-mode-test--calls)))
             (should call)
             (should (equal (car (cdr call)) "/model gpt-5.1")))
           ;; the submit (return) must follow the /model command
           (let ((call (assq 'ghostel-send-key pi-mode-test--calls)))
             (should call)
             (should (equal (cdr call) '("return")))))
       (pi-mode--unregister-session "*pi[cm]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-configure-thinking ()
  "pi-mode-configure-thinking sends shift+tab."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[ct]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[ct]*" :buffer b :process p
                                   :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/")))
             (with-current-buffer b (pi-mode-configure-thinking)))
           (should (equal (cdr (assq 'ghostel-send-key pi-mode-test--calls))
                          '("tab" "shift"))))
       (pi-mode--unregister-session "*pi[ct]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-configure-tui-mode ()
  "pi-mode-configure-tui-mode flips --tui-mode and relaunches."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[tu]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[tu]*" :buffer b :process p
                                   :project-root "/tmp/"))
          (pi-mode-cli-args '("--tui-mode" "regular"))
          (launch-calls nil))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-current-buffer b
             (cl-letf (((symbol-function 'pi-mode--project-root)
                        (lambda () "/tmp/"))
                       ((symbol-function 'y-or-n-p) (lambda (_) t))
                       ((symbol-function 'delete-process) (lambda (&rest _) nil))
                       ((symbol-function 'pi-mode--launch-buffer)
                        (lambda (root args &optional _name)
                          (push (list root args) launch-calls))))
               ;; toggle regular -> fullscreen, then fullscreen -> regular
               (pi-mode-configure-tui-mode)
               (pi-mode-configure-tui-mode)))
           (should (equal (nreverse launch-calls)
                          (list (list "/tmp/" '("--tui-mode" "fullscreen"))
                                (list "/tmp/" '("--tui-mode" "regular"))))))
       (pi-mode--unregister-session "*pi[tu]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-cli-version-found ()
  "pi-mode--cli-version runs pi --version and caches the result."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "pi") "/usr/bin/pi")))
            ((symbol-function 'call-process)
             (lambda (&rest args)
               (let ((dest (nth 2 args)))
                 (when (or (eq dest t) (bufferp dest))
                   (with-current-buffer (if (bufferp dest) dest (current-buffer))
                     (insert "0.84.1\n")))
                 0))))
    (let ((pi-mode--cli-cache nil))
      (should (equal (pi-mode--cli-version) "0.84.1"))
      (should (equal pi-mode--cli-cache '("/usr/bin/pi" . "0.84.1"))))))

(ert-deftest pi-mode-test-cli-version-missing ()
  "pi-mode--cli-version is nil when the CLI is absent."
  (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) nil)))
    (let ((pi-mode--cli-cache nil))
      (should-not (pi-mode--cli-version))
      (should-not pi-mode--cli-cache))))

(ert-deftest pi-mode-test-cli-status-strings ()
  "pi-mode--cli-status formats found and missing states."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "pi") "/usr/bin/pi")))
            ((symbol-function 'call-process)
             (lambda (&rest args)
               (when (eq (nth 2 args) t) (insert "0.84.1\n"))
               0)))
    (let ((pi-mode--cli-cache nil))
      (should (equal (pi-mode--cli-status) "pi 0.84.1 found at /usr/bin/pi")))
    (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) nil)))
      (should (equal (pi-mode--cli-status) "pi CLI not found in exec-path")))))

(ert-deftest pi-mode-test-check-status-message ()
  "pi-mode-check-status messages the CLI status."
  (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) "/usr/bin/pi"))
            ((symbol-function 'call-process)
             (lambda (&rest args)
               (when (eq (nth 2 args) t) (insert "0.84.1\n"))
               0))
            ((symbol-function 'message)
             (lambda (fmt &rest args) (apply #'format fmt args))))
    (let ((pi-mode--cli-cache nil))
      (should (equal (pi-mode-check-status)
                     "pi 0.84.1 found at /usr/bin/pi")))))

(ert-deftest pi-mode-test-version-info-buffer ()
  "pi-mode-show-version-info fills *pi-mode-status*."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[vi]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[vi]*" :buffer b :process p
                                   :project-root "/tmp/vi-proj/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) nil)))
             (pi-mode-show-version-info)
             (let ((info (with-current-buffer "*pi-mode-status*" (buffer-string))))
               (should (string-match-p "pi-mode" info))
               (should (string-match-p "Emacs" info))
               (should (string-match-p "\\*pi\\[vi\\]*" info))
               (should (string-match-p "vi-proj" info))
               (should (string-match-p "not found" info))))
           (should (buffer-local-value 'buffer-read-only
                                       (get-buffer "*pi-mode-status*"))))
       (pi-mode--unregister-session "*pi[vi]*")
       (kill-buffer b) (delete-process p)
       (when (get-buffer "*pi-mode-status*") (kill-buffer "*pi-mode-status*"))))))

(ert-deftest pi-mode-test-session-status-none ()
  "Header shows the no-sessions state."
  (cl-letf (((symbol-function 'pi-mode--project-root) (lambda () "/tmp/empty-proj/")))
    (should (string-match-p "No active sessions" (pi-mode--session-status)))))

(ert-deftest pi-mode-test-session-status-with-sessions ()
  "Header shows the current project's sessions."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[hs]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[hs]*" :buffer b :process p
                                   :project-root "/tmp/hs-proj/" :name "refactor")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/hs-proj/")))
             (let ((status (pi-mode--session-status)))
               (should (string-match-p "hs-proj" status))
               (should (string-match-p "refactor" status))
               (should (string-match-p "1 session" status))))
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/elsewhere/")))
             (should (string-match-p "running elsewhere" (pi-mode--session-status))))
           ;; No sessions at all
           (pi-mode--unregister-session "*pi[hs]*")
           (cl-letf (((symbol-function 'pi-mode--project-root)
                      (lambda () "/tmp/hs-proj/")))
             (should (string-match-p "No active sessions" (pi-mode--session-status)))))
       (pi-mode--unregister-session "*pi[hs]*")
       (kill-buffer b) (delete-process p)))))

;;; Prompt editing tests

(defun pi-mode-test--screen (width &rest content)
  "Build a fake pi TUI screen: a ─ border, CONTENT rows, a ─ border."
  (let ((border (make-string width ?─)))
    (append (list border) content (list border))))

(ert-deftest pi-mode-test-prompt-border-row-p ()
  "Border detection covers full and scrolled indicator rows."
  (should (pi-mode--prompt-border-row-p "──────"))
  (should (pi-mode--prompt-border-row-p "─── ↑ 2 more ─────"))
  (should (pi-mode--prompt-border-row-p "─── ↓ 5 more ─────"))
  (should-not (pi-mode--prompt-border-row-p "  hello"))
  (should-not (pi-mode--prompt-border-row-p "─── not a border")))

(ert-deftest pi-mode-test-prompt-extract-single-line ()
  "A single input row inside the borders is the prompt."
  (let* ((rows (pi-mode-test--screen 10 "hello"))
         (got (pi-mode--prompt-extract rows 1)))
    (should (equal got '("hello" . nil)))))

(ert-deftest pi-mode-test-prompt-extract-wrapped-line ()
  "A row that exactly fills the layout width joins its successor (wrap)."
  ;; width 10, default padding 0 → layout width 9 (one column reserved
  ;; for the cursor): the first row is full.
  (let* ((rows (pi-mode-test--screen 10 "abcdefghi" "jk"))
         (got (pi-mode--prompt-extract rows 1)))
    (should (equal got '("abcdefghijk" . nil)))))

(ert-deftest pi-mode-test-prompt-extract-multiline ()
  "Logical lines inside the input box are joined with newlines."
  (let* ((rows (pi-mode-test--screen 10 "line1" "line2"))
         (got (pi-mode--prompt-extract rows 1)))
    (should (equal got '("line1\nline2" . nil)))))

(ert-deftest pi-mode-test-prompt-extract-empty-input ()
  "An empty input box extracts an empty prompt."
  (let* ((rows (pi-mode-test--screen 10 ""))
         (got (pi-mode--prompt-extract rows 1)))
    (should (equal got '("" . nil)))))

(ert-deftest pi-mode-test-prompt-extract-scrolled ()
  "Scrolled editor borders mark the capture as partial."
  (let* ((rows (list "─── ↑ 2 more ─────" "hello" (make-string 10 ?─)))
         (got (pi-mode--prompt-extract rows 1)))
    (should (equal (car got) "hello"))
    (should (cdr got)))
  (let* ((rows (list (make-string 10 ?─) "hello" "─── ↓ 3 more ─────"))
         (got (pi-mode--prompt-extract rows 1)))
    (should (equal (car got) "hello"))
    (should (cdr got))))

(ert-deftest pi-mode-test-prompt-extract-no-input-box ()
  "No border framing the cursor row means the input box is not visible."
  (should-not (pi-mode--prompt-extract '("hello" "world") 1))
  ;; cursor on a border row itself
  (should-not (pi-mode--prompt-extract (pi-mode-test--screen 10 "x") 0))
  ;; cursor on the bottom border
  (should-not (pi-mode--prompt-extract (pi-mode-test--screen 10 "x") 2)))

(ert-deftest pi-mode-test-prompt-editor-padding ()
  "Padding resolution: project settings, then global settings, then fallback."
  (let* ((agent-dir (make-temp-file "pi-pad-agent-" t))
         (project (make-temp-file "pi-pad-proj-" t))
         (global (expand-file-name "settings.json" agent-dir))
         (project-file (expand-file-name ".pi/settings.json" project))
         (session (make-pi-mode-session :id "*pi[pad]*"
                                        :buffer (get-buffer-create "*pi[pad]*")
                                        :process (pi-mode-test--fake-process)
                                        :project-root project)))
    (unwind-protect
        (progn
          (setenv "PI_CODING_AGENT_DIR" agent-dir)
          ;; No settings files: the defcustom fallback.
          (let ((pi-mode-prompt-editor-padding-x 2))
            (should (= (pi-mode--prompt-editor-padding session) 2)))
          ;; Global settings only.
          (write-region "{\"editorPaddingX\": 1}" nil global)
          (should (= (pi-mode--prompt-editor-padding session) 1))
          ;; Project settings win over global.
          (make-directory (file-name-directory project-file) t)
          (write-region "{\"editorPaddingX\": 3}" nil project-file)
          (should (= (pi-mode--prompt-editor-padding session) 3))
          ;; Malformed JSON falls back.
          (write-region "{not json" nil project-file)
          (should (= (pi-mode--prompt-editor-padding session) 1)))
      (setenv "PI_CODING_AGENT_DIR" nil)
      (delete-directory agent-dir t)
      (delete-directory project t)
      (kill-buffer "*pi[pad]*"))))

(ert-deftest pi-mode-test-prompt-screen-rows ()
  "Screen rows are read from the viewport with the cursor row index."
  (with-temp-buffer
    (insert "scrollback line\n───\n  hello\n───\n")
    (let* ((vp-start (+ (point-min) (length "scrollback line\n")))
           (cursor-pos (+ (point-min) (string-match "hello" (buffer-string)))))
      (cl-letf (((symbol-function 'ghostel--viewport-start)
                 (lambda () vp-start))
                ((symbol-function 'ghostel-cursor-point)
                 (lambda () cursor-pos))
                ((symbol-function 'ghostel--viewport-row-at)
                 (lambda (_pos) 1)))
        (let ((got (pi-mode--prompt-screen-rows)))
          (should (equal (car got) '("───" "  hello" "───")))
          (should (equal (cdr got) 1)))))))

(ert-deftest pi-mode-test-edit-prompt-opens-popup ()
  "edit-prompt opens a markdown popup seeded with the current prompt."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[pe]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[pe]*" :buffer b :process p
                                   :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-current-buffer b
             (insert "scrollback\n──────\nhello pi\n──────\n")
             (cl-letf (((symbol-function 'ghostel--viewport-start)
                        (lambda () (point-min)))
                       ((symbol-function 'ghostel-cursor-point)
                        (lambda () (+ (point-min) (length "scrollback\n──────\n"))))
                       ((symbol-function 'ghostel--viewport-row-at)
                        (lambda (_pos) 2))
                       ((symbol-function 'pi-mode--project-root)
                        (lambda () "/tmp/")))
               (pi-mode-edit-prompt)))
           (let ((popup (get-buffer "*pi prompt *pi[pe]**")))
             (should popup)
             (should (equal (with-current-buffer popup (buffer-string))
                            "hello pi"))
             (should (with-current-buffer popup pi-mode-prompt-edit-mode))
             (should (with-current-buffer popup
                       (string-match-p "Finish" (or header-line-format ""))))
             (should (eq (with-current-buffer popup pi-mode--prompt-edit-session)
                         s))
             (should (with-current-buffer popup (derived-mode-p 'text-mode)))
             (should (get-buffer-window popup)))
           (kill-buffer "*pi prompt *pi[pe]**"))
       (pi-mode--unregister-session "*pi[pe]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-edit-prompt-reopen-switches ()
  "A second edit-prompt call switches to the existing popup instead of duplicating."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[pe2]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[pe2]*" :buffer b :process p
                                   :project-root "/tmp/")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-current-buffer b
             (insert "──────\nhi\n──────\n")
             (cl-letf (((symbol-function 'ghostel--viewport-start)
                        (lambda () (point-min)))
                       ((symbol-function 'ghostel-cursor-point)
                        (lambda () (+ (point-min) (length "──────\n"))))
                       ((symbol-function 'ghostel--viewport-row-at)
                        (lambda (_pos) 1))
                       ;; the second call runs from the popup buffer,
                       ;; where resolution is project-scoped
                       ((symbol-function 'pi-mode--project-root)
                        (lambda () "/tmp/")))
               (pi-mode-edit-prompt)
               (pi-mode-edit-prompt)))
           (let ((popup (get-buffer "*pi prompt *pi[pe2]**")))
             (should popup)
             ;; Only one prompt-edit buffer exists (no duplicate on reopen).
             (should (= 1 (length (cl-remove-if-not
                                   (lambda (x)
                                     (string-match-p "pi prompt" (buffer-name x)))
                                   (buffer-list)))))
             (should (get-buffer-window popup)))
           (kill-buffer "*pi prompt *pi[pe2]**"))
       (pi-mode--unregister-session "*pi[pe2]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-prompt-edit-submit-syncs ()
  "C-c C-c clears pi's input box, pastes the edited text, and closes."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[pe3]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[pe3]*" :buffer b :process p
                                   :project-root "/tmp/"))
          (popup (get-buffer-create "*pi prompt *pi[pe3]**")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-current-buffer popup
             (setq-local pi-mode--prompt-edit-session s)
             (insert "hello")
             (pi-mode-prompt-edit-mode +1)
             (goto-char (point-max))
             (insert " EDITED")
             (pi-mode-prompt-edit-submit))
           (should-not (buffer-live-p popup))
           (should (equal (cdr (assq 'ghostel-send-key pi-mode-test--calls))
                          '("c" "ctrl")))
           (should (equal (cdr (assq 'ghostel-paste-string pi-mode-test--calls))
                          '("hello EDITED"))))
       (pi-mode--unregister-session "*pi[pe3]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-prompt-edit-submit-dead-session ()
  "Submit against a dead session keeps the edit and signals user-error."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[pe4]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[pe4]*" :buffer b :process p
                                   :project-root "/tmp/"))
          (popup (get-buffer-create "*pi prompt *pi[pe4]**")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (delete-process p)
           (with-current-buffer popup
             (setq-local pi-mode--prompt-edit-session s)
             (insert "draft")
             (should-error (pi-mode-prompt-edit-submit) :type 'user-error))
           ;; The edit survives in the popup.
           (should (buffer-live-p popup))
           (should (equal (with-current-buffer popup (buffer-string)) "draft"))
           (should-not pi-mode-test--calls))
       (pi-mode--unregister-session "*pi[pe4]*")
       (kill-buffer b)
       (kill-buffer popup)))))

(ert-deftest pi-mode-test-prompt-edit-cancel ()
  "C-c C-k closes the popup without touching pi."
  (pi-mode-test-with-mock-ghostel
   (let* ((b (get-buffer-create "*pi[pe5]*"))
          (p (pi-mode-test--fake-process))
          (s (make-pi-mode-session :id "*pi[pe5]*" :buffer b :process p
                                   :project-root "/tmp/"))
          (popup (get-buffer-create "*pi prompt *pi[pe5]**")))
     (unwind-protect
         (progn
           (pi-mode--register-session s)
           (with-current-buffer popup
             (setq-local pi-mode--prompt-edit-session s)
             (insert "hello")
             (pi-mode-prompt-edit-mode +1)
             (pi-mode-prompt-edit-cancel))
           (should-not (buffer-live-p popup))
           (should-not pi-mode-test--calls))
       (pi-mode--unregister-session "*pi[pe5]*")
       (kill-buffer b) (delete-process p)))))

(ert-deftest pi-mode-test-prompt-edit-keybinding ()
  "C-c C-i in pi-mode-map runs pi-mode-edit-prompt; C-c C-c / C-c C-k drive the popup."
  (should (eq (lookup-key pi-mode-map (kbd "C-c C-i")) 'pi-mode-edit-prompt))
  (should (eq (lookup-key pi-mode-prompt-edit-mode-map (kbd "C-c C-c"))
              'pi-mode-prompt-edit-submit))
  (should (eq (lookup-key pi-mode-prompt-edit-mode-map (kbd "C-c C-k"))
              'pi-mode-prompt-edit-cancel)))

(ert-deftest pi-mode-test-prompt-edit-header ()
  "The popup shows a separedit-style header with Finish/Abort instructions."
  (with-temp-buffer
    (pi-mode-prompt-edit-mode +1)
    (should (string-match-p "C-c C-c: Finish" header-line-format))
    (should (string-match-p "C-c C-k: Abort" header-line-format))
    (should (string-match-p "^\*pi prompt\*" header-line-format))
    ;; The header is display-only: it must not be part of the buffer
    ;; text that gets synced back to pi on submit.
    (should (= (buffer-size) 0))
    (pi-mode-prompt-edit-mode -1)
    (should-not header-line-format)))

;;; Notifications tests

(defun pi-mode-test--msg-entry (role &optional stop ts)
  "A session-JSONL message entry plist (ROLE, STOP, TS)."
  (let ((msg `((role . ,role)
               ,@(and stop `((stopReason . ,stop)))
               ,@(and ts `((timestamp . ,ts))))))
    `((type . "message") (message . ,msg))))

(defun pi-mode-test--write-jsonl (file entries &optional append)
  "Write ENTRIES (plists) as JSON lines to FILE; append when APPEND."
  (with-temp-buffer
    (dolist (entry entries)
      (insert (json-encode entry) "\n"))
    (write-region (point-min) (point-max) file append)))

(defun pi-mode-test--notif-session (name dir)
  "Register a fake session whose session dir is DIR; return the session."
  (let* ((id (format "*pi[%s]*" name))
         (b (get-buffer-create id))
         (p (pi-mode-test--fake-process))
         (s (make-pi-mode-session :id id :buffer b :process p
                                  :project-root "/tmp/notif-proj/")))
    (pi-mode--register-session s)
    s))

(defun pi-mode-test--notif-teardown (session dir)
  "Unregister SESSION, kill its fixtures and DIR, reset poll state."
  (pi-mode--unregister-session (pi-mode-session-id session))
  (when (buffer-live-p (pi-mode-session-buffer session))
    (kill-buffer (pi-mode-session-buffer session)))
  (ignore-errors (delete-process (pi-mode-session-process session)))
  (delete-directory dir t)
  (clrhash pi-mode-notifications--state))

(ert-deftest pi-mode-test-notifications-completion ()
  "A turn that completes while watched triggers one notification."
  (pi-mode-test-with-mock-ghostel
   (let* ((dir (make-temp-file "pi-notif-" t))
          (session (pi-mode-test--notif-session "nt1" dir))
          (file (expand-file-name "s.jsonl" dir))
          (calls nil))
     (unwind-protect
         (let ((pi-mode-session-dir-function (lambda (_root) dir))
               (pi-mode-notifications t))
           ;; First observation: only the header and the user message (the
           ;; turn is still running) → pending, no notification.
           (pi-mode-test--write-jsonl file
             (list '((type . "session"))
                   (pi-mode-test--msg-entry "user")))
           (cl-letf (((symbol-function 'pi-mode-notifications--deliver)
                      (lambda (session) (push session calls))))
             (pi-mode-notifications--poll)
             (should-not calls)
             ;; The turn completes: toolUse/toolResult interleave, then stop.
             (pi-mode-test--write-jsonl file
               (list (pi-mode-test--msg-entry "assistant" "toolUse")
                     (pi-mode-test--msg-entry "toolResult")
                     (pi-mode-test--msg-entry "assistant" "stop")) t)
             (pi-mode-notifications--poll)
             (should (= 1 (length calls)))
             (should (eq (car calls) session))
             (should (equal (pi-mode-notifications--message session)
                            "pi finished: notif-proj"))
             ;; Pending cleared: nothing more to notify.
             (pi-mode-notifications--poll)
             (should (= 1 (length calls)))))
       (pi-mode-test--notif-teardown session dir)))))

(ert-deftest pi-mode-test-notifications-no-duplicate ()
  "One notification per completed turn; a second turn notifies again."
  (pi-mode-test-with-mock-ghostel
   (let* ((dir (make-temp-file "pi-notif-" t))
          (session (pi-mode-test--notif-session "nt2" dir))
          (file (expand-file-name "s.jsonl" dir))
          (calls nil))
     (unwind-protect
         (let ((pi-mode-session-dir-function (lambda (_root) dir))
               (pi-mode-notifications t))
           (pi-mode-test--write-jsonl file (list (pi-mode-test--msg-entry "user")))
           (cl-letf (((symbol-function 'pi-mode-notifications--deliver)
                      (lambda (session) (push session calls))))
             (pi-mode-notifications--poll)
             (pi-mode-test--write-jsonl file
               (list (pi-mode-test--msg-entry "assistant" "stop")) t)
             (pi-mode-notifications--poll)
             (should (= 1 (length calls)))
             (pi-mode-notifications--poll)
             (should (= 1 (length calls)))
             ;; A second turn completes → a second notification.
             (pi-mode-test--write-jsonl file (list (pi-mode-test--msg-entry "user")) t)
             (pi-mode-notifications--poll)
             (pi-mode-test--write-jsonl file
               (list (pi-mode-test--msg-entry "assistant" "stop")) t)
             (pi-mode-notifications--poll)
             (should (= 2 (length calls)))))
       (pi-mode-test--notif-teardown session dir)))))

(ert-deftest pi-mode-test-notifications-first-scan-inference ()
  "First observation infers pending: stale completions never notify."
  (pi-mode-test-with-mock-ghostel
   (let* ((dir (make-temp-file "pi-notif-" t))
          (session (pi-mode-test--notif-session "nt3" dir))
          (calls nil))
     (unwind-protect
         (let ((pi-mode-session-dir-function (lambda (_root) dir))
               (pi-mode-notifications t))
           (cl-letf (((symbol-function 'pi-mode-notifications--deliver)
                      (lambda (_s) (push t calls))))
             ;; A completed turn (user older than its terminal stop): stale.
             (let ((file (expand-file-name "old.jsonl" dir)))
               (pi-mode-test--write-jsonl file
                 (list (pi-mode-test--msg-entry "user" nil "2026-08-15T06:50:00.000Z")
                       (pi-mode-test--msg-entry "assistant" "stop" "2026-08-15T06:51:00.000Z")))
               (pi-mode-notifications--poll)
               (should-not calls))
             ;; A file holding only a completed assistant message: no user.
             (let ((file (expand-file-name "only.jsonl" dir)))
               (pi-mode-test--write-jsonl file
                 (list (pi-mode-test--msg-entry "assistant" "stop" "2026-08-15T07:00:00.000Z")))
               (pi-mode-notifications--poll)
               (should-not calls))
             ;; A mid-turn file (user without terminal stop) stays pending,
             ;; so the completion that follows does notify.
             (let ((file (expand-file-name "mid.jsonl" dir)))
               (pi-mode-test--write-jsonl file
                 (list (pi-mode-test--msg-entry "user" nil "2026-08-15T08:00:00.000Z")))
               (pi-mode-notifications--poll)
               (should-not calls)
               (pi-mode-test--write-jsonl file
                 (list (pi-mode-test--msg-entry "assistant" "stop" "2026-08-15T08:01:00.000Z")) t)
               (pi-mode-notifications--poll)
               (should (= 1 (length calls))))))
       (pi-mode-test--notif-teardown session dir)))))

(ert-deftest pi-mode-test-notifications-aborted ()
  "aborted assistant messages neither notify nor clear pending."
  (pi-mode-test-with-mock-ghostel
   (let* ((dir (make-temp-file "pi-notif-" t))
          (session (pi-mode-test--notif-session "nt4" dir))
          (file (expand-file-name "s.jsonl" dir))
          (calls nil))
     (unwind-protect
         (let ((pi-mode-session-dir-function (lambda (_root) dir))
               (pi-mode-notifications t))
           (pi-mode-test--write-jsonl file
             (list (pi-mode-test--msg-entry "user")
                   (pi-mode-test--msg-entry "assistant" "aborted")))
           (cl-letf (((symbol-function 'pi-mode-notifications--deliver)
                      (lambda (_s) (push t calls))))
             (pi-mode-notifications--poll)
             (should-not calls)
             ;; Pending survives the abort: the next turn's terminal stop
             ;; notifies exactly once.
             (pi-mode-test--write-jsonl file (list (pi-mode-test--msg-entry "user")) t)
             (pi-mode-notifications--poll)
             (should-not calls)
             (pi-mode-test--write-jsonl file
               (list (pi-mode-test--msg-entry "assistant" "stop")) t)
             (pi-mode-notifications--poll)
             (should (= 1 (length calls)))))
       (pi-mode-test--notif-teardown session dir)))))

(ert-deftest pi-mode-test-notifications-visibility ()
  "Visible session buffers suppress delivery unless when-visible is set."
  (pi-mode-test-with-mock-ghostel
   (let* ((dir (make-temp-file "pi-notif-" t))
          (session (pi-mode-test--notif-session "nt5" dir))
          (file (expand-file-name "s.jsonl" dir))
          (calls nil)
          (win (selected-window)))
     (unwind-protect
         (let ((pi-mode-session-dir-function (lambda (_root) dir))
               (pi-mode-notifications t))
           (cl-letf (((symbol-function 'get-buffer-window)
                      (lambda (&rest _) win))
                     ((symbol-function 'pi-mode-notifications--deliver)
                      (lambda (_s) (push t calls))))
             ;; Visible + when-visible nil: suppressed (and marked handled).
             (let ((pi-mode-notifications-when-visible nil))
               (pi-mode-test--write-jsonl file (list (pi-mode-test--msg-entry "user")))
               (pi-mode-notifications--poll)
               (pi-mode-test--write-jsonl file
                 (list (pi-mode-test--msg-entry "assistant" "stop")) t)
               (pi-mode-notifications--poll)
               (should-not calls)
               (pi-mode-notifications--poll)
               (should-not calls))
             ;; Visible + when-visible t: delivered.
             (let ((pi-mode-notifications-when-visible t))
               (pi-mode-test--write-jsonl file (list (pi-mode-test--msg-entry "user")) t)
               (pi-mode-notifications--poll)
               (pi-mode-test--write-jsonl file
                 (list (pi-mode-test--msg-entry "assistant" "stop")) t)
               (pi-mode-notifications--poll)
               (should (= 1 (length calls))))
             ;; Not visible + when-visible nil: delivered (default case).
             (cl-letf (((symbol-function 'get-buffer-window)
                        (lambda (&rest _) nil)))
               (let ((pi-mode-notifications-when-visible nil))
                 (pi-mode-test--write-jsonl file (list (pi-mode-test--msg-entry "user")) t)
                 (pi-mode-notifications--poll)
                 (pi-mode-test--write-jsonl file
                   (list (pi-mode-test--msg-entry "assistant" "stop")) t)
                 (pi-mode-notifications--poll)
                 (should (= 2 (length calls)))))))
       (pi-mode-test--notif-teardown session dir)))))

(ert-deftest pi-mode-test-notifications-rotation ()
  "A shrunken file resets the scan state; growth is picked up again."
  (pi-mode-test-with-mock-ghostel
   (let* ((dir (make-temp-file "pi-notif-" t))
          (session (pi-mode-test--notif-session "nt6" dir))
          (file (expand-file-name "s.jsonl" dir))
          (calls nil))
     (unwind-protect
         (let ((pi-mode-session-dir-function (lambda (_root) dir))
               (pi-mode-notifications t))
           ;; A completed turn observed first: stale, not notified.
           (pi-mode-test--write-jsonl file
             (list (pi-mode-test--msg-entry "user" nil "2026-08-15T06:00:00.000Z")
                   (pi-mode-test--msg-entry "assistant" "stop" "2026-08-15T06:01:00.000Z")))
           (cl-letf (((symbol-function 'pi-mode-notifications--deliver)
                      (lambda (_s) (push t calls))))
             (pi-mode-notifications--poll)
             (should-not calls)
             ;; Offset advancement: only the new user message is scanned.
             (pi-mode-test--write-jsonl file
               (list (pi-mode-test--msg-entry "user" nil "2026-08-15T06:02:00.000Z")) t)
             (pi-mode-notifications--poll)
             (should-not calls)
             ;; Rotation: the file shrank — the state resets to a fresh
             ;; observation and the smaller complete turn stays stale.
             (let ((old-size (file-attribute-size (file-attributes file))))
               (pi-mode-test--write-jsonl file
                 (list (pi-mode-test--msg-entry "user" nil "2026-08-15T07:00:00.000Z")
                       (pi-mode-test--msg-entry "assistant" "stop" "2026-08-15T07:01:00.000Z")))
               (should (< (file-attribute-size (file-attributes file)) old-size)))
             (pi-mode-notifications--poll)
             (should-not calls)
             ;; The rotated session's next turn completes → notified.
             (pi-mode-test--write-jsonl file
               (list (pi-mode-test--msg-entry "user" nil "2026-08-15T08:00:00.000Z")) t)
             (pi-mode-notifications--poll)
             (pi-mode-test--write-jsonl file
               (list (pi-mode-test--msg-entry "assistant" "stop" "2026-08-15T08:01:00.000Z")) t)
             (pi-mode-notifications--poll)
             (should (= 1 (length calls)))))
       (pi-mode-test--notif-teardown session dir)))))

(ert-deftest pi-mode-test-notifications-fallback-delivery ()
  "Without alert, delivery is a message plus a ding; alert is not called."
  (let ((session (make-pi-mode-session :id "*pi[nf]*" :project-root "/tmp/fall-proj/"))
        (msgs nil) (dings 0) (alerts nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) msgs)))
              ((symbol-function 'ding) (lambda () (setq dings (1+ dings))))
              ((symbol-function 'alert) (lambda (&rest _) (push t alerts))))
      (pi-mode-notifications--deliver session)
      (should (equal msgs '("pi finished: fall-proj")))
      (should (= dings 1))
      (should-not alerts))))

(ert-deftest pi-mode-test-notifications-message ()
  "The notification text carries the project and optional session name."
  (let ((session (make-pi-mode-session :id "x" :project-root "/tmp/some-proj/")))
    (should (equal (pi-mode-notifications--message session)
                   "pi finished: some-proj")))
  (let ((session (make-pi-mode-session :id "x" :project-root "/tmp/some-proj/"
                                       :name "refactor")))
    (should (equal (pi-mode-notifications--message session)
                   "pi finished: some-proj (refactor)"))))

(ert-deftest pi-mode-test-notifications-disabled-and-toggle ()
  "The poll is a no-op while disabled; the toggle flips the option."
  (pi-mode-test-with-mock-ghostel
   (let* ((dir (make-temp-file "pi-notif-" t))
          (session (pi-mode-test--notif-session "nt8" dir))
          (file (expand-file-name "s.jsonl" dir))
          (calls nil))
     (unwind-protect
         (let ((pi-mode-session-dir-function (lambda (_root) dir))
               (pi-mode-notifications nil))
           (pi-mode-test--write-jsonl file (list (pi-mode-test--msg-entry "user")))
           (cl-letf (((symbol-function 'pi-mode-notifications--deliver)
                      (lambda (_s) (push t calls))))
             (pi-mode-notifications--poll)
             (should-not calls)
             ;; No detection state was recorded while disabled.
             (should (= 0 (hash-table-count pi-mode-notifications--state)))
             ;; The toggle flips the option.
             (pi-mode-toggle-notifications)
             (should pi-mode-notifications)
             (pi-mode-toggle-notifications)
             (should-not pi-mode-notifications)))
       (pi-mode-test--notif-teardown session dir)))))

(provide 'pi-mode-tests)
;;; pi-mode-tests.el ends here
