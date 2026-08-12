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
            (should (equal hook-args (list b "finished")))))
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

(ert-deftest pi-mode-test-resolve-updates-mru ()
  "Resolving a session updates last-used so it becomes the MRU session."
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
      (delete-process p1) (delete-process p2))))


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
           (with-current-buffer b (pi-mode-interrupt))
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
             (cl-letf (((symbol-function 'read-from-minibuffer)
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
                 pi-mode-list-sessions
                 pi-mode-send-region pi-mode-send-file
                 pi-mode-switch-buffer pi-mode-toggle-panel
                 pi-mode-show-all pi-mode-toggle-recent
                 pi-mode-interrupt pi-mode-configure-model
                 pi-mode-configure-thinking pi-mode-configure-tui-mode
                 pi-mode-configure-cli-args pi-mode-install-keybindings))
    (should (commandp cmd))))

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

(ert-deftest pi-mode-test-display-args ()
  "display-args resolves side, slot, and size from customization."
  (let ((b (get-buffer-create "*pi[da]*"))
        (p (pi-mode-test--fake-process)))
    (unwind-protect
        (let ((pi-mode-window-side 'bottom)
              (pi-mode-window-height 0.3)
              (pi-mode-window-width 0.4))
          (should (equal (pi-mode--display-args b)
                         '(bottom 0 window-height 0.3)))
          (let ((pi-mode-window-side 'right))
            (should (equal (pi-mode--display-args b)
                           '(right 0 window-width 0.4))))
          (let ((s (make-pi-mode-session :id "*pi[da]*" :buffer b :process p
                                         :project-root "/tmp/" :window-slot 3)))
            (pi-mode--register-session s)
            (should (equal (pi-mode--display-args b)
                           '(bottom 3 window-height 0.3)))
            (pi-mode--unregister-session "*pi[da]*")))
      (kill-buffer b) (delete-process p))))

(ert-deftest pi-mode-test-window-commands-no-error ()
  "Window commands handle the no-session case gracefully."
  (should-error (pi-mode-toggle-recent) :type 'user-error)
  (should-error (pi-mode-show-all) :type 'user-error)
  (should (equal (pi-mode-toggle-panel) :hidden)))

;;; Task 8: keybindings preset + Configure commands

(ert-deftest pi-mode-test-keys-json-valid ()
  "The preset parses as JSON and contains the required remaps."
  ;; json-parse-string defaults :array-type to vector; ask for lists to
  ;; match the expectations below.
  (let ((data (json-parse-string pi-mode--keybindings-json :array-type 'list)))
    (should (equal (gethash "app.clear" data) '("ctrl+shift+c")))
    (should (equal (gethash "app.message.copy" data) '("ctrl+shift+x")))
    (should (equal (gethash "tui.editor.deleteToLineStart" data) '("ctrl+shift+u")))
    (should (equal (gethash "tui.editor.historyPrevious" data) "ctrl+p"))
    (should (equal (gethash "app.model.cycleForward" data) "ctrl+alt+p"))))

(ert-deftest pi-mode-test-keys-install ()
  "Install writes the preset; existing file gets a .bak backup."
  (let ((pi-mode-agent-dir (make-temp-file "pi-agent-" t)))
    (unwind-protect
        (progn
          (write-region "old" nil (pi-mode--keybindings-path))
          (pi-mode-install-keybindings t) ; force: no prompt
          (should (file-exists-p (concat (pi-mode--keybindings-path) ".bak")))
          (with-temp-buffer
            (insert-file-contents (pi-mode--keybindings-path))
            (should (string-match-p "ctrl\\+shift\\+c" (buffer-string)))))
      (delete-directory pi-mode-agent-dir t))))

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
           (with-current-buffer b (pi-mode-configure-model "gpt-5.1"))
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
           (with-current-buffer b (pi-mode-configure-thinking))
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
             (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
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

(provide 'pi-mode-tests)
;;; pi-mode-tests.el ends here
