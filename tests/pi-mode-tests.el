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

(provide 'pi-mode-tests)
;;; pi-mode-tests.el ends here
