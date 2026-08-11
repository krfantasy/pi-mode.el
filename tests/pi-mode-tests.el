;;; pi-mode-tests.el --- Tests for pi-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests.  The ghostel API is mocked with cl-letf; fake processes are
;; real pipe processes (they work in batch mode).

;;; Code:

(require 'ert)
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
                 (pi-mode-test--record-call 'ghostel-exec args)
                 (pi-mode-test--fake-process)))
              ((symbol-function 'ghostel-send-string)
               (lambda (&rest args) (pi-mode-test--record-call 'ghostel-send-string args)))
              ((symbol-function 'ghostel-send-key)
               (lambda (&rest args) (pi-mode-test--record-call 'ghostel-send-key args)))
              ((symbol-function 'ghostel-paste-string)
               (lambda (&rest args) (pi-mode-test--record-call 'ghostel-paste-string args)))
              ((symbol-function 'ghostel-send-C-c)
               (lambda (&rest args) (pi-mode-test--record-call 'ghostel-send-C-c args))))
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

(provide 'pi-mode-tests)
;;; pi-mode-tests.el ends here
