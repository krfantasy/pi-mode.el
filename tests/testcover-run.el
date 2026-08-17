;;; testcover-run.el --- Batch runner for built-in testcover -*- lexical-binding: t; -*-

;;; Commentary:
;; Instrument the package sources with Emacs's built-in `testcover', run the
;; existing hermetic ERT suite, and print a machine-readable report.  The two
;; message-literal failures below are known Edebug/testcover artifacts on the
;; current Emacs; they are reported but do not mask other test failures.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'testcover)

(defconst pi-mode-testcover--source-files
  '("pi-mode.el"
    "pi-mode-session.el"
    "pi-mode-notifications.el"
    "pi-mode-menu.el"
    "pi-mode-status.el")
  "Package source files instrumented by the coverage runner.")

(defconst pi-mode-testcover--known-artifacts
  '((pi-mode-test-toggle-recent-restores-set . "Closed all pi windows")
    (pi-mode-test-toggle-recent-stamps-mru . "Opened most recent pi session"))
  "Known Edebug constant-check artifacts keyed by test name.")

(defun pi-mode-testcover--project-root ()
  "Return the repository root containing this runner."
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun pi-mode-testcover--instrument-sources ()
  "Instrument sources in the required order and retain their form data."
  (let ((root (pi-mode-testcover--project-root))
        instrumented)
    (dolist (relative pi-mode-testcover--source-files)
      (let ((filename (expand-file-name relative root)))
        (testcover-start filename)
        (push (list relative (find-file-noselect filename) edebug-form-data)
              instrumented)))
    (nreverse instrumented)))

(defun pi-mode-testcover--known-artifact-p (test result)
  "Return non-nil when RESULT is a known testcover artifact for TEST."
  (let* ((name (ert-test-name test))
         (literal (cdr (assq name pi-mode-testcover--known-artifacts)))
         (condition (and (ert-test-result-with-condition-p result)
                         (ert-test-result-with-condition-condition result)))
         (message (and (consp condition) (cadr condition))))
    (and literal
         (stringp message)
         (string-match-p
          (regexp-quote
           (format "Value of form expected to be constant does vary, from (testcover-1value %s) to %s"
                   literal literal))
          message))))

(defun pi-mode-testcover--silent-listener (&rest _events)
  "Discard ERT progress events; the runner prints its own summary."
  nil)

(defun pi-mode-testcover--run-tests ()
  "Run ERT and return (KNOWN UNEXPECTED ABORTED).
UNEXPECTED counts only unexpected test results; ABORTED records an
incomplete ERT run separately."
  (let ((stats (ert-run-tests t #'pi-mode-testcover--silent-listener))
        (known 0)
        (unexpected 0))
    (dotimes (index (ert-stats-total stats))
      (let* ((test (aref (ert--stats-tests stats) index))
             (result (aref (ert--stats-test-results stats) index)))
        (when (and result
                   (not (ert-test-result-expected-p test result)))
          (if (pi-mode-testcover--known-artifact-p test result)
              (cl-incf known)
            (cl-incf unexpected)
            (princ (format "COVERAGE unexpected-test=%s\n"
                           (ert-test-name test)))))))
    (let ((aborted (if (ert--stats-aborted-p stats) 1 0)))
      (princ (format "COVERAGE tests=%d known-artifacts=%d unexpected=%d aborted=%d\n"
                     (ert-stats-total stats) known unexpected aborted))
      (list known unexpected aborted))))

(defun pi-mode-testcover--forms-in (form-data)
  "Return the number of instrumented forms described by FORM-DATA."
  (let ((forms 0))
    (dolist (entry form-data forms)
      (let ((data (get (car entry) 'edebug)))
        (when data
          (cl-incf forms (length (nth 2 data))))))))

(defun pi-mode-testcover--nohits-in (buffer)
  "Return the number of `testcover-nohits' overlays in BUFFER."
  (let ((nohits 0))
    (with-current-buffer buffer
      (dolist (overlay (overlays-in (point-min) (1+ (point-max))) nohits)
        (when (eq (overlay-get overlay 'face) 'testcover-nohits)
          (cl-incf nohits))))))

(defun pi-mode-testcover--percent (forms nohits)
  "Return coverage percentage for FORMS and NOHITS."
  (if (zerop forms)
      100.0
    (* 100.0 (/ (float (- forms nohits)) forms))))

(defun pi-mode-testcover--report (instrumented)
  "Mark INSTRUMENTED source buffers and print coverage lines.
Return the aggregate coverage percentage."
  (let ((total-forms 0)
        (total-nohits 0))
    (dolist (entry instrumented)
      (pcase-let ((`(,relative ,buffer ,form-data) entry))
        (let ((forms (pi-mode-testcover--forms-in form-data)))
          ;; `testcover-mark-all' reads `edebug-form-data' dynamically.  Each
          ;; source has its own snapshot because testcover-start replaces it.
          (with-current-buffer buffer
            (let ((edebug-form-data form-data))
              (testcover-mark-all buffer)))
          (let ((nohits (pi-mode-testcover--nohits-in buffer)))
            (cl-incf total-forms forms)
            (cl-incf total-nohits nohits)
            (princ (format "COVERAGE file=%s forms=%d nohits=%d percent=%.1f\n"
                           relative forms nohits
                           (pi-mode-testcover--percent forms nohits)))))))
    (let ((percent (pi-mode-testcover--percent total-forms total-nohits)))
      (princ (format "COVERAGE aggregate forms=%d nohits=%d percent=%.1f\n"
                     total-forms total-nohits percent))
      percent)))

(let* ((instrumented (pi-mode-testcover--instrument-sources))
       (tests-file (expand-file-name "tests/pi-mode-tests.el"
                                     (pi-mode-testcover--project-root))))
  (load tests-file nil nil t)
  ;; The last testcover-start leaves its source buffer current.  Establish the
  ;; normal batch scratch buffer as the previous buffer for window fallback
  ;; tests that exercise `switch-to-prev-buffer'.
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (pcase-let ((`(,known ,unexpected ,aborted)
               (pi-mode-testcover--run-tests)))
    (let ((percent (pi-mode-testcover--report instrumented)))
      ;; Keep the initial threshold report-only while the two known artifacts
      ;; remain.  Enforce it automatically once those artifacts disappear.
      (let ((status (if (or (> unexpected 0)
                            (> aborted 0)
                            (and (zerop known) (< percent 80.0)))
                        1
                      0)))
        (when noninteractive
          (kill-emacs status))))))

;;; testcover-run.el ends here
