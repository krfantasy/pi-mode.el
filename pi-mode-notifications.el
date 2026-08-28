;;; pi-mode-notifications.el --- Desktop notifications for pi-mode -*- lexical-binding: t; -*-

;; Author: Jay Xu
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.49") (transient "0.7"))
;; Keywords: tools, processes
;; URL: https://github.com/krfantasy/pi-mode.el
;; License: The License

;;; Commentary:
;; Desktop notifications when a pi session finishes answering a turn.
;; Off by default — enable with `pi-mode-toggle-notifications'
;; (`C-c C-'' → Configuration → Notifications) or by customizing
;; `pi-mode-notifications'.
;;
;; Turn completion is inferred from pi's session JSONL: pi records no
;; agent_end event outside RPC mode, but every assistant message entry
;; carries a terminal `stopReason' ("stop", "length", "error") when the
;; turn ends.  A repeating timer (`pi-mode-notifications--poll') scans
;; the .jsonl files under each live session's directory (recursively,
;; covering pi's nested run-0/session.jsonl layout) and notifies once
;; per completed turn.
;;
;; Detection state is per file, (OFFSET . PENDING), in
;; `pi-mode-notifications--state'.  New entries are scanned in file
;; order: a user message sets PENDING, and a terminal assistant message
;; while PENDING triggers the notification.  The first observation of a
;; file instead infers PENDING from its history (a user message newer
;; than the last terminal assistant message), so resuming or continuing
;; sessions behave correctly and stale completions — turns that
;; finished before pi-mode started watching — never notify.
;;
;; Known approximations, same class as the prompt-extraction wrap
;; approximation: (1) a mid-turn compaction runs a summarization LLM
;; call whose terminal message can notify early; (2) nested subagent
;; transcripts under the session dir are scanned too, so a subagent
;; finishing mid-turn can notify early.  Both are rare and benign.
;;
;; `alert' is an optional dependency: when installed the notification
;; is a desktop alert; otherwise a message plus a ding.  Package-Requires
;; is unchanged.

;;; Code:

(require 'cl-lib)
(require 'pi-mode)
(require 'pi-mode-session)

(defvar pi-mode-notifications--state (make-hash-table :test #'equal)
  "Per-file detection state: FILE -> (OFFSET . PENDING).")

(defcustom pi-mode-notifications nil
  "When non-nil, notify when a pi session finishes answering a turn.
The notification fires at most once per turn; sessions whose buffer is
displayed in a window are skipped while
`pi-mode-notifications-when-visible' is nil."
  :type 'boolean
  :group 'pi)

(defcustom pi-mode-notifications-when-visible nil
  "When non-nil, notify even when the session buffer is displayed.
When nil, a session whose buffer is visible in a window is skipped
(gptel-style: you are already looking at it)."
  :type 'boolean
  :group 'pi)

(defcustom pi-mode-notifications-interval 2.0
  "Seconds between polls of the live sessions' JSONL files.
Read afresh at every poll, so Customize changes take effect on the
next tick without re-arming."
  :type 'number
  :group 'pi)

(defun pi-mode-notifications--message (session)
  "Notification text for SESSION's completed turn."
  (let ((project (file-name-nondirectory
                  (directory-file-name (pi-mode-session-project-root session)))))
    (if (pi-mode-session-name session)
        (format "pi finished: %s (%s)" project (pi-mode-session-name session))
      (format "pi finished: %s" project))))

(declare-function alert "alert")

(defun pi-mode-notifications--deliver (session)
  "Notify that SESSION finished answering a turn.
Uses the `alert' package when available (optional dependency);
otherwise a message plus a ding.  Every notification is logged."
  (let ((text (pi-mode-notifications--message session)))
    (if (and (require 'alert nil t) (fboundp 'alert))
        (alert text :title "pi-mode")
      (progn
        (message "%s" text)
        (ding)))
    (pi-mode-log "notification: %s" text)))

(defun pi-mode-notifications--maybe-deliver (session)
  "Deliver SESSION's completion notification unless it is visible.
When the session buffer is displayed in a window and
`pi-mode-notifications-when-visible' is nil, the completed turn is
marked handled without an alert — the user is already looking at it."
  (unless (and (not pi-mode-notifications-when-visible)
               (get-buffer-window (pi-mode-session-buffer session)))
    (pi-mode-notifications--deliver session)))

;;;###autoload
(defun pi-mode-toggle-notifications ()
  "Toggle `pi-mode-notifications'."
  (interactive)
  (setq pi-mode-notifications (not pi-mode-notifications))
  (message "pi-mode notifications %s" (if pi-mode-notifications "on" "off")))

(defun pi-mode-notifications--jsonl-files (dir)
  "Return .jsonl files under DIR, recursively.
Recursion covers pi's nested run-0/session.jsonl layout."
  (when (file-directory-p dir)
    (directory-files-recursively dir "\\.jsonl\\'" nil t)))

(defun pi-mode-notifications--scan-tail (file start prev-pending session)
  "Scan FILE's new entries from byte START; return (NEXT-OFFSET . PENDING).
PREV-PENDING is the state's pending flag from earlier chunks: entries are
processed in file order on top of it — a user message sets PENDING; a
terminal assistant message (stopReason \"stop\", \"length\" or
\"error\") while PENDING notifies once and clears it.  \"toolUse\" and
\"aborted\" assistant messages are no-ops (mid-run; the user already
knows they interrupted).  A line that fails to parse (an entry caught
mid-write) rewinds NEXT-OFFSET to its own start so the next poll
retries it."
  (let ((pending prev-pending)
        (next start))
    (with-temp-buffer
      (insert-file-contents file nil start nil)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-start (point))
              (ok t))
          (unless (looking-at "\n")
            (let* ((line (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))
                   (entry (ignore-errors (json-parse-string line))))
              (if (null entry)
                  (setq ok nil)
                (let ((type (gethash "type" entry)))
                  (when (equal type "message")
                    (let* ((msg (gethash "message" entry))
                           (role (and msg (gethash "role" msg)))
                           (stop (and msg (gethash "stopReason" msg))))
                      (cond
                       ((equal role "user")
                        (setq pending t))
                       ((and (equal role "assistant")
                             (member stop '("stop" "length" "error"))
                             pending)
                        (pi-mode-notifications--maybe-deliver session)
                        (setq pending nil)))))))))
          (if ok
              (progn
                (goto-char (line-end-position))
                (forward-line 1)
                (setq next (+ start (- (point) 1))))
            ;; Unparseable line: stop here, retry from its start next poll.
            (setq next (+ start (- line-start 1)))
            (goto-char (point-max))))))
    (cons next pending)))

(defun pi-mode-notifications--infer-pending (file)
  "Infer FILE's pending state from its history.
Non-nil when the last user message is newer than the last terminal
assistant message: a turn was submitted and no completion has been
recorded since.  Timestamps are ISO-8601 strings, which sort
lexicographically in time order."
  (let ((last-user nil)
        (last-terminal nil)
        (saw-user nil))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (line (split-string (buffer-string) "\n" t))
        (let ((entry (ignore-errors (json-parse-string line))))
          (when (and entry (equal (gethash "type" entry) "message"))
            (let* ((msg (gethash "message" entry))
                   (role (and msg (gethash "role" msg)))
                   (stop (and msg (gethash "stopReason" msg)))
                   ;; ISO-8601 timestamps sort lexicographically in time
                   ;; order; the entry-level one is the ISO string (the
                   ;; message-level one may be epoch milliseconds).
                   (ts (or (and (stringp (gethash "timestamp" entry))
                                (gethash "timestamp" entry))
                           (let ((mts (and msg (gethash "timestamp" msg))))
                             (and (stringp mts) mts)))))
              (cond
               ((equal role "user")
                (setq saw-user t)
                (setq last-user ts))
               ((and (equal role "assistant")
                     (member stop '("stop" "length" "error")))
                (setq last-terminal ts))))))))
    (and saw-user
         (or (null last-terminal)
             (and last-user (string> last-user last-terminal))))))

(defun pi-mode-notifications--scan-file (file session)
  "Scan FILE for a completed turn and notify SESSION accordingly.
The first observation of a file (or of a rotated one that shrank)
infers PENDING from history instead of processing entries, so stale
completions and resumed sessions behave correctly."
  (ignore-errors
    (when (file-readable-p file)
      (let* ((size (file-attribute-size (file-attributes file)))
             (state (gethash file pi-mode-notifications--state))
             (offset (car state)))
        (cond
         ((null state)
          (puthash file (cons size (pi-mode-notifications--infer-pending file))
                   pi-mode-notifications--state))
         ((< size offset)
          (puthash file (cons size (pi-mode-notifications--infer-pending file))
                   pi-mode-notifications--state))
         ((> size offset)
          (let ((result (pi-mode-notifications--scan-tail
                         file offset (cdr state) session)))
            (puthash file (cons (car result) (cdr result))
                     pi-mode-notifications--state))))))))

(defun pi-mode-notifications--scan-dir (dir session)
  "Scan DIR's .jsonl files for SESSION."
  (dolist (file (pi-mode-notifications--jsonl-files dir))
    (pi-mode-notifications--scan-file file session)))

(defun pi-mode-notifications--prune ()
  "Drop detection state for files outside live sessions' dirs."
  (let ((dirs (mapcar (lambda (s)
                        (pi-mode--session-dir
                         (pi-mode-session-project-root s)))
                      (pi-mode--active-sessions))))
    (maphash (lambda (file value)
               (ignore value)
               (unless (cl-loop for dir in dirs
                                thereis (file-in-directory-p file dir))
                 (remhash file pi-mode-notifications--state)))
             pi-mode-notifications--state)))

(defun pi-mode-notifications--poll ()
  "Check live sessions' JSONL for completed turns; notify when found.
A no-op while `pi-mode-notifications' is nil; sessions launched
outside pi-mode are not watched (no session struct to associate).
Reschedules itself one-shot at `pi-mode-notifications-interval', so
the chain survives while notifications are disabled and Customize
changes to the interval take effect on the next tick."
  (when pi-mode-notifications
    (let ((sessions (pi-mode--active-sessions)))
      (when sessions
        (dolist (session sessions)
          (pi-mode-notifications--scan-dir
           (pi-mode--session-dir (pi-mode-session-project-root session))
           session)))
      (pi-mode-notifications--prune)))
  (run-at-time pi-mode-notifications-interval nil
               #'pi-mode-notifications--poll))

;; One-shot start of the poll chain; `pi-mode-notifications--poll'
;; reschedules itself each tick.
(run-at-time pi-mode-notifications-interval nil
             #'pi-mode-notifications--poll)

(provide 'pi-mode-notifications)

;;; pi-mode-notifications.el ends here
