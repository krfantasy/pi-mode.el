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
;; turn ends.  A repeating timer (`pi-mode-notifications--poll') groups
;; the live sessions by their pi session directory and scans each
;; directory's .jsonl files once (recursively, covering pi's nested
;; run-0/session.jsonl layout), notifying once per completed turn.
;; A directory shared by several sessions cannot attribute a completion
;; to one of them, so the notification then names the project only and
;; is skipped (with `pi-mode-notifications-when-visible' nil) only when
;; every session of the directory is displayed in a window.
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
The notification fires at most once per turn; it is skipped while
`pi-mode-notifications-when-visible' is nil and every watched
session's buffer is displayed in a window (single-session behavior)."
  :type 'boolean
  :group 'pi)

(defcustom pi-mode-notifications-when-visible nil
  "When non-nil, notify even when the session buffer is displayed.
When nil, a completion whose directory's sessions are all displayed
in windows is skipped (gptel-style: you are already looking at it)."
  :type 'boolean
  :group 'pi)

(defcustom pi-mode-notifications-interval 2.0
  "Seconds between polls of the live sessions' JSONL files.
Read afresh at every poll, so Customize changes take effect on the
next tick without re-arming."
  :type 'number
  :group 'pi)

(defun pi-mode-notifications--message (sessions)
  "Notification text for the completed turn watched by SESSIONS.
SESSIONS are the live sessions of the directory holding the completed
file.  A single session keeps the current text, with its name when it
has one; two or more sessions share the directory, so the completion
cannot be attributed to one of them and the text names the project
only."
  (let* ((session (car sessions))
         (project (file-name-nondirectory
                   (directory-file-name (pi-mode-session-project-root session)))))
    (if (cdr sessions)
        (format "pi finished: %s" project)
      (if (pi-mode-session-name session)
          (format "pi finished: %s (%s)" project (pi-mode-session-name session))
        (format "pi finished: %s" project)))))

(declare-function alert "alert")

(defun pi-mode-notifications--deliver (sessions)
  "Notify that one of the directory's SESSIONS finished a turn.
Uses the `alert' package when available (optional dependency);
otherwise a message plus a ding.  Every notification is logged."
  (let ((text (pi-mode-notifications--message sessions)))
    (if (and (require 'alert nil t) (fboundp 'alert))
        (alert text :title "pi-mode")
      (progn
        (message "%s" text)
        (ding)))
    (pi-mode-log "notification: %s" text)))

(defun pi-mode-notifications--maybe-deliver (sessions)
  "Deliver SESSIONS' completion notification unless all are visible.
When every session of the scanned directory is displayed in a window
and `pi-mode-notifications-when-visible' is nil, the completed turn
is marked handled without an alert — the user is already looking at
it.  A completion cannot be pinned to one session of a shared
directory, so it is suppressed only when ALL of them are visible;
single-session directories behave exactly as before."
  (unless (and (not pi-mode-notifications-when-visible)
               (cl-every (lambda (session)
                           (get-buffer-window (pi-mode-session-buffer session)))
                         sessions))
    (pi-mode-notifications--deliver sessions)))

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

(defun pi-mode-notifications--scan-tail (file start prev-pending sessions)
  "Scan FILE's new entries from byte START; return (NEXT-OFFSET . PENDING).
SESSIONS are the live sessions of FILE's directory; a completion is
attributed to all of them (see `pi-mode-notifications--maybe-deliver').
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
                        (pi-mode-notifications--maybe-deliver sessions)
                        (setq pending nil)))))))))
          (if ok
              (progn
                (goto-char (line-end-position))
                (forward-line 1)
                ;; The buffer holds only the tail inserted from byte
                ;; START, so bytes consumed up to buffer position P is
                ;; (- (position-bytes P) 1).  Character positions would
                ;; undercount multibyte UTF-8 and drift the stored
                ;; offset into the middle of a sequence.
                (setq next (+ start (- (position-bytes (point)) 1))))
            ;; Unparseable line: stop here, retry from its start next poll.
            (setq next (+ start (- (position-bytes line-start) 1)))
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

(defun pi-mode-notifications--scan-file (file sessions)
  "Scan FILE for a completed turn and notify SESSIONS accordingly.
SESSIONS are the live sessions of FILE's directory.  The first
observation of a file (or of a rotated one that shrank) infers PENDING
from history instead of processing entries, so stale completions and
resumed sessions behave correctly."
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
                         file offset (cdr state) sessions)))
            (puthash file (cons (car result) (cdr result))
                     pi-mode-notifications--state))))))))

(defun pi-mode-notifications--scan-dir (dir sessions)
  "Scan DIR's .jsonl files for the directory's live SESSIONS."
  (dolist (file (pi-mode-notifications--jsonl-files dir))
    (pi-mode-notifications--scan-file file sessions)))

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

(defvar pi-mode-notifications--timer nil
  "Outstanding notification poll timer, or nil when idle.")

(defun pi-mode-notifications--poll ()
  "Check live sessions' JSONL for completed turns; notify when found.
Sessions are grouped by their pi session directory and each unique
directory is scanned once per poll, with the directory's session list
threaded down: a completion is attributed to all of them, so no
sibling session can be credited with another's turn (and shared
directories are not scanned once per session).  State for dead
sessions is always pruned, even while notifications are disabled, so
the state hash cannot pin deleted files.  The poll chain stops when
there is nothing to watch (no live sessions and empty state) and
restarts via `pi-mode-notifications--ensure-poll'."
  (let ((sessions (pi-mode--active-sessions)))
    (when pi-mode-notifications
      (when sessions
        (let ((by-dir (make-hash-table :test #'equal)))
          (dolist (session sessions)
            (push session (gethash (pi-mode--session-dir
                                    (pi-mode-session-project-root session))
                                   by-dir)))
          (maphash (lambda (dir dir-sessions)
                     (pi-mode-notifications--scan-dir dir (nreverse dir-sessions)))
                   by-dir))))
    (pi-mode-notifications--prune)
    (setq pi-mode-notifications--timer nil)
    ;; Keep watching while there is anything to watch; otherwise let
    ;; the chain die so the timer does not fire forever on an idle
    ;; Emacs.  Sessions launched outside pi-mode are not watched (no
    ;; session struct to associate).
    (when (or sessions (> (hash-table-count pi-mode-notifications--state) 0))
      (setq pi-mode-notifications--timer
            (run-at-time pi-mode-notifications-interval nil
                         #'pi-mode-notifications--poll)))))

(defun pi-mode-notifications--ensure-poll (&rest _args)
  "Start the notification poll chain when it is not running.
Called at load and whenever a session starts (via
`pi-mode-after-start-hook'), so the chain restarts after going idle.
ARGS are ignored (the hook passes the new session)."
  (unless (and pi-mode-notifications--timer
               (memq pi-mode-notifications--timer timer-list))
    (setq pi-mode-notifications--timer
          (run-at-time pi-mode-notifications-interval nil
                       #'pi-mode-notifications--poll))))

;; One-shot start of the poll chain; `pi-mode-notifications--poll'
;; keeps it alive only while there is state to watch.
(pi-mode-notifications--ensure-poll)
(add-hook 'pi-mode-after-start-hook #'pi-mode-notifications--ensure-poll)

(provide 'pi-mode-notifications)

;;; pi-mode-notifications.el ends here
