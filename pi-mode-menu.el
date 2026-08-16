;;; pi-mode-menu.el --- Transient command menu for pi-mode -*- lexical-binding: t; -*-

;; Author: Jay Xu
;; Version: 0.1.0
;; Keywords: tools, processes
;; URL: https://github.com/jayxu/pi-mode.el
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:
;; The pi-mode command surface: one transient prefix, `C-c C-''.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'pi-mode)
(require 'pi-mode-session)
(require 'pi-mode-status)
(require 'pi-mode-notifications)

(defun pi-mode--start-description ()
  "Dynamic description for the start command.
Mirrors cc-ide's \"Start new Claude Code instance (N running)\"."
  (let ((count (length (pi-mode--project-sessions))))
    (if (> count 0)
        (format "Start new pi session (%d running)" count)
      "Start new pi session")))

;;;###autoload
(transient-define-prefix pi-mode-menu ()
  "Command menu for pi-mode sessions.
Key layout follows claude-code-ide.el: session s/c/r/F/R/q/Q/l,
navigation b/w/W/a, interaction i/p/n/f/E/e, submenus C/d."
  [:description pi-mode--session-status
   :class transient-columns
   ["Session"
    ("s" pi-mode-start :description pi-mode--start-description)
    ("c" "Continue" pi-mode-session-continue)
    ("r" "Resume" pi-mode-session-resume)
    ("F" "Fork" pi-mode-session-fork)
    ("R" "Rename" pi-mode-session-rename)
    ("q" "Stop" pi-mode-session-stop)
    ("Q" "Stop all" pi-mode-session-stop-all)
    ("l" "List sessions" pi-mode-list-sessions)]
   ["Navigation"
    ("b" "Switch buffer" pi-mode-switch-buffer)
    ("w" "Toggle panel" pi-mode-toggle-panel)
    ("W" "Toggle recent" pi-mode-toggle-recent)
    ("a" "Show all" pi-mode-show-all)]
   ["Interaction"
    ("i" "Insert selection" pi-mode-insert-selection)
    ("p" "Send prompt" pi-mode-send-prompt)
    ("n" "Insert newline" pi-mode-insert-newline)
    ("f" "Insert file ref" pi-mode-send-file)
    ("E" "Edit prompt" pi-mode-edit-prompt)
    ("e" "Interrupt" pi-mode-interrupt)]
   ["Submenus"
    ("C" "Configuration" pi-mode-config-menu)
    ("d" "Debugging" pi-mode-debug-menu)]])

(defun pi-mode--save-config ()
  "Save the pi-mode configuration to the custom file.
Persists every setting the configuration menu can change:
window layout (`pi-mode-window-side', `-width', `-height'),
`pi-mode-focus-on-open', `pi-mode-notifications' and
`pi-mode-cli-args'."
  (interactive)
  (customize-save-variable 'pi-mode-window-side pi-mode-window-side)
  (customize-save-variable 'pi-mode-window-width pi-mode-window-width)
  (customize-save-variable 'pi-mode-window-height pi-mode-window-height)
  (customize-save-variable 'pi-mode-focus-on-open pi-mode-focus-on-open)
  (customize-save-variable 'pi-mode-notifications pi-mode-notifications)
  (customize-save-variable 'pi-mode-cli-args pi-mode-cli-args)
  (pi-mode-log "Configuration saved to custom file"))

(defun pi-mode--set-window-side (side)
  "Set `pi-mode-window-side' to SIDE (left/right/top/bottom).
Takes effect on the next display of any pi window; existing windows
move when displayed again."
  (interactive (list (intern (completing-read
                              "Window side: "
                              '("left" "right" "top" "bottom")
                              nil t nil nil
                              (symbol-name pi-mode-window-side)))))
  (setq pi-mode-window-side side)
  (pi-mode-log "Window side set to %s" side))

(defun pi-mode--set-window-width (width)
  "Set `pi-mode-window-width' to WIDTH (body columns)."
  (interactive (list (read-number "Window width: " pi-mode-window-width)))
  (setq pi-mode-window-width width)
  (pi-mode-log "Window width set to %d" width))

(defun pi-mode--set-window-height (height)
  "Set `pi-mode-window-height' to HEIGHT (text lines)."
  (interactive (list (read-number "Window height: " pi-mode-window-height)))
  (setq pi-mode-window-height height)
  (pi-mode-log "Window height set to %d" height))

(defun pi-mode--toggle-focus-on-open ()
  "Toggle `pi-mode-focus-on-open'."
  (interactive)
  (setq pi-mode-focus-on-open (not pi-mode-focus-on-open))
  (pi-mode-log "Focus on open %s" (if pi-mode-focus-on-open "enabled" "disabled")))

(transient-define-prefix pi-mode-config-menu ()
  "pi-mode configuration menu."
  [:description "pi-mode Configuration"
   ["Window"
    ("s" "Set window side" pi-mode--set-window-side)
    ("w" "Set window width" pi-mode--set-window-width)
    ("h" "Set window height" pi-mode--set-window-height)
    ("f" "Toggle focus on open" pi-mode--toggle-focus-on-open
     :description (lambda () (format "Focus on open (%s)"
                                     (if pi-mode-focus-on-open "ON" "OFF"))))]
   ["Configure"
    ("m" "Model" pi-mode-configure-model)
    ("T" "Thinking" pi-mode-configure-thinking)
    ("u" "TUI mode" pi-mode-configure-tui-mode)
    ("x" "CLI args" pi-mode-configure-cli-args)
    ("n" "Notifications" pi-mode-toggle-notifications
     :description (lambda () (format "Notifications (%s)"
                                     (if pi-mode-notifications "ON" "OFF"))))]
   ["Save"
    ("S" "Save configuration" pi-mode--save-config)]])

(transient-define-prefix pi-mode-debug-menu ()
  "pi-mode debugging menu."
  [:description "pi-mode Debug"
   ["Status"
    ("S" "Check CLI status" pi-mode-check-status)
    ("v" "Version info" pi-mode-show-version-info)]
   ["Debug"
    ("d" "Debug on/off" pi-mode-toggle-debug
     :description (lambda () (format "Debug on/off (%s)"
                                     (if pi-mode-debug "ON" "OFF"))))]
   ["Debug Logs"
    ("l" "Log buffer" pi-mode-show-debug)
    ("c" "Clear debug log" pi-mode--clear-debug-log)]])

(transient-define-prefix pi-mode-status-menu ()
  "pi-mode status menu."
  [:description "pi-mode Status"
   ["Status"
    ("s" "Check CLI status" pi-mode-check-status)
    ("v" "Version info" pi-mode-show-version-info)]])

(provide 'pi-mode-menu)
;;; pi-mode-menu.el ends here
