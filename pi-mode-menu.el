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

;;;###autoload
(transient-define-prefix pi-mode-menu ()
  "Command menu for pi-mode sessions.
Key layout follows claude-code-ide.el: session s/c/r/q/Q/R/l,
navigation b/w/W/a, interaction i/f/e, submenus C/d/S."
  [:description pi-mode--session-status
   :class transient-columns
   ["Session"
    ("s" "Start" pi-mode-start)
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
    ("i" "Insert region" pi-mode-send-region)
    ("f" "Insert file ref" pi-mode-send-file)
    ("E" "Edit prompt" pi-mode-edit-prompt)
    ("e" "Interrupt" pi-mode-interrupt)]
   ["Submenus"
    ("C" "Configuration" pi-mode-config-menu)
    ("d" "Debugging" pi-mode-debug-menu)
    ("S" "Status" pi-mode-status-menu)]])

(transient-define-prefix pi-mode-config-menu ()
  "pi-mode configuration menu."
  [:description "pi-mode Configuration"
   ["Configure"
    ("m" "Model" pi-mode-configure-model)
    ("T" "Thinking" pi-mode-configure-thinking)
    ("u" "TUI mode" pi-mode-configure-tui-mode)
    ("x" "CLI args" pi-mode-configure-cli-args)
    ("n" "Notifications" pi-mode-toggle-notifications
     :description (lambda () (format "Notifications (%s)"
                                     (if pi-mode-notifications "ON" "OFF"))))]])

(transient-define-prefix pi-mode-debug-menu ()
  "pi-mode debugging menu."
  [:description "pi-mode Debug"
   ["Debug"
    ("l" "Log buffer" pi-mode-show-debug)
    ("d" "Debug on/off" pi-mode-toggle-debug)]])

(transient-define-prefix pi-mode-status-menu ()
  "pi-mode status menu."
  [:description "pi-mode Status"
   ["Status"
    ("s" "Check CLI status" pi-mode-check-status)
    ("v" "Version info" pi-mode-show-version-info)]])

(provide 'pi-mode-menu)
;;; pi-mode-menu.el ends here
