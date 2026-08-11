;;; pi-mode-menu.el --- Transient command menu for pi-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; The pi-mode command surface: one transient prefix, `C-c C-''.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'pi-mode)
(require 'pi-mode-session)

;;;###autoload
(transient-define-prefix pi-mode-menu ()
  "Command menu for pi-mode sessions."
  [:description "pi-mode"
   :class transient-columns
   ["Session"
    ("s" "Start" pi-mode-start)
    ("c" "Continue" pi-mode-session-continue)
    ("r" "Resume" pi-mode-session-resume)
    ("f" "Fork" pi-mode-session-fork)
    ("n" "Rename" pi-mode-session-rename)
    ("q" "Stop" pi-mode-session-stop)
    ("Q" "Stop all" pi-mode-session-stop-all)
    ("l" "List sessions" pi-mode-list-sessions)]
   ["Send"
    ("p" "Prompt" pi-mode-send-prompt)
    ("r" "Region (C-u: reference)" pi-mode-send-region)
    ("f" "File" pi-mode-send-file)
    ("d" "Defun" pi-mode-send-defun)
    ("e" "Error at point" pi-mode-send-error)]
   ["Navigate"
    ("b" "Switch buffer" pi-mode-switch-buffer)
    ("w" "Toggle panel" pi-mode-toggle-panel)
    ("W" "Show all" pi-mode-show-all)
    ("t" "Toggle recent" pi-mode-toggle-recent)]
   ["Debug"
    ("l" "Log buffer" pi-mode-show-debug)
    ("d" "Debug on/off" pi-mode-toggle-debug)]
   ["Configure"
    ("m" "Model" pi-mode-configure-model)
    ("T" "Thinking" pi-mode-configure-thinking)
    ("u" "TUI mode" pi-mode-configure-tui-mode)
    ("k" "Install keybindings" pi-mode-install-keybindings)
    ("c" "CLI args" pi-mode-configure-cli-args)]])

(provide 'pi-mode-menu)
;;; pi-mode-menu.el ends here
