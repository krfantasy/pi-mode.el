;;; pi-mode-keys.el --- pi keybindings preset for pi-mode -*- lexical-binding: t; -*-

;; Author: Jay Xu
;; Version: 0.1.0
;; Keywords: tools, processes
;; URL: https://github.com/jayxu/pi-mode.el
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Installs an Emacs-friendly keybinding preset for the pi TUI, resolving
;; conflicts with Emacs and ghostel.  See the design spec section 4.1.

;;; Code:

(require 'cl-lib)

(defcustom pi-mode-agent-dir "~/.pi/agent"
  "pi's agent configuration directory."
  :type 'directory
  :group 'pi)

(defconst pi-mode--keybindings-json
  (string-join
   '("{"
     "  \"tui.editor.historyPrevious\": \"ctrl+p\","
     "  \"tui.editor.historyNext\": \"ctrl+n\","
     "  \"app.model.cycleForward\": \"ctrl+alt+p\","
     "  \"app.model.cycleBackward\": \"shift+ctrl+alt+p\","
     "  \"app.clear\": [\"ctrl+shift+c\"],"
     "  \"app.message.copy\": [\"ctrl+shift+x\"],"
     "  \"app.editor.external\": [\"ctrl+shift+g\"],"
     "  \"tui.editor.deleteToLineStart\": [\"ctrl+shift+u\"],"
     "  \"tui.editor.undo\": [\"ctrl+shift+z\"],"
     "  \"tui.editor.deleteCharBackward\": [\"backspace\", \"ctrl+h\"],"
     "  \"tui.editor.yank\": [\"ctrl+shift+y\"],"
     "  \"tui.editor.yankPop\": [\"alt+y\"],"
     "  \"tui.input.newLine\": [\"shift+enter\", \"ctrl+j\"],"
     "  \"tui.editor.cursorLeft\": [\"left\", \"ctrl+b\"],"
     "  \"tui.editor.cursorRight\": [\"right\", \"ctrl+f\"],"
     "  \"tui.editor.cursorWordLeft\": [\"alt+left\", \"alt+b\"],"
     "  \"tui.editor.cursorWordRight\": [\"alt+right\", \"alt+f\"],"
     "  \"tui.editor.deleteCharForward\": [\"delete\", \"ctrl+d\"],"
     "  \"app.session.rename\": \"ctrl+r\","
     "  \"app.thinking.toggle\": \"ctrl+t\","
     "  \"app.tools.expand\": \"ctrl+o\","
     "  \"app.message.followUp\": \"alt+enter\","
     "  \"app.message.dequeue\": \"alt+up\","
     "  \"tui.altScreen.previousPrompt\": \"ctrl+shift+up\","
     "  \"tui.altScreen.nextPrompt\": \"ctrl+shift+down\""
     "}")
   "\n")
  "The pi keybinding preset installed by `pi-mode-install-keybindings'.")

(defun pi-mode--keybindings-path ()
  "Return the pi keybindings.json path."
  (expand-file-name "keybindings.json" pi-mode-agent-dir))

;;;###autoload
(defun pi-mode-install-keybindings (&optional force)
  "Install the pi keybinding preset, backing up an existing file.
With FORCE (prefix), skip the overwrite confirmation."
  (interactive "P")
  (let ((path (pi-mode--keybindings-path)))
    (when (and (file-exists-p path) (not force))
      (unless (y-or-n-p (format "Overwrite %s? A .bak backup will be created. " path))
        (user-error "Aborted")))
    (when (file-exists-p path)
      (copy-file path (concat path ".bak") t))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert pi-mode--keybindings-json))
    (message "Installed pi keybindings to %s; run `/reload` in pi to apply" path)))

(provide 'pi-mode-keys)
;;; pi-mode-keys.el ends here
