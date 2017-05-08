;;; custom-mac.el --- Configuration for OSX
;;
;;; Commentary:
;;
;; Configuration for mac environment.
;;
;;; Code:

(require 'exec-path-from-shell)

(defun custom-mac ()
  "Mac specific stuff."
  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'nil)
    (setq mac-command-modifier 'meta)

    ;;Loads environment variables from the shell.
    (setq exec-path-from-shell-variables '("GOPATH" "PATH" "MANPATH"))
    (exec-path-from-shell-initialize)

    (menu-bar-mode 1)))

(provide 'custom-mac)
;;; custom-mac.el ends here
