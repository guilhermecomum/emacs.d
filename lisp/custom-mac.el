;;; custom-mac.el --- Configuration for OSX
;;
;;; Commentary:
;;
;; Configuration for mac environment.
;;
;;; Code:


(defun custom-mac ()
  "Mac specific stuff."
  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'nil)
    (setq mac-command-modifier 'meta)

    ;; Set font
    (set-frame-font "Monaco 12")

    (menu-bar-mode 1)))

(provide 'custom-mac)
;;; custom-mac.el ends here
