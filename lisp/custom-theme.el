;;; custom-theme.el --- Configuration for OSX
;;
;;; Commentary:
;;
;; Configuration for Emacs color theme
;;
;;; Code:

(require 'doom-themes)
(require 'solaire-mode)

(defun custom-theme ()
  "Setup doom theme."

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Set font
  (set-frame-font "Monaco 10")

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-one t)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; ;; brighten buffers (that represent real files)
  ;; (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  ;; ;; To enable solaire-mode unconditionally for certain modes:
  ;; (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

  ;; ;; ...if you use auto-revert-mode:
  ;; (add-hook 'after-revert-hook #'turn-on-solaire-mode)

  ;; highlight the minibuffer when it is activated:
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

  ;; ;; if the bright and dark background colors are the wrong way around, use this
  ;; ;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
  ;; ;; This should be used *after* you load the active theme!
  ;; ;;
  ;; ;; NOTE: This is necessary for themes in the doom-themes package!
  ;; (solaire-mode-swap-bg)

  ;; ;; Enable nlinum line highlighting
  ;; (doom-themes-nlinum-config)
)
(provide 'custom-theme)
;;; custom-theme.el ends here
