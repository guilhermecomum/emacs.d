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

  ;; Enable custom neotree theme
  (setq doom-neotree-enable-file-icons t)

  (load-theme 'doom-one t)

  (setq doom-enable-bold t    ; if nil, bolding are universally disabled
        doom-enable-italic t  ; if nil, italics are universally disabled

        ;; doom-one specific settings
        doom-one-brighter-modeline nil
        doom-one-brighter-comments nil)

  ;; brighter source buffers
  (add-hook 'find-file-hook 'solaire-mode)

  ;; brighter minibuffer when active
  (add-hook 'minibuffer-setup-hook 'solaire-mode)

  ;; The temporary buffers ediff spins up aren't dimmed. You can fix this with:
  (add-hook 'ediff-prepare-buffer-hook 'solaire-mode)

  ;; Set font
  (set-frame-font "Monaco 12")

  ;; ;;; More reliable inter-window border
  ;; The native border "consumes" a pixel of the fringe on righter-most splits
  (setq window-divider-default-places t
        window-divider-default-bottom-width 0
        window-divider-default-right-width 1)
  (window-divider-mode +1)

  ;; Enable nlinum line highlighting
  (doom-themes-nlinum-config)

  ;; Necessary for org-mode
  (setq org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t))

(provide 'custom-theme)
;;; custom-theme.el ends here
