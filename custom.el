;;; custom.el --- User-specific customizations -*- lexical-binding: t -*-

;; This file is for your personal customizations.
;; It will not be overwritten when updating the main configuration.

;; Example: Set your full name and email for Git commits, org-mode, etc.
(setq user-full-name "Your Name"
      user-mail-address "your.email@example.com")

;; Example: Customize theme
;; (load-theme 'doom-nord t)  ;; Uncomment and change to your preferred theme

;; Example: Custom key bindings
;; (global-set-key (kbd "C-c f") 'find-file-in-project)

;; Example: Add additional packages
;; (use-package markdown-mode
;;   :mode ("\\.md\\'" . markdown-mode))

;;; custom.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("32f22d075269daabc5e661299ca9a08716aa8cda7e85310b9625c434041916af"
     default))
 '(safe-local-variable-values '((projectile-project-compilation-cmd . "tsc"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
