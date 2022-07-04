(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("/home/guerra/Projects/org-files/journal/2022.org"))
 '(safe-local-variable-values
   '((eval progn
           (require 'projectile)
           (puthash
            (projectile-project-root)
            "cask exec buttercup -L ." projectile-test-cmd-map)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#9099AB" :background nil :height 0.9 :italic t))))
