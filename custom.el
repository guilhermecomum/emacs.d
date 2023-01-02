(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-confirm-server-initiated-edits nil)
 '(flycheck-display-errors-delay 0.1)
 '(flymake-error-bitmap '(my-rounded-fringe-indicator compilation-error))
 '(flymake-note-bitmap '(my-rounded-fringe-indicator compilation-info))
 '(flymake-warning-bitmap '(my-rounded-fringe-indicator compilation-warning))
 '(nano-modeline-position 'bottom)
 '(org-agenda-files '("/home/guerra/Projects/org-files/roam"))
 '(safe-local-variable-values
   '((eval progn
           (require 'projectile)
           (puthash
            (projectile-project-root)
            "cask exec buttercup -L ." projectile-test-cmd-map)))))
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(blamer-face ((t :foreground "#9099AB" :background nil :height 0.9 :italic t)) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
