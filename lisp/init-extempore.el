;;; Commentary:
;; Configuration for extempore (creating music from Emacs)

;;; Code:

(autoload 'extempore-mode "~/.emacs.d/extempore.el" "" t)
(autoload 'extempore-repl "~/.emacs.d/extempore.el" "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))


(provide 'init-extempore)
;;; init-extempore.el ends here
