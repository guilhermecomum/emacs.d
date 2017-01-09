;;; Commentary:
;; Extensions mapping to mode

;;; Code:

(add-to-list 'auto-mode-alist '("\\.es\\'"         . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'"        . web-mode))
(add-to-list 'auto-mode-alist '("\\.art\\'"        . artist-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'"        . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme\\'"  . sh-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))


(provide 'init-extensions)
;;; init-extensions.el ends here
