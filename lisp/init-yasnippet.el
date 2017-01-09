;;; Commentary:
;; yasnippet configuration

;;; Code:

(require 'yasnippet)
(add-to-list 'load-path
             (expand-file-name "plugins/yasnippet" user-emacs-directory))
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-completing-prompt))


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
