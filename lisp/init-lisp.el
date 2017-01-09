;;; Commentary:
;;; lisp configuration

;;; Code:

;; so that flycheck does not give error on require
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;; disable checkdoc
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(provide 'init-lisp)
;;; init-lisp.el ends here
