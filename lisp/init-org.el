;;; Commentary:
;; org-mode configuration

;;; Code:

(require 'org)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "WAITING_REVIEW" "|" "DONE")))

(setq org-todo-keyword-faces
      '(("WAITING_REVIEW" . "yellow")))

(setq org-log-done 'time)

;; make org mode allow eval of some langs
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (js . t)))

;; stop emacs asking for confirmation
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)


(provide 'init-org)
;;; init-org.el ends here
