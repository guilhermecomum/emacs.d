;; Default path to load lisp files
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; cask
(require 'cask "~/.emacs.d/.cask/cask/cask.el")
(cask-initialize)

(setq package-enable-at-startup nil)
(package-initialize)

;; load path so that configs from lisp folder can be required
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-lisp)
(require 'init-general)
(require 'init-keys)
(require 'init-helm)
(require 'init-company)
(require 'init-neotree)
(require 'init-projectile)
(require 'init-magit)
(require 'init-yasnippet)
(require 'init-flyspell)
(require 'init-theme)
(require 'init-org)
(require 'init-scss)
(require 'init-css)
(require 'init-javascript)
(require 'init-mc)


;; server
(server-start)

;; file with custom-set-variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; init.el ends here
