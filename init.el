;; cask
(require 'cask "~/.emacs.d/.cask/cask/cask.el")
(cask-initialize)

(setq package-enable-at-startup nil)
(package-initialize)

;; load path so that configs from lisp folder can be required
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; (require 'init-melpa)
(require 'init-general)
(require 'init-helm)
(require 'init-projectile)
(require 'init-magit)
(require 'init-yasnippet)
(require 'init-company)
(require 'init-helm-swoop)
(require 'init-org)
(require 'init-keys)
(require 'init-extensions)
(require 'init-functions)
(require 'init-scss)
(require 'init-css)
(require 'init-theme)
(require 'init-mc)
(require 'init-javascript)
(require 'init-lisp)
(require 'init-python)
(require 'init-neotree)
(require 'init-flyspell)

;; server
(server-start)

;; file with custom-set-variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;; init.el ends here
