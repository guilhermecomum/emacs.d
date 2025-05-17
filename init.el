;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
(load-theme 'misterioso)

;; Set up straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure use-package with straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t) ;; Always use straight.el for use-package

;; Load modular configuration files
(defun load-config-file (file)
  "Load the configuration file FILE."
  (load (expand-file-name file user-emacs-directory)))

;; Load configuration modules
(load-config-file "config/base.el")        ;; Base configuration
(load-config-file "config/ui.el")          ;; UI configuration
(load-config-file "config/editor.el")      ;; Editing enhancements
;; (load-config-file "config/dev-core.el")    ;; Core development tools
;; (load-config-file "config/typescript.el")  ;; TypeScript/JavaScript config
;; (load-config-file "config/ruby.el")        ;; Ruby config
;; (load-config-file "config/web.el")         ;; Web development (HTML, CSS, Shopify)
;; (load-config-file "config/org.el")         ;; Org-mode configuration

;; Keep customization settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
