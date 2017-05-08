;;; init.el --- My Emacs Setup
;;
;;; Commentary:
;;
;; Entry point for my Emacs setup.  This module's main goal is to load
;; other files that do more specific setup work.
;;
;;; Code:

;; load path so that configs from lisp folder can be required
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load packages
(require 'cask "~/.emacs.d/.cask/cask/cask.el")
(cask-initialize)

(setq package-enable-at-startup nil)
(package-initialize)

;; Load all the fun modules
(require 'custom-general)
(require 'custom-mac)
(require 'custom-editing)
(require 'custom-modes)
(require 'custom-theme)

;; Initialize all the modules loaded above
(custom-general)
(custom-mac)
(custom-editing)
(custom-modes)
(custom-theme)

;; file with custom-set-variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; init.el ends here
