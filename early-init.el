;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Adjust garbage collection for faster startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Disable package.el in favor of Elpaca
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent unwanted runtime builds in gccemacs; packages are compiled ahead of time
(setq comp-deferred-compilation nil)

;; Load a built-in theme early to avoid flash of unstyled Emacs
;; This will be replaced by doom-themes later in readme.org
(load-theme 'misterioso t)

;;; early-init.el ends here
