;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Adjust garbage collection for faster startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Disable package.el in favor of Elpaca
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set the font before the first frame is drawn to avoid a resize flicker
(push '(font . "Menlo-13") default-frame-alist)

;;; early-init.el ends here
