;;; ui.el --- UI enhancements -*- lexical-binding: t -*-

;; UI Improvements
;; ==============

;; Font settings

(set-face-attribute 'default nil :font "Menlo" :height 130)
(set-face-attribute 'variable-pitch nil :font "Verdana" :height 130)

;; Set color theme path and load it
(use-package nerd-icons
  :config
  (when (and (display-graphic-p)
             (not (file-exists-p (expand-file-name "icons" user-emacs-directory))))
    (nerd-icons-install-fonts t)))

;; Dashboard - a nicer startup screen
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Olá, bem vindo ao Emacs"
        dashboard-startup-banner "~/.emacs.d/nyan-cat.png"
        dashboard-center-content t
        dashboard-agenda-release-buffers t
        dashboard-items '((projects . 5) (agenda . 5)))
  (dashboard-setup-startup-hook))


;; Rainbow delimiters for clearer code
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Better delimiter matching
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; File tree navigation
(use-package neotree
  :bind([f9] . neotree-toggle)
  :hook (neo-after-create . (lambda (_)(call-interactively 'text-scale-twice)))
  :config
  (setq neo-autorefresh nil)
  (setq neo-smart-open t)
  (with-eval-after-load 'neotree
    (define-key neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)))

;; Multiple cursors for efficient editing
(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)))

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Emojis
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; Additional UI settings
(setq ring-bell-function 'ignore)        ; Disable ring bell sound
(scroll-bar-mode -1)                      ; Disable visible scrollbar
(tool-bar-mode -1)                        ; Disable the toolbar
(tooltip-mode -1)                         ; Disable tooltips
(menu-bar-mode -1)                        ; Disable the menu bar
(set-fringe-mode 10)                      ; Give some breathing room

;; Improved scrolling behavior
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Nyan cat - because why not?
(use-package nyan-mode
  :config
  (nyan-mode 1))

;;; ui.el ends here
