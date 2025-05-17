;;; base.el --- Base configuration -*- lexical-binding: t -*-

;; Core Emacs settings
;; ===================

;; Better defaults
(setq-default
 inhibit-startup-message t        ; Don't show the startup message
 inhibit-startup-screen t         ; Don't show the startup screen
 cursor-in-non-selected-windows t ; Hide the cursor in inactive windows
 echo-keystrokes 0.1              ; Show keystrokes right away
 create-lockfiles nil             ; Don't create lock files
 make-backup-files nil            ; Don't make backups
 auto-save-default nil            ; Don't auto save
 sentence-end-double-space nil)   ; End sentences with a single space

;; Keep .emacs.d clean
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save" user-emacs-directory) t)))

;; Make directories if they don't exist
(dolist (dir '("backups" "auto-save"))
  (unless (file-directory-p (expand-file-name dir user-emacs-directory))
    (make-directory (expand-file-name dir user-emacs-directory))))

;; Display line and column numbers
(column-number-mode 1)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Highlight current line
(global-hl-line-mode 1)

;; Start with a decent theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t      ; if nil, bold is universally disabled
        doom-themes-enable-italic t)   ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

;; Better mode line
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-project-detection 'projectile
        doom-modeline-buffer-file-name-style 'truncate-with-project))

;; File management
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Remember cursor position
(save-place-mode 1)

;; Use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Buffer Navigation
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)

;; Better help
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; Key discovery
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Environment setup, particularly for macOS
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt
        mac-command-modifier 'meta
        mac-right-option-modifier 'none ; use right option for special chars
        ns-use-native-fullscreen t)

  ;; Set up exec-path-from-shell to get environment variables from the shell
  (use-package exec-path-from-shell
    :config
    (when (daemonp)
      (exec-path-from-shell-initialize))
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

;; Efficient project navigation
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/Projects")
        projectile-sort-order 'recently-active
        projectile-indexing-method 'hybrid)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-files "yarn.lock")
  (add-to-list 'projectile-globally-ignored-files "package-lock.json"))

;; Git integration
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

;; Show git changes in fringe
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; Completion systems
(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("C-c s" . consult-ripgrep))
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; Provide better documentation
(use-package eldoc
  :diminish eldoc-mode
  :init
  (global-eldoc-mode))

;;; base.el ends here
