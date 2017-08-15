;;; custom-general.el --- General Options
;;
;;; Commentary:
;;
;; General configuration that includes setting up UI, global key
;; shortcuts, UTF-8 support, etc.
;;
;;; Code:

(require 'company)
(require 'flyspell)
(require 'neotree)
(require 'pallet)
(require 'projectile)
(require 'spaceline-all-the-icons)

(defun custom-general-utf-8 ()
  "Configure all known coding variables to use `UTF-8'."
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (setq current-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

(defun custom-general-ui-fringe ()
  "Configure the Fringe area."
  ;; Custom bitmap to be shown in the fringe area for lines with any
  ;; sort of linting issues
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000)))
  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info)

  ;; Get rid of the background color in the Fringe area
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))

  ;; Finally, enable the fringe mode
  (fringe-mode 1))

(defun custom-general-ui ()
  "General UI configuration."
  ;; No bars. Doing this first to avoid showing/hidding delay on start
  ;; up
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)

  ;; Misc
  (column-number-mode)              ;; Basic config fr ocolumns
  (setq ring-bell-function 'ignore) ;; No freaking bell
  (setq inhibit-splash-screen t)    ;; No splash screen
  (setq inhibit-startup-screen t)

  ;; Spaceline
  (spaceline-all-the-icons-theme)
  (spaceline-toggle-all-the-icons-buffer-size-off)
  (spaceline-toggle-all-the-icons-battery-status-off)
  (spaceline-toggle-all-the-icons-hud-off)

  ;; window size
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(defun custom-general-keys ()
  "Configure global key bindings."

  (when (eq system-type 'darwin)
    (global-set-key (kbd "M-]") 'next-buffer)
    (global-set-key (kbd "M-[") 'previous-buffer))

  (global-set-key [s-tab] 'next-buffer)
  (global-set-key [S-s-iso-lefttab] 'previous-buffer)

  ;;; comments
  (global-set-key [(ctrl c) (c)] 'comment-region)
  (global-set-key [(ctrl c) (d)] 'uncomment-region)

  ;;; Navegation
  (global-set-key (kbd "M-g") 'goto-line)

  ;;; Sort
  (global-set-key (kbd "C-c s") 'sort-lines)

  ;;; change window
  (global-set-key [(C-tab)] 'other-window))

(defun dont-kill-scratch ()
  "This function doesn't let you kill scratch by mistake."
  (if (not (equal (buffer-name) "*scratch*"))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

(defun custom-general-misc ()
  "Miscellaneous settings and start up actions."
  (setq default-directory "~/") ;; There's no place like home

  ;; Sync package list with Cask file
  (pallet-mode t)

  ;; Store auto-save and backup files in a temporary directory
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  (add-hook 'kill-buffer-query-functions #'dont-kill-scratch)

  ;; writing yes or no is length, type y / n instead
  (defalias 'yes-or-no-p 'y-or-n-p))

(defun custom-general-projectile()
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")

  (helm-projectile-on))

(defun custom-general-flyspell()
  (setq ispell-program-name "aspell")

  (defun fd-switch-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (change (if (string= dic "pt_BR") "english" "pt_BR")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change)))

  (global-set-key (kbd "<f5>") 'fd-switch-dictionary)
  (global-set-key (kbd "C-;") 'helm-flyspell-correct))

(defun custom-general-company()
  (add-hook 'after-init-hook 'global-company-mode)
  ;; bigger popup window
  (setq company-tooltip-limit 10)

  ;; decrease delay before autocompletion popup shows
  (setq company-idle-delay .3))

(defun custom-general-neotree()
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-smart-open t))

(defun custom-general ()
  "Call out other general customization functions."
  (custom-general-ui)
  (custom-general-ui-fringe)
  (custom-general-utf-8)
  (custom-general-keys)
  (custom-general-misc)
  (custom-general-projectile)
  (custom-general-flyspell)
  (custom-general-company)
  (custom-general-neotree))

(provide 'custom-general)
;;; custom-general.el ends here
