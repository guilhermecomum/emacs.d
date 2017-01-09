;;; Commentary:
;; general settings for Emacs

;;; Code:

;; execute path from shell
 (when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize))

;; Disable distractions
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Disable tooltip
(tooltip-mode nil)

;; Disable splash screen
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Disable the annoying alarms
(setq ring-bell-function 'ignore)

;; Don't save backup files
(setq make-backup-files nil)

;; show line numbers globally
(global-linum-mode 1)

;; don't kill scratch
(defun my/dont-kill-scratch ()
  "This function doesn't let you kill scratch by mistake."
  (if (not (equal (buffer-name) "*scratch*"))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))
(add-hook 'kill-buffer-query-functions #'my/dont-kill-scratch)

;; refresh buffers automatically if changed by ext program
(global-auto-revert-mode 1)

;; writing yes or no is length, type y / n instead
(defalias 'yes-or-no-p 'y-or-n-p)

;; unnecessary whitespace must be cleaned up
(global-whitespace-cleanup-mode)

;; buffer path in bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; window size
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; show column number
(setq column-number-mode t)

;; smooth scrolling
;; scroll one line at a time (less "jumpy" than defaults)

;; one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)
;; keyboard scroll one line at a time
(setq scroll-step 1)

;; allow narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; use space instead of tab for indentation
(setq-default indent-tabs-mode nil)

;; tab width global
(setq-default tab-width 2)

;; electric pair mode
(electric-pair-mode 1)

;; Ctrl-K with no kill
(defun delete-line-no-kill ()
  "Do not cut line with Control-k, just remove it."
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
  )
(global-set-key (kbd "C-k") 'delete-line-no-kill)

;; enable rainbow-delimiters-mode in any programming mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; disable wierd auto vertical scroll
(setq auto-window-vscroll nil)

(setq system-uses-terminfo nil)

(global-undo-tree-mode t)

(global-flycheck-mode 1)

(show-paren-mode t)

(provide 'init-general)
;;; init-general.el ends here
