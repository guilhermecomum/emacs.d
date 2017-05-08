;;; custom-editing.el --- Editing Options
;;
;;; Commentary:
;;
;; Editing setup includes Fonts, Line Numbers, Tabs vs Spaces, Auto
;; Complete, Code Snippets, etc.
;;
;;; Code:

(require 'flycheck)
(require 'linum)
(require 'multiple-cursors)

(defun custom-editing-line-numbers ()
  "Configure line numbers in the Emacs UI."
  (add-hook 'conf-mode-hook 'linum-on)
  (add-hook 'prog-mode-hook 'linum-on)
  (add-hook 'text-mode-hook 'linum-on)
  (setq linum-format "%d "))

(defun custom-editing-misc ()
  "Misc editing settings."

  ;; Do not wrap lines
  (setq-default truncate-lines t)

  ;; spaces instead of tabs
  (setq-default indent-tabs-mode nil)

  ;; Complain about trailing white spaces
  (setq show-trailing-whitespace t)

  ;; Also highlight parenthesis
  (show-paren-mode 1)

  ;; scroll smoothly
  (setq scroll-conservatively 10000)

  ;; keep newline end of file
  (setq require-final-newline t)

  ;; Enable syntax checks
  (global-flycheck-mode)
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))

  ;; Find peace with syntax checking when requiring files located in
  ;; custom paths
  (setq flycheck-emacs-lisp-load-path 'inherit))

(defun custom-multiple-cursors()
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(defun custom-editing ()
  "Call out other editing customization functions."
  (custom-editing-line-numbers)
  (custom-editing-misc)
  (custom-multiple-cursors))

(provide 'custom-editing)
;;; custom-editing.el ends here
