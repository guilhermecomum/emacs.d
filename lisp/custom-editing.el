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
(require 'smartparens)
(require 'rainbow-delimiters)
(require 'editorconfig)

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

  ;; Cleanup white spaces before save
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; Also highlight parenthesis
  (show-paren-mode 1)

  ;; scroll smoothly
  (setq scroll-conservatively 10000)

  ;; keep newline end of file
  (setq require-final-newline t)

  ;; smartparens
  (smartparens-global-mode t)

  ;; rainbow delimiters
  (rainbow-delimiters-mode t)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

  ;; Enable syntax checks
  (global-flycheck-mode)
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))

  ;; Find peace with syntax checking when requiring files located in
  ;; custom paths
  (setq flycheck-emacs-lisp-load-path 'inherit))

(defun custom-magic-edit()
  (setq ruby-insert-encoding-magic-comment nil))

(defun custom-multiple-cursors()
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(defun custom-edit-functions ()
  "Functions make edit easier."

  (defun unfill-paragraph (&optional region)
    "Takes a multi-line paragraph or (REGION) and make it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region))))

(defun custom-editorconfig ()
  "Functions config editorconfig."
  (editorconfig-mode 1)
  )

(defun custom-editing ()
  "Call out other editing customization functions."
  (custom-editing-line-numbers)
  (custom-editing-misc)
  (custom-magic-edit)
  (custom-multiple-cursors)
  (custom-edit-functions)
  (custom-editorconfig))


(provide 'custom-editing)
;;; custom-editing.el ends here
