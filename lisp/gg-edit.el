;;; init.el --- Editing options
;;
;; Author: Guilherme Guerra <guilherme.ga@gmail.com>
;;
;; Copyright (C) 2012-2020  Guilherme Guerra
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Editing setup includes setting up line Numbers, Tabs vs Spaces,
;; Code Snippets, etc.
;;
;;; Code:

(defun gg/edit/general ()
  "Misc edit configs."

  (use-package linum
    :config
    (global-linum-mode 1)
    (setq linum-format "%d "))

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
  (setq show-paren-style 'parenthesis)

  ;; keep newline end of file
  (setq require-final-newline t)

  (use-package multiple-cursors
    :bind (("C-S-c C-S-c" . mc/edit-lines)
           ("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c C-<" . mc/mark-all-like-this)))

  (use-package smartparens)
  (smartparens-global-mode t)

  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  (use-package flycheck
    :init
    (global-flycheck-mode)
    :config
    (setq flycheck-emacs-lisp-load-path 'inherit))

  (use-package editorconfig
    :config
    (editorconfig-mode 1)))

(defun gg/edit/functions ()
  "Functions make edit easier."

  (defun unfill-paragraph (&optional region)
    "Takes a multi-line paragraph or (REGION) and make it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region))))

(defun gg/edit()
  "Call out other editing customization function."
  (gg/edit/general)
  (gg/edit/functions))

(provide 'gg-edit)

;;; gg-edit.el ends here
