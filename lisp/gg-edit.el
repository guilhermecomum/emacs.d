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
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)

  ;; Do not wrap lines
  (setq-default truncate-lines t)

  ;; spaces instead of tabs
  (setq-default indent-tabs-mode nil)

  ;; Complain about trailing white spaces
  (setq show-trailing-whitespace t)

  ;; Cleanup white spaces before save
  (setq whitespace-style '(face trailing lines tabs big-indent))
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; Also highlight parenthesis
  (use-package paren)
  (setq show-paren-style 'parenthesis)
  (show-paren-mode +1)
  (setq show-paren-delay 0)

  ;; keep newline end of file
  (setq require-final-newline t)

  (use-package lorem-ipsum)

  (use-package multiple-cursors
    :bind (("C-S-c C-S-c" . mc/edit-lines)
           ("s-." . mc/mark-next-like-this)
           ("s-," . mc/mark-previous-like-this)
           ("s->" . mc/mark-all-like-this)))

  (use-package smartparens)
  (smartparens-global-mode t)

  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  (use-package editorconfig
    :config
    (editorconfig-mode 1))
  (use-package auto-rename-tag)
  (auto-rename-tag-mode t)


(use-package move-text)

(global-set-key [(control shift up)]  'move-text-up)
(global-set-key [(control shift down)]  'move-text-down))

(defun gg/edit/flyspell ()
  (use-package flyspell-correct-popup)
  (use-package flyspell)
  (setq ispell-program-name "aspell")
  (ispell-change-dictionary "pt_BR")

  (defun fd-switch-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (change (if (string= dic "pt_BR") "english" "pt_BR")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change)))

  (global-set-key (kbd "<f5>") 'fd-switch-dictionary)
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper))

(defun gg/edit/functions ()
  "Functions make edit easier."

  (defun unfill-paragraph (&optional region)
    "Takes a multi-line paragraph or (REGION) and make it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region))))

(defun gg/edit/yasnippet ()
  "Configuration for yasnippets."
  (use-package yasnippet
    :init
    :config
    (yas-load-directory "~/.emacs.d/snippets")
    (yas-global-mode 1)))

(defun gg/edit()
  "Call out other editing customization function."
  (gg/edit/general)
  (gg/edit/functions)
  (gg/edit/flyspell)
  (gg/edit/general)
  (gg/edit/yasnippet))

(provide 'gg-edit)

;;; gg-edit.el ends here
