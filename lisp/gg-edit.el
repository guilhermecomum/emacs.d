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
;; Auto Complete, Code Snippets, etc.
;;
;;; Code:

(defun gg/edit/general ()
  ;; Misc edit configs

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

  (use-package multiple-cursors
    :bind (("C-S-c C-S-c" . mc/edit-lines)
           ("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c C-<" . mc/mark-all-like-this)))

  (use-package smartparens
    :init
    (bind-key "C-M-f" #'sp-forward-sexp smartparens-mode-map)
    (bind-key "C-M-b" #'sp-backward-sexp smartparens-mode-map)
    (bind-key "C-S-s" #'sp-splice-sexp)
    (bind-key "C-M-<backspace>" #'backward-kill-sexp)
    (bind-key "C-M-S-<SPC>" (lambda () (interactive) (mark-sexp -1)))

    :config
    (smartparens-global-mode t)

    (sp-pair "'" nil :actions :rem)
    (sp-pair "`" nil :actions :rem))

  (use-package rainbow-delimiters
    :config
    (rainbow-delimiters-mode t)
    :hook
    ('web-mode . 'rainbow-delimiters-mode)
    ('css-mode . 'rainbow-delimiters-mode)
    ('lisp-mode . 'rainbow-delimiters-mode)))

(defun gg/edit()
  "Call out other editing customization function."
  (gg/edit/general))

(provide 'gg-edit)
;;;; gg-edit.el ends here
