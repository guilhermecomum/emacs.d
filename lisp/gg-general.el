;;; init.el --- General Options
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
;; Entry point for my Emacs setup.  This module's main goal is to load
;; other files that do more specific setup work.
;;
;;; Code:


(defun gg/general/utf-8 ()
  "Configure all known coding variables to use `UTF-8'."

  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (setq current-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

(defun gg/general/keys ()
  "Configure global key bindings."

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
  (global-set-key [(C-tab)] 'other-window)
  (global-set-key [(C-M-tab)] 'other-window))



(defun gg/general/misc ()
  "Miscellaneous settings and start up actions."
  (setq default-directory "~/") ;; There's no place like home

  (use-package tramp
    :config
    (setq tramp-auto-save-directory "/tmp")
    (defvar disable-tramp-backups '(all)))

  ;; Maximize
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (defun dont-kill-scratch ()
    "This function doesn't let you kill scratch by mistake."
    (if (not (equal (buffer-name) "*scratch*"))
        t
      (bury-buffer)
      nil))

  (use-package emojify
    :config
    (add-hook 'after-init-hook #'global-emojify-mode))

  ;; Store auto-save and backup files in a temporary directory
  (setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backups"))))
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  ;; Disable lockfiles
  (setq create-lockfiles nil)

  (add-hook 'kill-buffer-query-functions #'dont-kill-scratch)

  ;; writing yes or no is length, type y / n instead
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; file with custom-set-variables
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

  (use-package magit)

  (use-package company
    :hook (after-init . global-company-mode)
    :config
    (setq company-idle-delay .3))

  (use-package helm-projectile)
  (use-package projectile
    :bind-keymap ("C-c p" . projectile-command-map)
    :config
    (setq projectile-indexing-method 'alien)
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (projectile-mode))

  ;; use with neotree
  (use-package neotree
    :bind ([f8] . neotree-toggle)
    :config
    (setq neo-autorefresh nil)
    (setq neo-smart-open t)))

(defun gg/general/path ()
  ;;Loads environment variables from the shell.
  (use-package exec-path-from-shell)
  (setq exec-path-from-shell-variables '("GOPATH" "PATH" "MANPATH"))
  (exec-path-from-shell-initialize))

(defun gg/general ()
  "Call out other general customization functions."
  (gg/general/utf-8)
  (gg/general/keys)
  (gg/general/misc)
  (gg/general/path))


(provide 'gg-general)

;;; gg-general.el ends here
