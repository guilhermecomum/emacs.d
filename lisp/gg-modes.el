;;; init.el --- Configuration Modes for Emacs
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
;; Configuration for different modes, like web, javascript, etc.
;;
;;; Code:

(defun setup-tide ()
  (tide-setup)
  (flycheck-mode 1)
  (eldoc-mode +1)
  (company-mode 1))

(defun gg/modes/web ()
  (use-package emmet-mode)
  (use-package svelte-mode)
  (use-package mode-local)
  (use-package web-mode
    :mode (("\\.html?\\'" . web-mode)
           ("\\.tsx\\'" . web-mode)
           ("\\.svelte\\'" . web-mode))
    :hook
    (web-mode . emmet-mode)
    :config
    (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-enable-auto-indentation nil))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setq-mode-local web-mode emmet-expand-jsx-className? t)
                (setup-tide))))

  (setq-mode-local web-mode emmet-expand-jsx-className? nil)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-hook 'web-mode-hook 'flyspell-prog-mode))

(defun gg/modes/prettier ()
  (use-package prettier-js
    :init
    (add-hook 'js-mode-hook  'prettier-js-mode)
    (add-hook 'typescript-mode-hook 'prettier-js-mode)))

(defun gg/modes/react ()
  "Setup to work in react projects"
  (use-package rjsx-mode)
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
  (add-hook 'rjsx-mode-hook  'emmet-mode)
  (add-hook 'rjsx-mode-hook  'auto-rename-tag-mode)
  (add-hook 'rjsx-mode-hook 'flyspell-prog-mode)

  (add-hook
   'rjsx-mode-hook
   '(lambda ()
      (setq emmet-expand-jsx-className? t)))
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(defun gg/modes/angular ()
  "Setup to work in Angular projects"
  (use-package ng2-mode
    :ensure t
    :mode ("\\component.html$" . ng2-mode))
    (add-hook 'typescript-mode-hook #'setup-tide))


(defun gg/modes/javascript ()
  "Setup javascript and typescript."
  (use-package js-comint)
  (use-package add-node-modules-path)
  (use-package xref-js2)
  (use-package tide
    :init
    :after (typescript-mode company flycheck)
    :hook ((typescript-mode . setup-tide)
           (web-mode . setup-tide)))
  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'js-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  (setq js-indent-level 2)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'javascript-mode)
  (add-hook 'flycheck-mode-hook 'add-node-modules-path)
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t))

(defun gg/modes/misc ()
  (use-package yaml-mode)
  (use-package docker-compose-mode))

(defun gg/modes ()
  (gg/modes/javascript)
  (gg/modes/web)
  (gg/modes/prettier)
  (gg/modes/misc)
  (gg/modes/react)
  (gg/modes/angular))

(provide 'gg-modes)
