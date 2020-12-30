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

(defun gg/modes/tools ()
  "Useful tools for modes"
  (use-package emmet-mode)
  (use-package mode-local)
  (use-package js-comint)
  (use-package add-node-modules-path)
  (use-package yaml-mode)
  (use-package docker-compose-mode)
  (use-package gradle-mode)
  (use-package sass-mode)
  (use-package prettier-js
    :init
    (add-hook 'js-mode-hook  'prettier-js-mode)
    (add-hook 'typescript-mode-hook 'prettier-js-mode)
    (add-hook 'svelte-mode-hook 'prettier-js-mode))
  (use-package eglot
    :ensure t
    :hook ((js-mode . eglot-ensure)
           (typescript-mode . eglot-ensure)
           (go-mode . eglot-ensure))
    :config
    (define-key eglot-mode-map (kbd "C-c .") 'eglot-code-actions)
    (define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)
    (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))
    (add-to-list 'eglot-server-programs '((js-mode rjsx-mode ng2-ts-mode ng2-mode typescript-mode) . ("typescript-language-server" "--stdio")))))

(defun gg/modes/web ()
  (use-package web-mode
    :mode (("\\.html?\\'" . web-mode)
           ("\\.tsx\\'" . web-mode))
    :hook
    (web-mode . emmet-mode)
    (web-mode . auto-rename-tag-mode)
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
          web-mode-enable-auto-indentation nil)

  ;; Configure emmet to use className when use react with typescript
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setq-mode-local web-mode emmet-expand-jsx-className? t))))
  (setq-mode-local web-mode emmet-expand-jsx-className? nil)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-hook 'web-mode-hook 'flyspell-prog-mode)))

(defun gg/modes/react ()
  "Setup to work in react projects"
  (use-package rjsx-mode)
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
  (add-hook 'rjsx-mode-hook  'emmet-mode)
  (add-hook 'rjsx-mode-hook  'auto-rename-tag-mode)
  (add-hook 'rjsx-mode-hook 'flyspell-prog-mode)

  ;; Configure emmet to use className when use react
  (add-hook
   'rjsx-mode-hook
   '(lambda ()
      (setq emmet-expand-jsx-className? t)))
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(defun gg/modes/javascript ()
  "Setup javascript and typescript."

  (setq js-indent-level 2)

  ;; Disable jshint
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'javascript-mode)
  (add-hook 'flycheck-mode-hook 'add-node-modules-path)
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t))


(defun gg/modes/svelte ()
  "Svelte config"
  (use-package svelte-mode
    :hook (svelte-mode . emmet-mode)))

(defun gg/modes/go ()
  (use-package go-mode
    :config
    (add-hook 'go-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'gofmt-before-save)
                (setq tab-width 2)
                (setq indent-tabs-mode nil)))))


(defun gg/modes ()
  (gg/modes/tools)
  (gg/modes/javascript)
  (gg/modes/web)
  (gg/modes/react)
  (gg/modes/svelte)
  (gg/modes/go))

(provide 'gg-modes)