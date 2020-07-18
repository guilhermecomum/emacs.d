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

(defun gg/modes/web ()
  (use-package emmet-mode)
  (use-package svelte-mode)
  (use-package web-mode
    :mode
    ("\\.html\\'" "\\.svelte\\'")
    :hook
    (web-mode . emmet-mode))
  (add-hook
   'web-mode-hook
   '(lambda ()
      (setq web-mode-attr-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-enable-current-column-highlight t)
      (setq web-mode-enable-current-element-highlight t)
      (setq web-mode-markup-indent-offset 2)
      ;; On for angular, svelte projects emmet use class
      (setq-mode-local web-mode emmet-expand-jsx-className? nil)
      ;; Disable auto-indent after yank
      (setq web-mode-enable-auto-indentation nil)
      (setq css-indent-offset 2)))
  (add-hook 'web-mode-hook  'auto-rename-tag-mode)
  (add-hook 'web-mode-hook 'flyspell-mode))

(defun gg/modes/prettier ()
  (use-package prettier-js
    :init
    (add-hook 'js-mode-hook  'prettier-js-mode)
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'web-mode-hook 'prettier-js-mode)
    (add-hook 'typescript-mode-hook 'prettier-js-mode)
    (add-hook 'web-mode-hook 'prettier-js-mode)))

(defun gg/modes/javascript ()
  "Setup javascript mode."
  (use-package add-node-modules-path)
  (use-package ng2-mode)
  (use-package tide)
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (setq typescript-indent-level 2)
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (add-hook 'typescript-mode-hook 'setup-tide-mode)
  (add-hook 'typescript-mode-hook 'company-mode)
  (add-hook 'typescript-mode-hook 'flyspell-mode)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                ;; On react projects with typescript emmet use className
                (setq-mode-local web-mode emmet-expand-jsx-className? t)
                (setup-tide-mode)
                )))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (setq js-indent-level 2)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint json-jsonlist)))

  (flycheck-add-mode 'javascript-eslint 'javascript-mode)
  (add-hook 'flycheck-mode-hook 'add-node-modules-path)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'js-mode-hook  'emmet-mode)
  (add-hook 'js-mode-hook  'auto-rename-tag-mode)
  (add-hook 'js-mode-hook 'flyspell-mode)
  ;; On react projects emmet use className
  (add-hook
   'js-mode-hook
   '(lambda ()
      (setq emmet-expand-jsx-className? t))))

(defun gg/modes/org ()
  (add-hook 'org-mode-hook 'turn-on-flyspell))

(defun gg/modes/misc ()
  (use-package yaml-mode)
  (use-package docker)
  (use-package docker-compose-mode))

(defun gg/modes ()
  (gg/modes/javascript)
  (gg/modes/web)
  (gg/modes/prettier)
  (gg/modes/org)
  (gg/modes/misc))

(provide 'gg-modes)
