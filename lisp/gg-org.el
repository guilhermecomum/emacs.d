;;;  gg-org.el --- Org-mode configurations
;;
;; Author: Guilherme Guerra <guilherme.ga@gmail.com>
;;
;; Copyright (C) 2020  Guilherme Guerra
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
;;  General configurations for org-modes
;;
;;; Code:

(defun gg/org/init ()
  "Initial 'org-mode' configuration."
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (global-set-key (kbd "C-c i")
                  (lambda () (interactive) (find-file "~/org/index.org")))

  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(defun gg/org/misc ()
  "General org stuffs."
  (require 'org-inlinetask)
  (defun do-org-show-images ()
    (interactive)
    (org-display-inline-images t t))
  (global-set-key (kbd "C-c C-x C v")
                  'do-org-show-all-inline-images)

  (use-package ox-clip
    :config
    (global-set-key (kbd "s-c")
                  'ox-clip-formatted-copy))

  (custom-set-variables '(org-bullets-bullet-list (quote ("*" "*" ))))
  (setq left-margin-width 2)
  (setq right-margin-width 2)
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 0)

  ;;Setup olivetti mode
  (use-package olivetti
    :config
    (add-hook 'markdown-mode-hook (lambda () (olivetti-mode)))
    (add-hook 'org-mode-hook (lambda () (olivetti-mode)))))

(defun gg/org/utf-8-bullet ()
  "Replace asterisk chars with sexy UTF-8 Bullets."
  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 ()
           (compose-region (match-beginning 1) (match-end 1) "•")))))))

;; (defun gg/org/drill ()
;;   "Install and setup org-drill."
;;   (use-package org-drill))

(defun gg/org ()
  "Call out other org customization functions."
  (gg/org/init)
  (gg/org/misc)
  (gg/org/utf-8-bullet))

(provide 'gg-org)

;;; gg-org.el ends here
