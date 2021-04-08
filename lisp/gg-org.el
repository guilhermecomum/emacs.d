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
  (global-set-key (kbd "C-c j") 'org-journal-open-current-journal-file)
  (global-set-key (kbd "C-c a")
                  (lambda ()
                    (interactive)
                    (org-agenda nil "c")))
  (global-set-key (kbd "C-c /") 'org-capture))

(defun gg/org/misc ()
  "General org stuffs."
  (require 'org-inlinetask)
  (defun do-org-show-images ()
    (interactive)
    (org-display-inline-images t t))
  (global-set-key (kbd "C-c C-x C v")
                  'do-org-show-all-inline-images)

  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((agenda "")
            (alltodo "")))))
  (use-package ox-clip
    :config
    (global-set-key (kbd "s-c")
                    'ox-clip-formatted-copy))

  ;;Clean bullets
  (setq org-indent-indentation-per-level 1)
  (setq org-hide-leading-stars 't)

  ;;Setup olivetti mode
  (use-package olivetti
    :config
    (add-hook 'markdown-mode-hook (lambda () (olivetti-mode)))
    (add-hook 'org-mode-hook (lambda () (olivetti-mode))))

  (defun my-org-insert-link ()
    "Insert org link where default description is set to html title."
    (interactive)
    (let* ((url (read-string "URL: "))
           (title (get-html-title-from-url url)))
      (org-insert-link nil url title)))

  (defun get-html-title-from-url (url)
    "Return content in <title> tag."
    (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
      (save-excursion
        (set-buffer download-buffer)
        (beginning-of-buffer)
        (setq x1 (search-forward "<title>"))
        (search-forward "</title>")
        (setq x2 (search-backward "<"))
        (buffer-substring-no-properties x1 x2)))))

(defun gg/org/journal ()
  "Setup org-journal."
  (use-package org-journal
    :config
    (setq org-journal-enable-agenda-integration t)
    (setq org-journal-dir "~/Dropbox/org/journal/")
    (setq org-journal-file-type 'yearly)
    (setq org-journal-time-format "")
    (setq org-journal-date-format "%A, %d %B %Y"))

  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max)))

  (setq org-capture-templates '(("j" "Journal entry" plain (function org-journal-find-location)
                                 "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                                 :jump-to-captured t :immediate-finish t))))

(defun gg/org ()
  "Call out other org customization functions."
  (gg/org/init)
  (gg/org/misc)
  (gg/org/journal))

(provide 'gg-org)

;;; gg-org.el ends here
