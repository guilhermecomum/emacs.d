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
  (use-package org-contrib)
  (use-package git-auto-commit-mode)
  (setq org-directory "~/Projects/org-files")
  (setq org-tag-alist '(("work" . ?w) ("personal" . ?p) ("cto" . ?c) ("emacsLove" . ?l) ("read" . ?r) ("quotes" . ?q) ("finances" . ?f)))
  (setq org-startup-indented t)
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil)
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (setq gac-automatically-push-p t)
  (add-hook 'org-journal-after-save-hook 'git-auto-commit-mode)
  (global-set-key (kbd "C-c j") 'org-journal-open-current-journal-file)
  (global-set-key (kbd "C-c a")
                  (lambda ()
                    (interactive)
                    (org-agenda nil "z")))
  (global-set-key (kbd "C-c /") 'org-capture))

(defun gg/org/misc ()
  "General org stuffs."
  (require 'org-inlinetask)
  (require 'org-tempo)
  (require 'org-collector)

  (add-to-list 'org-modules 'org-habit t)
  (setq org-treat-insert-todo-heading-as-state-change t)
  (setq org-log-into-drawer t)

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

  ;;Reveal
  (use-package ox-reveal)
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (setq org-reveal-title-slide nil)
  (setq org-reveal-mathjax t)

  (use-package htmlize
    :ensure t)

  ;;Clean bullets
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
    (setq org-journal-enable-agenda-integration t
     org-journal-dir "~/Projects/org-files/journal/"
     org-journal-file-type 'yearly
     org-journal-time-format ""
     org-journal-file-format "%Y.org"
     org-journal-date-format "%A, %d %B %Y")
    :bind
      :bind* ("C-c C-j" . org-journal-new-entry))

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

(defun gg/org/super-agenda ()
  "Setup super-agenda"
  (use-package org-super-agenda)
  (org-super-agenda-mode t)

  (setq org-agenda-custom-commands
        '(("z" "Super zaen view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :todo "TODAY"
                                  :scheduled today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Next to do"
                                   :todo "NEXT"
                                   :order 1)
                            (:name "Due Soon"
                                   :discard (:tag ("finances"))
                                   :deadline future
                                   :order 8)
                            (:name "Personal"
                                   :tag "personal"
                                   :order 30)
                            (:name "Work"
                                   :tag "work"
                                   :order 31)
                            (:discard (:tag ("Routine" "Daily")))))))))))
  (org-agenda nil "z")
  (delete-other-windows)

  (defun is-paid? (time)
    "Check if a billing is paid based on the date"
    (if (eq (string-to-number (format-time-string "%m")) (nth 4 (org-parse-time-string time)))
        "-" "pago")))

(defun gg/org ()
  "Call out other org customization functions."
  (gg/org/init)
  (gg/org/misc)
  (gg/org/super-agenda)
  (gg/org/journal))

(provide 'gg-org)

;;; gg-org.el ends here
