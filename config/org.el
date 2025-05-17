;;; org.el --- Org mode configuration -*- lexical-binding: t -*-

;; Org Mode Configuration
;; =====================

;; Basic Org Setup
(use-package org
  :ensure nil  ; using built-in org mode
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . (lambda () (display-line-numbers-mode -1))))
  :custom
  (org-directory "~/Projects/org-files")
  (org-default-notes-file (concat org-directory "/inbox.org"))
  (org-agenda-files (list org-directory))
  (org-agenda-span 14)
  (org-agenda-start-day "-0d")
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-todo-keyword-faces '(("NEXT" . (:foreground "orange" :weight bold))
                           ("IN-PROGRESS" . (:foreground "deep sky blue" :weight bold))
                           ("WAITING" . (:foreground "light coral" :weight bold))))
  (org-log-done 'time)
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-catch-invisible-edits 'smart)
  (org-image-actual-width nil)
  (org-enforce-todo-dependencies t)
  (org-special-ctrl-a/e t)
  :config
  ;; Create org-directory if it doesn't exist
  (unless (file-directory-p org-directory)
    (make-directory org-directory))

  ;; Make sure we have our basic files
  (dolist (file '("inbox.org" "gtd.org" "notes.org" "someday.org"))
    (let ((path (concat org-directory "/" file)))
      (unless (file-exists-p path)
        (with-temp-file path
          (insert (format "#+TITLE: %s\n#+AUTHOR: %s\n\n* %s\n"
                          (file-name-sans-extension file)
                          user-full-name
                          (file-name-sans-extension file)))))))

  ;; Set up a basic capture template
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %U")
          ("m" "Meeting" entry (file+headline org-default-notes-file "Meetings")
           "* MEETING %? :meeting:\n  %U")
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %? :note:\n  %U\n  %i")
          ("j" "Journal" entry (file+olp+datetree (concat org-directory "/journal.org"))
           "* %?\nEntered on %U\n  %i"))))

;; Org-contrib packages
(use-package org-contrib
  :after org
  :config
  (require 'org-tempo))

;; Org-superagenda
(use-package org-super-agenda
  :after org
  :config
  (org-super-agenda-mode)

  (setq org-agenda-custom-commands
        '(("z" "Super view"
           ((agenda "" ((org-agenda-span 'week)
                       (org-super-agenda-groups
                        '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)
                          (:name "Important"
                                :priority "A"
                                :order 2)
                          (:name "Next to do"
                                :todo "NEXT"
                                :order 3)
                          (:name "Due Today"
                                :deadline today
                                :order 4)
                          (:name "Overdue"
                                :deadline past
                                :order 5)
                          (:name "Due Soon"
                                :deadline future
                                :order 6)
                          (:name "Waiting"
                                :todo "WAITING"
                                :order 7)
                          (:name "Scheduled Soon"
                                :scheduled future
                                :order 8)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "In Progress"
                                  :todo "IN-PROGRESS"
                                  :order 1)
                            (:name "Next to do"
                                  :todo "NEXT"
                                  :order 2)
                            (:name "Important"
                                  :priority "A"
                                  :order 3)
                            (:name "Today's tasks"
                                  :scheduled today
                                  :order 4)
                            (:name "Waiting/Delegated"
                                  :todo "WAITING"
                                  :order 5)
                            (:name "Overdue"
                                  :deadline past
                                  :order 6)
                            (:name "Projects"
                                  :tag "project"
                                  :order 7)
                            (:name "Research"
                                  :tag "research"
                                  :order 8)
                            (:name "To read"
                                  :tag "read"
                                  :order 9)
                            (:name "Someday"
                                  :tag "someday"
                                  :order 10)
                            (:name "Tasks"
                                  :todo "TODO"
                                  :order 11)
                            (:discard (:anything t))))))))))

;; Org-roam for knowledge management
(use-package org-roam
  :custom
  (org-roam-directory (file-truename (concat org-directory "/roam")))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "\n* Characteristics\n\n- Family: %?\n- Designed by: \n\n* Reference\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain
      "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :project:")
      :unnarrowed t)))
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  ;; Create org-roam directory if it doesn't exist
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))

  ;; Create org-roam-dailies directory if it doesn't exist
  (unless (file-directory-p (concat org-roam-directory "/" org-roam-dailies-directory))
    (make-directory (concat org-roam-directory "/" org-roam-dailies-directory) t))

  (org-roam-db-autosync-mode))

;; Consult-org-roam for improved search
(use-package consult-org-roam
  :after (org-roam consult)
  :init
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :bind
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n s" . consult-org-roam-search))

;; Org-web-tools to capture content from the web
(use-package org-web-tools)

;; Org-modern for prettier org mode
(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-star '("◉" "○" "●" "○" "●" "○" "●"))
  (org-modern-list '((43 . "•") (45 . "–") (42 . "⁃")))
  (org-modern-table nil)
  (org-modern-tag nil))

;; Better list bullets
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-special-todo-items t))

;; Better export options
(use-package ox-gfm)  ;; GitHub-flavored markdown export
(use-package ox-clip)  ;; Copy as formatted text

;; Better citation management
(use-package citar
  :custom
  (citar-bibliography '("~/Documents/bibliography.bib"))
  :hook
  (org-mode . citar-capf-setup))

;; Images in org-mode
(use-package org-download
  :after org
  :config
  (setq-default org-download-image-dir "./images")
  (setq-default org-download-heading-lvl nil)
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-clipboard)
         ("s-y" . org-download-yank))))

;; Fix mixed font faces in org mode
(defun my/org-fonts ()
  "Set up custom faces for org-mode."
  (interactive)
  (set-face-attribute 'org-document-title nil :height 1.5 :weight 'bold)
  (set-face-attribute 'org-level-1 nil :height 1.4 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.3 :weight 'semi-bold)
  (set-face-attribute 'org-level-3 nil :height 1.2 :weight 'semi-bold)
  (set-face-attribute 'org-level-4 nil :height 1.1 :weight 'semi-bold)
  (set-face-attribute 'org-level-5 nil :weight 'semi-bold)
  (set-face-attribute 'org-level-6 nil :weight 'semi-bold)
  (set-face-attribute 'org-level-7 nil :weight 'semi-bold)
  (set-face-attribute 'org-level-8 nil :weight 'semi-bold)
  ;; Keep some faces fixed-pitch that should be monospaced
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; Apply the font settings
(add-hook 'org-mode-hook 'my/org-fonts)

;;; org.el ends here
