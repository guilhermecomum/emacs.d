;;; init.el --- Personal Emacs configuration  -*- lexical-binding: t; -*-

;; [imagem]


;;; Package Manager

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Block until current queue processed.
(elpaca-wait)

;; Use the built-in Org instead of pulling a newer one from ELPA.
;; Without these entries, packages like org-roam / org-contrib / org-ql
;; declare `org' as a dependency and elpaca installs it, clashing with the
;; built-in version that `org-babel-load-file' already loaded from init.el.
(add-to-list 'elpaca-ignored-dependencies 'org)
(add-to-list 'elpaca-ignored-dependencies 'org-mode)

;; Turns off elpaca-use-package-mode current declaration
;; Note this will cause the declaration to be interpreted immediately (not deferred).
;; Useful for configuring built-in emacs features.
(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

;;; Essentials

;;;; Mac OS

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(undecorated . t))
  ;; Don't use Option as Alt - conflicts with accented character input (see below)
  ;;(setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)

  ;; Note: These A-tab (Alt-tab) keybindings won't work with Option used for accented chars
  ;; Use C-tab or M-` instead for window/frame navigation
  ;; (global-set-key (kbd "<A-tab>") #'other-window)
  ;; (global-set-key (kbd "<A-S-tab>")
  ;;                 #'(lambda () (interactive) (other-window -1)))

  ;; Keys for visiting next & previous frame
  (global-set-key (kbd "M-`") #'other-frame)
  (global-set-key (kbd "M-~") #'(lambda () (interactive) (other-frame -1)))

  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char)
  (menu-bar-mode 1)

  ;; Enable mac option to create accented characters
  (setq ns-alternate-modifier 'none)
  (setq frame-resize-pixelwise t)
  (setq ns-left-alternate-modifier 'none))

;;;; Key mapping

;;;;; Buffer/Window

;; Before killing a modified buffer, give option to see the diff.
;; Original code from https://emacs.stackexchange.com/questions/3245/
(defun my/kill-this-buffer ()
  (interactive)
  (catch 'quit
    (save-window-excursion
      (let (done)
        (when (and buffer-file-name (buffer-modified-p))
          (while (not done)
            (let ((response (read-char-choice
                             (format "Save file %s? (y, n, d, q) " (buffer-file-name))
                             '(?y ?n ?d ?q))))
              (setq done (cond
                          ((eq response ?q) (throw 'quit nil))
                          ((eq response ?y) (save-buffer) t)
                          ((eq response ?n) (set-buffer-modified-p nil) t)
                          ((eq response ?d) (diff-buffer-with-file) nil))))))
        (kill-buffer (current-buffer))))))

(global-set-key [s-tab] 'next-buffer)
(global-set-key [S-s-iso-lefttab] 'previous-buffer)
(global-set-key ["M-{"] 'next-buffer)
(global-set-key ["M-}"] 'previous-buffer)


;; change window
(global-set-key [(C-tab)] 'other-window)
(global-set-key [(C-M-tab)] 'other-window)

;; Remap kill buffer to my/kill-this-buffer
(global-set-key (kbd "C-x k") 'my/kill-this-buffer)

;; Revert buffer
(global-set-key (kbd "C-<f5>") 'revert-buffer)

;; Go to scratch buffer
(global-set-key (kbd "<f2>") (lambda() (interactive)(switch-to-buffer "*scratch*")))

;;;;; Code navigation

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c s") 'sort-lines)

;;;;; Editing

;; Enhanced region commenting
(use-package evil-nerd-commenter
  :ensure t
  :bind ("C-c c" . evilnc-comment-or-uncomment-lines))

;;;;; Conf

(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

;;; General

;; Close all dired buffers after opening
(setq dired-kill-when-opening-new-dired-buffer t)

(defun dont-kill-scratch ()
  "This function doesn't let you kill scratch by mistake."
  (if (not (equal (buffer-name) "*scratch*"))
      t
    (bury-buffer)
    nil))
(add-hook 'kill-buffer-query-functions #'dont-kill-scratch)

;; SECURITY: Don't disable risky variable checks - it's dangerous!
;; Instead, use one of these safer alternatives:
;;
;; Option 1: Automatically accept safe local variables without prompting
;; (setq enable-local-variables :safe)
;;
;; Option 2: Mark specific variables as safe (recommended)
;; (put 'variable-name 'safe-local-variable #'functionp)
;;
;; Option 3: If you really trust all .dir-locals in your projects:
;; (setq enable-local-variables :all)  ; Still safer than disabling checks entirely

(use-package ls-lisp
  :config
  (setq ls-lisp-dirs-first t
        ls-lisp-use-insert-directory-program nil))

;;;; Startup Performance

;; Reduce GC frequency during startup.
;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;;; Native Compilation

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;;;; Encoding

;; From Doom emacs: this is enough to make UTF-8 the default coding system.
(set-language-environment "UTF-8")

;;;; Path

;; Load environment variables from the shell
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize)
  :config
  (setq exec-path-from-shell-variables '("GOPATH" "PATH" "MANPATH")))

;; Set the start point for the current buffer; affects file-search default dir.
(setq default-directory "~/")

;;;; Auth
(setq auth-sources '("~/.authinfo"))

;;; Startup Screen

(use-package dashboard
  :ensure t
  :after nerd-icons
  :config
  (setq dashboard-banner-logo-title "Olá, bem vindo ao Emacs"
        dashboard-startup-banner "~/.emacs.d/nyan-cat.png"
        dashboard-center-content t
        dashboard-agenda-release-buffers t
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((projects . 5) (agenda . 5)))
  :init
  (add-hook 'elpaca-after-init-hook #'dashboard-open))

;;; Keep .emacs.d clean

;; Move transient files out of the repo so they don't show as untracked.
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))


(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      create-lockfiles nil)

(setq tramp-auto-save-directory temporary-file-directory)

;;; Look & Feel

;;;; Improve theme loading

;; Source: https://www.reddit.com/r/emacs/comments/4mzynd/
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

;;;; Theme

;;;;; Doom Themes

(use-package doom-themes
  :ensure t
  :preface
  (setq
   dark-theme "doom-fairy-floss"
   light-theme "doom-solarized-light")
  :config
  (load-theme (intern dark-theme) t)

  (defun gg-switch-theme()
    (interactive)
    (let* ((theme (car custom-enabled-themes))
           (change (if (string= theme light-theme) dark-theme light-theme)))
      (load-theme (intern change) t)
      (setq selected-theme change)
      (message "Theme switched from %s to %s" theme change)))
  (global-set-key (kbd "<f8>") 'gg-switch-theme)

  (set-face-attribute 'default nil :font "Menlo 13")
  (set-face-attribute 'region nil :background "#000" :foreground "#ffffff"))

;;;; Dirvish (file tree / dired)

;; Modern dired replacement with sidebar mode. [f9] toggles a side panel.
(use-package dirvish
  :ensure t
  :after nerd-icons
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                  "Home")
     ("p" "~/Projects/"         "Projects")
     ("o" "~/Projects/org-files/" "Org files")
     ("e" "~/.emacs.d/"         "Emacs config")))
  :config
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-side-width 32)
  :bind (([f9] . dirvish-side)
         :map dirvish-mode-map
         ("a" . dirvish-quick-access)
         ("h" . dired-up-directory)
         ("y" . dirvish-yank-menu)
         ("N" . dirvish-narrow)
         ("?" . dirvish-dispatch)))

;;;; Icons

;; First run: M-x nerd-icons-install-fonts to install the glyph font.
(use-package nerd-icons
  :ensure (nerd-icons :type git :host github
                      :repo "rainstormstudio/nerd-icons.el"
                      :files (:defaults "data")))

;;;; Nyan cat

(use-package nyan-mode
  :ensure t
  :init
  (nyan-mode t))

;;;; Emacs interface

(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(column-number-mode)
(setq ring-bell-function 'ignore)

;; Writing yes or no is long, type y / n instead
(defalias 'yes-or-no-p 'y-or-n-p)

;;;; Doom modeline

(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :config
  (setq doom-modeline-height 35
        doom-modeline-bar-width 1
        doom-modeline-icon t)
  (set-face-background 'doom-modeline-bar (face-background 'mode-line))
  (doom-modeline-mode 1))

;;;; Dialog

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;;;; Completion (Corfu)

;; Modern in-buffer completion stack: corfu (UI) + cape (extra capfs) + kind-icon (icons).
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  :config
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Make TAB do completion when there's nothing to indent.
(setq tab-always-indent 'complete)

;;; Editing

;; Remembering the last place you visited in a file
(save-place-mode 1)

(setq-default truncate-lines t          ; Do not wrap lines
              indent-tabs-mode nil)     ; spaces instead of tabs

(setq show-trailing-whitespace t        ; Complain about trailing white spaces
      whitespace-style '(face trailing lines tabs big-indent))

;; Cleanup whitespace before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Yank ring
(global-set-key (kbd "C-M-y") 'yank-pop)

;;;; Parenthesis

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-local-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode :ensure t)

(use-package string-inflection
  :ensure t
  :bind ("C-c i" . string-inflection-cycle))

(global-hl-line-mode t)

;;;; Display line numbers

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;;;; Outline

;; Fold/navigate elisp files by their `;;;'-style heading comments.
;; `outline-minor-faces' styles headings like org-mode; `backline'
;; extends the heading highlight across the full line.
(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(setq outline-minor-mode-cycle t)

;; Only treat `;;;'+-comments as headings — without this, every top-level
;; form (defun, use-package, setq) is reported as an outline entry too.
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local outline-regexp ";;;\\(;*\\) ")))

(use-package outline-minor-faces
  :ensure t
  :after outline
  :hook (outline-minor-mode . outline-minor-faces-mode))

(use-package backline
  :ensure t
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

;; Hide the leading `;;;' prefix on headings for a cleaner org-like look.
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("^\\(;;;+\\) "
                           1 '(face nil display "") prepend)))

;; Scale outline headings so each level reads larger than regular text.
;; Buffer-local remap keeps the effect in source buffers only — consult
;; result lists (which render inside mini-frame) stay at normal size.
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (face-remap-add-relative 'outline-1 '(:height 1.4  :weight bold))
            (face-remap-add-relative 'outline-2 '(:height 1.25 :weight bold))
            (face-remap-add-relative 'outline-3 '(:height 1.15 :weight bold))
            (face-remap-add-relative 'outline-4 '(:height 1.1  :weight bold))
            (face-remap-add-relative 'outline-5 '(:height 1.05 :weight bold))))

;;;; Indent Guides

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character))

;;;; Multiple cursor

(use-package multiple-cursors
  :ensure t
  :bind (("A-S-c A-S-c" . mc/edit-lines)
         ("C-S-n" . mc/mark-next-like-this)
         ("C-S-p" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this)
         ("C-c m n" . mc/mark-next-lines)
         ("C-c m p" . mc/mark-previous-lines)
         ("C-A-<mouse-1>" . mc/add-cursor-on-click)))

;;;; Unfill paragraph

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph or (REGION) and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;; Treesitter

;; Use the built-in Emacs 29+ grammar installer. Run
;; `M-x treesit-install-language-grammar' per language, or evaluate
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
;; to install all at once.
(require 'treesit)

(setq treesit-language-source-alist
      '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
        (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
        (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
        (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))))

;; Remap classic modes to their tree-sitter counterparts.
(dolist (mapping '((css-mode        . css-ts-mode)
                   (javascript-mode . js-ts-mode)
                   (js-json-mode    . json-ts-mode)
                   (typescript-mode . typescript-ts-mode)
                   (ruby-mode       . ruby-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

;; .tsx files use tsx-ts-mode (typescript-ts-mode handles plain .ts).
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))

;;; Flymake

(use-package sideline-flymake
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :custom
  (flymake-error-bitmap '(my-rounded-fringe-indicator compilation-error))
  (flymake-note-bitmap '(my-rounded-fringe-indicator compilation-info))
  (flymake-warning-bitmap '(my-rounded-fringe-indicator compilation-warning))
  :init
  (setq sideline-flymake-display-errors-whole-line 'point ; 'point to show errors only on point
        sideline-backends-right '(sideline-flymake)))    ; 'line to show errors on the current line

;;;; Custom Fringe

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'my-rounded-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000)))

;;;; Eslint

;; source: https://github.com/angrybacon/dotemacs/blob/master/lisp/use-lint.el
(use-package flymake-eslint
  :ensure t
  :functions flymake-eslint-enable
  :preface
  (defun flymake-eslint-enable-maybe ()
    "Enable `flymake-eslint' based on the project configuration.
Search for the project ESLint configuration to determine whether the buffer
should be checked."
    (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json"))
                (rc (locate-file ".eslintrc" (list root) '(".js" ".json"))))
      (make-local-variable 'exec-path)
      (push (file-name-concat root "node_modules" ".bin") exec-path)
      (setq-local flymake-eslint-project-root root)
      (flymake-eslint-enable))))

;;; Jinx (spell check)

;; Modern, fast spell checker via enchant. Requires `brew install enchant' on macOS.
;; On first run install dictionaries (e.g. `brew install hunspell' and place
;; pt_BR + en_US dicts in ~/Library/Spelling/).
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-;"  . jinx-correct)
         ("<f7>" . jinx-languages)
         ("C-M-$" . jinx-languages))
  :custom
  (jinx-languages "pt_BR en_US"))

;;; Yasnippet

(use-package yasnippet
  :ensure t
  :init
  :config
  (yas-load-directory "~/.emacs.d/snippets")
  (yas-global-mode 1))

;;; Code Folding

(use-package treesit-fold
  :ensure (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :hook (prog-mode . treesit-fold-mode)
  :bind (("C-c C-f" . treesit-fold-toggle)
         ("C-c C-S-f" . treesit-fold-close-all)))

;;; Restclient

(use-package restclient :ensure t)

;;; Projectile

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (("C-c p" . projectile-command-map)
         ("M-[" . projectile-previous-project-buffer)
         ("M-]" . projectile-next-project-buffer))
  :config
  (setq projectile-indexing-method 'hybrid
        projectile-sort-order 'recently-active
        compilation-read-command nil
        projectile-comint-mode t)

  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-files "yarn.lock")
  :custom
  (projectile-globally-ignored-buffers '("*scratch*" "*lsp-log*" "*xref*" "*EGLOT" "*Messages*" "*compilation" "*vterm*" "*Flymake")))

;;; Magit

;; Magit requires transient >= 0.8.8, but Emacs ships an older built-in
;; version. We install transient explicitly so Magit picks the newer one.
(use-package transient :ensure t)
(use-package magit
  :ensure t
  :after transient)

;;; Org

;; org-agenda-files is machine-specific and set in ~/.emacs.d/local.el
(use-package org
  :ensure nil
  :custom
  (org-agenda-span 15)
  (org-deadline-warning-days 0)
  (org-icalendar-deadline-summary-prefix "")
  (org-icalendar-timezone "")
  (org-icalendar-use-deadline '(event-if-todo todo-due))
  (org-icalendar-with-timestamps nil)
  :bind (("C-c a" . (lambda () (interactive) (org-agenda nil "z")) )
         ("C-c /" . 'org-capture)
         ("s-c" . 'ox-clip-formatted-copy)))

(use-package org-contrib
  :ensure t
  :config
  (require 'org-inlinetask)
  (require 'org-tempo)
  (require 'org-collector))

(use-package org-web-tools
  :ensure t
  :custom
  (org-web-tools-pandoc-sleep-time 1.8))

(use-package org-ql
  :ensure (org-ql
           :type git :host github
           :repo "alphapapa/org-ql")
  :after '(org))


(use-package git-auto-commit-mode :ensure t)
(use-package ox-clip :ensure t)

(setq org-export-coding-system 'utf-8
      org-tag-alist '(("work" . ?w) ("personal" . ?p) ("meta" . ?m) ("emacsLove" . ?l) ("quotes" . ?q) ("finances" . ?f) ("howto" . ?h))
      org-log-done nil
      org-log-repeat nil
      org-startup-indented t
      org-export-with-toc nil
      org-export-with-section-numbers nil
      gac-automatically-push-p t)

;;;; Ox

;;;;; Slack

(use-package ox-slack
  :ensure t
  :bind ("C-c e s" . org-slack-export-to-clipboard-as-slack))

;;;; Reveal

(use-package ox-reveal :ensure t)
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"
      org-reveal-title-slide nil
      org-reveal-mathjax t)
(use-package htmlize :ensure t)

;;;; Look & Feel

;;;;; Olivetti

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 120))

;;;; Super-agenda

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (org-super-agenda-mode t)
  (setq org-agenda-skip-scheduled-if-done t))

(setq org-agenda-custom-commands
      '(("z" "Super view"
         ((tags "meta" ((org-agenda-overriding-header "Objetivos de 2023")))
          (agenda "" ((org-agenda-span 'week)
                      (org-agenda-overriding-header "")))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-agenda-remove-tags t)
                       (org-super-agenda-groups
                        '((:name "🚨 Atrasados"
                                 :deadline past
                                 :order 7)
                          (:name "Próximos eventos"
                                 :discard (:tag ("finances"))
                                 :deadline future
                                 :order 8)
                          (:name "Sem data" :deadline nil :order 9)
                          (:discard (:tag ("Routine" "Daily" "meta" "finances")))))))))))

;;;; Functions

;; Check if a billing is paid based on the date
(defun is-paid? (time)
  (if (eq (string-to-number (format-time-string "%m")) (nth 4 (org-parse-time-string time)))
      "-" "pago"))

;; Add ID to all headings
;; source: https://stackoverflow.com/questions/13340616/
(defun add-id-to-tasks-in-file ()
  "Add ID properties to all tasks in the current file which
  do not already have one."
  (interactive)
  (org-ql-select (buffer-file-name)
    '(and
      (todo))
    :action #'org-id-get-create))

;;;; Roam

(use-package org-roam
  :ensure t
  :custom
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("<f4>" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("<f12>" . org-roam-dailies-goto-today)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

;;;; Sync

(defun org-agenda-export-to-ics ()
  (interactive)
  (org-icalendar-combine-agenda-files)
  (copy-file org-agenda-private-local-path org-agenda-private-remote-path t))

(use-package midnight
  :ensure nil
  :config
  (midnight-delay-set 'midnight-delay 16200)
  :hook (midnight . org-agenda-export-to-ics)
  :bind ("C-c e i" . org-agenda-export-to-ics))

;;;; Babel

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-src-window-setup 'current-window)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sql . t)))

;;; Markdown

(use-package markdown-mode :ensure t)

;;; Web mode

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-enable-auto-indentation nil
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-content-types-alist  '(("django" . "\\.tpl\\'") ("django" . "\\.liquid\\'"))))

;; yasnippet
(eval-after-load 'yasnippet
  '(let ((dir "~/.emacs.d/snippets/web-mode"))
     (add-to-list 'yas-snippet-dirs dir)
     (yas-load-directory dir)))

;; liquid
(define-derived-mode liquid-mode web-mode "Liquid"
  "Use web mode to highlight liquid files.")
(provide 'liquid-mode)
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . liquid-mode))

;;; Zencoding

(use-package emmet-mode
  :ensure t
  :hook ((web-mode tsx-ts-mode typescript-ts-mode) . emmet-mode)
  :config
  (setq emmet-indent-after-insert nil
        emmet-indentation 2
        emmet-expand-jsx-className? t
        emmet-move-cursor-between-quotes t
        emmet-self-closing-tag-style " /")
  (add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode))

;;; Javascript

(setq js-indent-level 2)

;;;; Apheleia (formatting)

;; Async formatting that doesn't fight the cursor. Auto-detects prettier,
;; biome, rubocop, etc. from project config.
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;;;; Jest mode

(use-package jest-test-mode
  :ensure t
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

;;; Typescript

;;;; Mode

(use-package typescript-ts-mode
  :ensure nil
  :ensure-system-package (typescript-language-server . "npm i -g typescript-language-server"))

(defun node-project-p ()
  "Predicate for determining if the open project is a Node one."
  (let ((p-root (cdr (project-current))))
    (file-exists-p (concat p-root "package.json"))))

;; source: https://github.com/emacs-typescript/typescript.el
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;; ts-comint

;; xterm-color for proper ANSI color handling in comint buffers.
(use-package xterm-color
  :ensure t
  :config
  ;; Disable the default ansi-color handling in compilation buffers
  (setq compilation-environment '("TERM=xterm-256color"))

  ;; Remove the default ansi-color handling
  (defun advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  (advice-add 'compilation-filter :around #'advice-compilation-filter))

(use-package ts-comint
  :ensure (ts-comint
           :type git :host github
           :repo "nverno/ts-comint")
  :after xterm-color
  :config
  ;; Configure ts-comint to use xterm-color for better ANSI color support
  (setenv "NODE_NO_READLINE" "1"))

;;; Ruby

(use-package flymake-ruby :ensure t)
(add-hook 'ruby-ts-mode-hook 'flymake-ruby-load)

;;;; Rubocop

;; Linting via rubocop interactive commands; formatting handled by apheleia.
(use-package rubocop
  :ensure t)

;;; Deno

(defun deno-project-p ()
  "Predicate for determining if the open project is a Deno one."
  (let ((p-root (cdr (project-current))))
    (file-exists-p (concat p-root "deno.json"))))

;;; Elisp

;;;; Unit Test

;; Buttercup
(use-package buttercup :ensure t)

;;; JSON

(use-package json-mode :ensure t)

;;; YAML

(use-package yaml-mode :ensure t)

;;; Eglot

;; Original code from https://github.com/joaotavora/eglot/discussions/999
(defun ecma-server-program (_)
  "Decide which server to use for ECMA Script based on project characteristics."
  (cond ((deno-project-p) '("deno" "lsp" :initializationOptions (:enable t :lint t)))
        ((node-project-p) '("typescript-language-server" "--stdio"))
        (t                nil)))

;; source: https://manueluberti.eu/2022/09/01/consult-xref.html
(defun mu-project-find-regexp ()
  "Use `project-find-regexp' with completion."
  (interactive)
  (defvar xref-show-xrefs-function)
  (let ((xref-show-xrefs-function #'consult-xref))
    (if-let ((tap (thing-at-point 'symbol)))
        (project-find-regexp tap)
      (call-interactively #'project-find-regexp))))

(defun eglot-shutdown-project ()
  "Kill the LSP server for the current project if it exists."
  (when-let ((server (eglot-current-server)))
    (ignore-errors (eglot-shutdown server))))

(use-package eglot
  :ensure nil
  :init
  (put 'eglot-server-programs 'safe-local-variable 'listp)
  :hook
  (typescript-ts-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (web-mode . eglot-ensure)
  (liquid-mode . eglot-ensure)
  (ruby-ts-mode . eglot-ensure)
  (prisma-mode . eglot-ensure)
  (sql-mode . eglot-ensure)
  (eglot-managed-mode . flymake-eslint-enable-maybe)

  :bind (:map eglot-mode-map
              ("C-c ." . eglot-code-actions)
              ("C-c e r" . eglot-rename)
              ("C-c e f" . eglot-format)
              ("C-c C-d" . eglot-help-at-point)
              ("M-?" . xref-find-references)
              ("M-." . xref-find-definitions)
              ("C-c f n" . flymake-goto-next-error)
              ("C-c f p" . flymake-goto-prev-error)
              ("C-c f d" . flymake-show-project-diagnostics))
  :custom
  (eglot-autoshutdown t)
  (eglot-menu-string "LSP")
  (eglot-confirm-server-initiated-edits nil)
  :config
  (setq eglot-sync-connect 1)
  (fset #'jsonrpc--log-event #'ignore)
  (put 'eglot-error 'flymake-overlay-control nil)
  (put 'eglot-note 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil)
  (advice-add 'project-kill-buffers :before #'eglot-shutdown-project)
  (defclass eglot-sqls (eglot-lsp-server) () :documentation "SQL's Language Server")
  (add-to-list 'eglot-server-programs '((liquid-mode) . ("shopify" "theme" "language-server")))
  (add-to-list 'eglot-server-programs '(sql-mode . (eglot-sqls "sqls")))
  (add-to-list 'eglot-server-programs '((js-ts-mode tsx-ts-mode typescript-ts-mode) . ecma-server-program))

  ;; source https://github.com/joaotavora/eglot/issues/523#issuecomment-1746342643
  (defun sloth/org-babel-edit-prep (info)
    (setq buffer-file-name (or (alist-get :file (caddr info))
                               "org-src-babel-tmp"))
    (eglot-ensure))

  (advice-add 'org-edit-src-code
              :before (defun sloth/org-edit-src-code/before (&rest args)
                        (when-let* ((element (org-element-at-point))
                                    (type (org-element-type element))
                                    (lang (org-element-property :language element))
                                    (mode (org-src-get-lang-mode lang))
                                    ((eglot--lookup-mode mode))
                                    (edit-pre (intern
                                               (format "org-babel-edit-prep:%s" lang))))
                          (if (fboundp edit-pre)
                              (advice-add edit-pre :after #'sloth/org-babel-edit-prep)
                            (fset edit-pre #'sloth/org-babel-edit-prep))))))

;;; Eldoc

(use-package eldoc-box
  :ensure t
  :bind ("C-h ." . eldoc-box-help-at-point))

;;; SQL

(use-package sql-indent :ensure t)
(use-package sqlformat
  :ensure t
  :config
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g"))
  :hook (sql-mode . sqlformat-on-save-mode)
  :bind (:map sql-mode-map ("C-c C-f" . sqlformat)))

;;; Vertigo

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-group-separator ((t (:inherit all-the-icons-dorange :strike-through t))))
  (vertico-group-title ((t (:inherit all-the-icons-dorange :slant italic)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;; Mini-frame

;; Centered child-frame for any minibuffer prompt (M-x, find-file, vertico
;; completion, etc). Replaces the previous vertico-posframe setup.
(use-package mini-frame
  :ensure t
  :custom
  (mini-frame-show-parameters
   (lambda ()
     `((top    . 0.3)
       (width  . 0.7)
       (left   . 0.5)
       (left-fringe  . 8)
       (right-fringe . 8)
       (child-frame-border-width . 3)
       ;; Subtle background tint so the frame reads as a dropdown.
       (background-color . ,(face-attribute 'mode-line :background nil t)))))
  ;; Mirror old vertico-posframe behavior: commands that benefit from
  ;; seeing surrounding buffer context keep the regular minibuffer.
  (mini-frame-ignore-commands
   '(eval-expression
     "edebug-eval-expression"
     debugger-eval-expression
     consult-line
     consult-theme
     consult-ripgrep
     consult-org-heading
     consult-xref
     consult-imenu))
  :config
  ;; Border color picks up the theme's mode-line foreground for contrast.
  (set-face-background 'child-frame-border
                       (face-attribute 'mode-line :foreground nil t))
  (mini-frame-mode 1))

;;; Consult

(use-package consult
  :ensure t
  :bind (("C-M-l" . consult-imenu)
         ("C-s" . consult-line)
         ("C-M-g" . consult-ripgrep)
         ("C-M-o" . consult-outline)
         ("C-x C-b" . consult-buffer)
         ("C-x b" . consult-project-buffer))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (autoload 'projectile-project-root "projectile")
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;;;; Consult org

(use-package consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))

;;; which-key

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; Commit

;; Javascript
(use-package js-comint :ensure t)

;;; CSV Mode

(use-package csv-mode :ensure t)

;;; Compile

(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-max-output-line-length nil)
  :hook (compilation-mode . hl-line-mode)
  :init
  ;; from enberg on #emacs
  (add-hook 'compilation-finish-functions
            (lambda (buf str)
              (if (null (string-match ".*exited abnormally.*" str))
                  ;; no errors, make the compilation window go away in a few seconds
                  (progn
                    (run-at-time
                     "1 sec" nil 'delete-windows-on
                     (get-buffer-create "*compilation*"))
                    (message "No Compilation Errors!"))))))

;;;; Recompile on Save

(use-package recompile-on-save
  :ensure t
  ;; Kill the buffer message that pops up after running advice on compile
  :hook (after-init . (lambda () (run-at-time 1 nil
                                              (lambda ()
                                                (when (get-buffer "*Compile-Log*")
                                                  (kill-buffer "*Compile-Log*"))
                                                (delete-other-windows)))))
  :init
  (recompile-on-save-advice compile))

;;; GraphQL

(use-package graphql-mode
  :ensure t)

;;; Local Config

;; Per-machine overrides live in ~/.emacs.d/local.el (gitignored).
;; See local.el.example for a template. Loaded last so any setting here
;; overrides the base config.
(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load local-config)))
