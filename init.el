;;; No bars. Doing this first to avoid showing/hidding delay on startup
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;;; Initialize cask
(require 'cask "~/.emacs.d/.cask/cask/cask.el")
(cask-initialize)

;;; utf-8 for good (is there any other encoding related var I could set?)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq current-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;;; Use node binaries on emacs
(setq exec-path
      '(
        "/usr/local/bin/"
        "/usr/bin/"
        "~/.nodenv/shims/"
        "~/.emacs.d/node_modules/.bin/"
        ))

;;; UI Configuration
(load-theme 'deeper-blue)               ;; Theme
(column-number-mode)                    ;; Basic config for columns
(setq ring-bell-function 'ignore)       ;; No freaking bell
(setq inhibit-splash-screen t)          ;; No splash screen
(setq inhibit-startup-screen t)

(global-font-lock-mode 1)               ;; Always do syntax highlighting
(transient-mark-mode 1)                 ;; highlight mark region
(set-default-font "Monaco 10")          ;; Font face/size

(require 'linum)                        ;; show line numbers
(global-linum-mode 1)
(setq linum-format "%d ")
(setq-default truncate-lines t)         ;; Do not wrap lines

;;; Nyan-mode
(nyan-mode)

;;; Also highlight parenthesis
(setq show-paren-delay 0 show-paren-style 'parenthesis)
(show-paren-mode 1)

;;; Autopair
(require 'autopair)
(autopair-global-mode)

;;; Editing options
(setq-default indent-tabs-mode nil)    ;; spaces instead of tabs
(setq make-backup-files nil)           ;; No backup files
(setq scroll-conservatively 10000)     ;; scroll smoothly
(setq show-trailing-whitespace t)      ;; Whitespaces
(setq x-select-enable-clipboard t)     ;; Clipboard shared with the DE

;;; Other small configurations
(setq default-directory "~/")          ;; There's no place like home
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; Trailing whitespace before save

;; Reloading the buffer instead of pissing me off with "what should I
;; do" questions
(defun ask-user-about-supersession-threat (filename)
  ;; (revert-buffer t t)
  (message "This buffer was refreshed due to external changes"))


;;;-------- Keybinds --------

;;; change window
(global-set-key [C-tab] 'other-window)

;;; comments
(global-set-key [(ctrl c) (c)] 'comment-region)
(global-set-key [(ctrl c) (d)] 'uncomment-region)

;;; delete trailing whitespace
(global-set-key [(ctrl x) (w)] 'delete-trailing-whitespace)

;;; buffer
(global-set-key [s-tab] 'next-buffer)
(global-set-key [S-s-iso-lefttab] 'previous-buffer)

;;; Navegation
(global-set-key (kbd "M-g") 'goto-line)

;;; Sort
(global-set-key (kbd "C-c s") 'sort-lines)

;;; List buffers
(bind-key "C-x b" 'helm-buffers-list)

;;; highlight indentation column
(global-set-key (kbd "M-1") 'highlight-indentation-current-column-mode)

;;; scrolling without changing the cursor
(global-set-key [(meta n)] '(lambda () (interactive) (scroll-up 1)))
(global-set-key [(meta p)] '(lambda () (interactive) (scroll-down 1)))

;;; scrolling other window
(global-set-key [(meta j)] '(lambda () (interactive) (scroll-other-window 1)))
(global-set-key [(meta k)] '(lambda () (interactive) (scroll-other-window -1)))

;;; join lines
(global-set-key [(ctrl J)] '(lambda () (interactive) (join-line -1)))

;;; resize windows
(global-set-key (kbd "C-}") 'shrink-window-horizontally)
(global-set-key (kbd "C-{") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-}") 'shrink-window)
(global-set-key (kbd "C-M-{") 'enlarge-window)

;;; Multiple Cursors
(global-set-key (kbd "C-c C-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; Fiplr
(global-set-key (kbd "C-c f") 'fiplr-find-file)
(setq fiplr-ignored-globs '((directories (".git" ".svn"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" "public/*" "tmp/*" "vendor" "bin" "docs" "log" "script"))))

;;; Mac specific stuff
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)

  ;;; Loads environment variables from the shell
  (setq exec-path-from-shell-variables '("GOPATH" "PATH" "MANPATH"))
  (exec-path-from-shell-initialize)

  ;;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char)
  (menu-bar-mode 1))


;;-------- Modes --------

(require 'tramp)           ;; ssh and local sudo/su


;;; Neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; ;;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(flycheck-add-mode 'javascript-jshint 'web-mode)

;;; Flyspell
(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "pt_BR") "english" "pt_BR")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(global-set-key (kbd "<f5>") 'fd-switch-dictionary)
(global-set-key (kbd "<f6>") 'ispell-word)


;;; css config
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 4)

;;; Set CSS colors with themselves
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :foreground
                     (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)

;;; sass mode
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(setq sass-indent-offset 4)

;;; less mode
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
(setq less-compile-at-save nil)

;;; Web mode
(require 'web-mode)
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (set-face-attribute 'web-mode-doctype-face nil :foreground
                      (face-foreground font-lock-function-name-face))
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground
                      (face-foreground font-lock-variable-name-face))
  (set-face-attribute 'web-mode-html-attr-value-face nil :foreground
                      (face-foreground font-lock-type-face)))
(add-hook 'web-mode-hook  'web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;; Zencoding
(require 'zencoding-mode)
(add-hook 'web-mode-hook 'zencoding-mode)

;;; Markdown mode
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook '(lambda() (flyspell-mode)))

;;; Slim-mode
(require 'slim-mode)

;;; Auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-dwim 2)
(ac-config-default)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;;; Loading YAS personal snippets
(require 'yasnippet)
(setq yas-root-directory "~/.emacs.d/snippets")
(yas-global-mode 1)
(yas-load-directory yas-root-directory)

;;; Configuring the dropdown list, submodule used by yasnippet
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt))

;;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq grep-find-ignored-files '("*.json" "*.http"))
(setq projectile-globally-ignored-files '(".git" ".svn" "tmp" "*.json" "*.http"))
(bind-key (kbd "C-c C-p") 'helm-projectile-ack)
