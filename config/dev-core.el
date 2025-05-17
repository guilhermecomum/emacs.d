;;; dev-core.el --- Core development tools -*- lexical-binding: t -*-

;; Core Development Setup
;; =====================

;; Language Server Protocol
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((typescript-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (web-mode . (lambda ()
                       (when (or (string-match-p "\\.liquid\\'" (buffer-file-name))
                                 (string-match-p "\\.html\\'" (buffer-file-name)))
                         (lsp-deferred))))
         (ruby-ts-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  ;; Performance optimization
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-use-plists t) ;; Use plists for deserialization (faster)

  :custom
  (lsp-keymap-prefix "C-c l")         ; LSP keybindings prefix
  (lsp-enable-file-watchers nil)      ; Disable file watchers for performance
  (lsp-completion-provider :none)     ; We use corfu/cape
  (lsp-headerline-breadcrumb-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-keep-workspace-alive nil)      ; Kill LSP server when closing last buffer
  (lsp-enable-indentation nil)        ; Don't let LSP handle indentation
  (lsp-enable-on-type-formatting nil) ; Don't format as we type (use prettier)
  (lsp-enable-snippet t)              ; Needed for React component snippets
  (lsp-log-io nil)                    ; Don't log LSP communication

  :config
  ;; Set up LSP booster if available
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)
               (not (file-remote-p default-directory))
               lsp-use-plists
               (not (functionp 'json-rpc-connection))
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))

  ;; Apply LSP booster advice if available
  (when (executable-find "emacs-lsp-booster")
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)))

;; LSP UI - enhanced UI for LSP
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-peek-enable t)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c d" . lsp-ui-doc-show)))

;; Debugging
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (dap-ui-mode 1)
  (require 'dap-node))

;; On-the-fly syntax checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-display-errors-delay 0.25)
  :config
  (global-flycheck-mode 1))

;; Improved Prettier Integration for auto-formatting
(use-package prettier-js
  :ensure-system-package prettier
  :hook ((typescript-ts-mode js-ts-mode tsx-ts-mode web-mode) . prettier-js-mode)
  :config
  (setq prettier-js-args '("--trailing-comma" "es5"
                           "--bracket-spacing" "true"
                           "--single-quote" "true"
                           "--jsx-bracket-same-line" "false"
                           "--single-attribute-per-line" "false")))

;; EditorConfig for consistent formatting
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Advanced Git interface
(use-package magit
  :bind ("C-x g" . magit-status))

;; Git time machine for file history
(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine))

;; Compilation
(use-package compile
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-max-output-line-length nil)
  :hook (compilation-mode . hl-line-mode)
  :init
  (add-hook 'compilation-finish-functions
            (lambda (buf str)
              (if (null (string-match ".*exited abnormally.*" str))
                  (progn
                    (run-at-time
                     "1 sec" nil 'delete-windows-on
                     (get-buffer-create "*compilation*"))
                    (message "No Compilation Errors!"))))))

;; Enhanced compilation interface
(use-package fancy-compilation
  :config
  (fancy-compilation-mode))

;; Terminal emulation for running commands
(use-package vterm
  :bind ("C-c t" . vterm))

;; Use ripgrep for faster project search
(use-package ripgrep)

;; Use docker from Emacs
(use-package docker
  :bind ("C-c d" . docker))

(use-package docker-compose-mode)

;; REST client for API testing
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; Show docs for symbol at point
(use-package eldoc-box
  :bind ("C-h ." . eldoc-box-help-at-point))

;; Tailwind CSS completion
(use-package lsp-tailwindcss
  :straight (:host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (mode '(typescript-ts-mode tsx-ts-mode web-mode css-ts-mode))
    (add-to-list 'lsp-tailwindcss-major-modes mode)))

;; Pretty TypeScript errors
(use-package pretty-ts-errors
  :straight (:host github :repo "artawower/pretty-ts-errors.el")
  :hook ((typescript-ts-mode tsx-ts-mode) . pretty-ts-errors-mode))

;; Emmet for HTML expansion
(use-package emmet-mode
  :hook ((web-mode css-mode css-ts-mode tsx-ts-mode) . emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t
        emmet-expand-jsx-className? t
        emmet-self-closing-tag-style " /"))

;;; dev-core.el ends here
