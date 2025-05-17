;;; editor.el --- Editor enhancements -*- lexical-binding: t -*-

;; Editor Enhancements
;; ===================

;; Tabs, spaces, indentation
(setq-default tab-width 2
              indent-tabs-mode nil) ; Use spaces instead of tabs

;; Automatically match parentheses
(electric-pair-mode 1)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show trailing whitespace in prog mode
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Ediff settings
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; Jump to definition with dumb-jump
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read))

;; Code completion with Corfu
(use-package corfu
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Minimum length of prefix for auto completion
  (corfu-auto-delay 0.0)          ; No delay for completion
  (corfu-echo-documentation 0.25) ; Show documentation in echo area
  (corfu-preview-current 'insert) ; Insert the current candidate
  (corfu-preselect-first t)       ; Preselect first candidate
  :init
  (global-corfu-mode)
  :config
  ;; Enable Corfu more generally
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                 corfu-quit-no-match t
                                 corfu-auto nil)
              (corfu-mode))))

;; Add Cape for better completion-at-point
(use-package cape
  :init
  ;; Add to the global default value of completion-at-point-functions.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Popup documentation
(use-package corfu-doc
  :after corfu
  :hook (corfu-mode . corfu-doc-mode)
  :custom
  (corfu-doc-delay 0.5)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20))

;; Snippets
(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (yas-reload-all))

;; Predefined snippets
(use-package yasnippet-snippets
  :after yasnippet)

;; Create a directory for custom snippets
(unless (file-directory-p "~/.emacs.d/snippets")
  (make-directory "~/.emacs.d/snippets"))

;; Add the directory to the snippets path
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")

;; Enhanced text selection
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Spell checking
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

;; Improved clipboard handling
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Enhanced region commenting
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Code folding with tree-sitter
(use-package treesit-fold
  :after treesit
  :bind ("C-c C-f" . treesit-fold-toggle)
  :config
  (global-treesit-fold-mode))

;; Tree-sitter for better syntax handling
(use-package treesit
  :straight (:type built-in)
  :preface
  (defun setup-install-ts-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  :config
  (setup-install-ts-grammars)

  ;; Remap major modes to tree-sitter versions
  (dolist (mapping
           '((css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . js-ts-mode)
             (js2-mode . js-ts-mode)
             (json-mode . json-ts-mode)
             (ruby-mode . ruby-ts-mode)
             (python-mode . python-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

;; Show key bindings
(use-package which-key
  :config
  (which-key-mode))

;;; editor.el ends here
