;;; typescript.el --- TypeScript/JavaScript configuration -*- lexical-binding: t -*-

;; TypeScript and JavaScript Configuration
;; ======================================

;; Install and configure typescript and javascript modes
(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.js\\'" . js-ts-mode))
  :hook ((typescript-ts-mode . prettier-js-mode)
         (js-ts-mode . prettier-js-mode))
  :config
  (setq typescript-indent-level 2
        js-indent-level 2))

;; TSX mode for React components
(use-package tsx-ts-mode
  :straight (:type built-in)
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode))
  :hook (tsx-ts-mode . prettier-js-mode)
  :config
  (setq typescript-indent-level 2
        js-indent-level 2))

;; LSP TypeScript Server configuration
(use-package lsp-mode
  :config
  (setq lsp-typescript-format-enable nil      ; we use prettier
        lsp-typescript-suggest-complete-function-calls t
        lsp-typescript-update-imports-on-file-move-enabled "always"
        lsp-javascript-format-enable nil      ; we use prettier
        lsp-javascript-suggest-complete-function-calls t
        lsp-javascript-update-imports-on-file-move-enabled "always"
        lsp-eslint-enable t
        lsp-eslint-run "onSave"))

;; Node.js REPL
(use-package nodejs-repl
  :commands nodejs-repl
  :bind (("C-c C-e" . nodejs-repl-send-last-expression)
         ("C-c C-j" . nodejs-repl-send-line)
         ("C-c C-r" . nodejs-repl-send-region)
         ("C-c C-l" . nodejs-repl-load-file)
         ("C-c C-z" . nodejs-repl-switch-to-repl)))

;; Remix specific stuff - routes and loaders
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("routes\\." . web-mode))
  (add-to-list 'auto-mode-alist '("loaders\\." . web-mode)))

;; JSON mode
(use-package json-mode
  :mode ("\\.json\\'" . json-ts-mode)
  :hook (json-ts-mode . prettier-js-mode))



;; Better JSX editing support
(use-package rjsx-mode
  :hook (rjsx-mode . lsp-deferred)
  :config
  (add-to-list 'magic-mode-alist '("\\(import.*from \\|export\\).*" . rjsx-mode)))

;; Graphql mode for remix and apollo
(use-package graphql-mode
  :mode ("\\.graphql\\'" . graphql-mode))

;; Prisma support for database schema
(use-package prisma-mode
  :straight (:host github :repo "pimeys/emacs-prisma-mode")
  :mode ("\\.prisma\\'" . prisma-mode))

;; Package.json support
(use-package npm-mode
  :hook ((typescript-ts-mode js-ts-mode tsx-ts-mode) . npm-mode)
  :config
  (npm-global-mode))

;; Auto rename paired tags
(use-package auto-rename-tag
  :hook ((tsx-ts-mode web-mode) . auto-rename-tag-mode))

;; Tailwind CSS intellisense
(use-package lsp-tailwindcss
  :after lsp-mode)

;; Add eslint and typescript to path if needed
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (dolist (cmd '("eslint" "typescript"))
      (unless (executable-find cmd)
        (warn "%s not found in exec-path, linting/typechecking might not work properly" cmd)))))

;; Create custom function for running npm/yarn scripts easily
(defun run-npm-script ()
  "Run an npm script from package.json"
  (interactive)
  (let* ((default-directory (or (project-root (project-current))
                               default-directory))
         (cmd (if (file-exists-p (concat default-directory "yarn.lock"))
                  "yarn"
                "npm"))
         (scripts (shell-command-to-string (concat cmd " run")))
         (script (completing-read "Run script: " (split-string scripts "\n" t))))
    (compile (concat cmd " run " script))))

(global-set-key (kbd "C-c n r") 'run-npm-script)

;;; typescript.el ends here
