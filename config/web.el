;;; web.el --- Web development configuration -*- lexical-binding: t -*-

;; Web Development Configuration (HTML, CSS, Shopify)
;; ================================================

;; Web mode for templates, HTML, etc.
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.liquid\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :hook ((web-mode . lsp-deferred)
         (web-mode . prettier-js-mode)
         (web-mode . emmet-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-expanding t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t))

;; CSS mode
(use-package css-ts-mode
  :straight (:type built-in)
  :mode (("\\.css\\'" . css-ts-mode)
         ("\\.scss\\'" . css-ts-mode))
  :hook ((css-ts-mode . lsp-deferred)
         (css-ts-mode . prettier-js-mode)))

;; SCSS mode for advanced CSS
(use-package scss-mode
  :mode "\\.scss\\'"
  :hook (scss-mode . lsp-deferred))

;; Color visualization
(use-package rainbow-mode
  :hook (css-ts-mode scss-mode web-mode))

;; Shopify-specific configurations
(use-package lsp-mode
  :config
  ;; Register liquid files with LSP
  (add-to-list 'lsp-language-id-configuration '("\\.liquid\\'" . "liquid"))

  ;; Register Shopify theme language server
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("shopify" "theme" "language-server"))
                    :activation-fn (lambda (file-name major-mode)
                                      (or (string-match-p "\\.liquid\\'" file-name)
                                          (and (string-match-p "\\.json\\'" file-name)
                                               (string-match-p "/assets/" file-name))))
                    :priority -1
                    :server-id 'shopify-theme-ls))

  ;; Add snippets for Shopify theme development
  (with-eval-after-load 'yasnippet
    (add-to-list 'yas-snippet-dirs (expand-file-name "snippets/shopify" user-emacs-directory))))

;; Create Shopify snippets directory if it doesn't exist
(let ((shopify-snippets-dir (expand-file-name "snippets/shopify" user-emacs-directory)))
  (unless (file-directory-p shopify-snippets-dir)
    (make-directory shopify-snippets-dir t)))

;; Create some basic Shopify snippets
(when (and (file-directory-p (expand-file-name "snippets/shopify" user-emacs-directory))
           (not (file-exists-p (expand-file-name "snippets/shopify/liquid-for.yasnippet" user-emacs-directory))))
  (with-temp-file (expand-file-name "snippets/shopify/liquid-for.yasnippet" user-emacs-directory)
    (insert "# -*- mode: snippet -*-\n# name: liquid-for\n# key: for\n# --\n{% for ${1:item} in ${2:collection} %}\n  $0\n{% endfor %}"))

  (with-temp-file (expand-file-name "snippets/shopify/liquid-if.yasnippet" user-emacs-directory)
    (insert "# -*- mode: snippet -*-\n# name: liquid-if\n# key: if\n# --\n{% if ${1:condition} %}\n  $0\n{% endif %}"))

  (with-temp-file (expand-file-name "snippets/shopify/liquid-assign.yasnippet" user-emacs-directory)
    (insert "# -*- mode: snippet -*-\n# name: liquid-assign\n# key: assign\n# --\n{% assign ${1:variable} = ${2:value} %}")))

;; HTML live preview
(use-package simple-httpd
  :config
  (setq httpd-port 8888
        httpd-root (expand-file-name "www" user-emacs-directory)))

(use-package impatient-mode
  :commands impatient-mode)

(defun html-live-preview ()
  "Open a browser to preview the current HTML file."
  (interactive)
  (when (not (file-directory-p (expand-file-name "www" user-emacs-directory)))
    (make-directory (expand-file-name "www" user-emacs-directory)))
  (let ((file-name (buffer-file-name)))
    (when file-name
      (let ((file-extension (file-name-extension file-name)))
        (if (or (string= file-extension "html")
                (string= file-extension "liquid"))
            (progn
              (impatient-mode 1)
              (httpd-start)
              (browse-url (format "http://localhost:%d/imp/live/%s"
                                 httpd-port
                                 (url-hexify-string (buffer-name)))))
          (message "Not an HTML file."))))))

(global-set-key (kbd "C-c C-p") 'html-live-preview)

;; Install node_modules for Shopify theme development if necessary
(defun install-shopify-theme-server ()
  "Install shopify theme language server if it's missing."
  (interactive)
  (unless (executable-find "shopify")
    (if (executable-find "npm")
        (async-shell-command "npm install -g @shopify/theme-language-server" "*Shopify Theme Server Installation*")
      (if (executable-find "yarn")
          (async-shell-command "yarn global add @shopify/theme-language-server" "*Shopify Theme Server Installation*")
        (message "Could not find npm or yarn. Please install @shopify/theme-language-server manually.")))))

;; Automatically check for Shopify theme server when loading this file
(when (and (not (executable-find "shopify"))
           (or (executable-find "npm") (executable-find "yarn")))
  (when (yes-or-no-p "Shopify theme language server not found. Install it now? ")
    (install-shopify-theme-server)))

;; Function to validate Shopify theme
(defun shopify-theme-validate ()
  "Validate the current Shopify theme."
  (interactive)
  (if (executable-find "theme")
      (let ((default-directory (or (project-root (project-current))
                                  default-directory)))
        (compile "theme check"))
    (message "Shopify Theme CLI not found. Install it with 'gem install shopify-cli'.")))

;; REST API testing for Shopify
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))

;; Add additional Shopify specific key bindings
(global-set-key (kbd "C-c s v") 'shopify-theme-validate)

;;; web.el ends here
