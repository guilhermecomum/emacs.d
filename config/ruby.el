;;; ruby.el --- Ruby configuration -*- lexical-binding: t -*-

;; Ruby Configuration
;; =================

;; Ruby mode settings
(use-package ruby-ts-mode
  :straight (:type built-in)
  :mode (("\\.rb\\'" . ruby-ts-mode)
         ("\\.rake\\'" . ruby-ts-mode)
         ("Rakefile\\'" . ruby-ts-mode)
         ("Gemfile\\'" . ruby-ts-mode)
         ("\\.gemspec\\'" . ruby-ts-mode)
         ("\\.ru\\'" . ruby-ts-mode)
         ("Guardfile\\'" . ruby-ts-mode)
         ("Capfile\\'" . ruby-ts-mode)
         ("\\.cap\\'" . ruby-ts-mode)
         ("\\.thor\\'" . ruby-ts-mode)
         ("\\.rabl\\'" . ruby-ts-mode)
         ("Thorfile\\'" . ruby-ts-mode)
         ("Vagrantfile\\'" . ruby-ts-mode)
         ("\\.jbuilder\\'" . ruby-ts-mode)
         ("Podfile\\'" . ruby-ts-mode)
         ("\\.podspec\\'" . ruby-ts-mode)
         ("Puppetfile\\'" . ruby-ts-mode)
         ("Berksfile\\'" . ruby-ts-mode)
         ("Appraisals\\'" . ruby-ts-mode))
  :hook ((ruby-ts-mode . lsp-deferred)
         (ruby-ts-mode . (lambda ()
                          (setq-local flycheck-checker 'ruby-rubocop))))
  :config
  (setq ruby-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

;; Rubocop integration
(use-package rubocop
  :hook (ruby-ts-mode . rubocop-mode)
  :config
  (setq rubocop-autocorrect-on-save t))

;; Improved Ruby code formatting
(use-package rubocopfmt
  :hook (ruby-ts-mode . rubocopfmt-mode)
  :config
  (setq rubocopfmt-on-save-use-lsp-format-buffer nil))

;; Enhanced Ruby REPL
(use-package inf-ruby
  :hook ((ruby-ts-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter))
  :bind (:map ruby-ts-mode-map
              ("C-c C-s" . inf-ruby)
              ("C-c C-b" . ruby-send-buffer)
              ("C-c C-r" . ruby-send-region)))

;; RSpec integration
(use-package rspec-mode
  :hook (ruby-ts-mode . rspec-mode)
  :config
  (setq rspec-use-spring-when-possible nil
        rspec-use-bundler-when-possible t))

;; Ruby on Rails integration
(use-package projectile-rails
  :hook (projectile-mode . projectile-rails-global-mode)
  :bind (:map projectile-rails-mode-map
              ("C-c r" . projectile-rails-command-map)))

;; YAML support for config files
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode))
  :hook (yaml-ts-mode . lsp-deferred))

;; Bundler integration
(use-package bundler
  :after ruby-ts-mode
  :bind (:map ruby-ts-mode-map
              ("C-c b i" . bundle-install)
              ("C-c b o" . bundle-open)
              ("C-c b e" . bundle-exec)
              ("C-c b c" . bundle-console)))

;; Improved ERB support
(use-package web-mode
  :mode (("\\.erb\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t))

;; Ruby test runner
(use-package ruby-test-mode
  :hook (ruby-ts-mode . ruby-test-mode)
  :config
  (setq ruby-test-rspec-options '("--format documentation")))

;; Language server setup for Ruby
(use-package lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(ruby-ts-mode . "ruby"))

  ;; Configure solargraph (Ruby LSP server)
  (setq lsp-solargraph-multi-root t
        lsp-solargraph-autoformat nil  ; We use rubocop for formatting
        lsp-solargraph-formatting t
        lsp-solargraph-use-bundler nil))

;; Create custom function for Rails console
(defun rails-console ()
  "Open a Rails console."
  (interactive)
  (let ((default-directory (or (projectile-rails-root) default-directory)))
    (with-current-buffer (run-ruby "bundle exec rails console" "rails"))))

(defun rails-server ()
  "Start a Rails server."
  (interactive)
  (let ((default-directory (or (projectile-rails-root) default-directory)))
    (async-shell-command "bundle exec rails server" "*Rails Server*")))

;; Add keybindings
(global-set-key (kbd "C-c r c") 'rails-console)
(global-set-key (kbd "C-c r s") 'rails-server)

;;; ruby.el ends here
