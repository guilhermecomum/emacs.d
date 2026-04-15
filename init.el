;; Don't load theme here - it triggers package.el before Elpaca initializes
;; Theme is configured in readme.org via doom-themes

(add-to-list 'load-path "~/Projects/emacs-mcp/")
(require 'emacs-mcp)

(org-babel-load-file "~/.emacs.d/readme.org")
