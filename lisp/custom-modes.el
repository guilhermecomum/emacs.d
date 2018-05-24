;;; custom-modes.el --- Configuration for Modes
;;
;;; Commentary:
;;
;; Configuration for different modes, like markdown, vala, css, etc.
;;
;;; Code:

(require 'web-mode)
(require 'yaml-mode)
(require 'rjsx-mode)
(require 'prettier-js)

(defun custom-modes-yaml ()
  "Configuration for yaml-mode."
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  ;; Salt Stack Files
  (add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode)))

(defun custom-modes-web ()
  "Configuration for web-mode."

  (defun enable-minor-mode (my-pair)
  ;;; https://github.com/prettier/prettier-emacs
    "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
    (if (buffer-file-name)
        (if (string-match (car my-pair) buffer-file-name)
            (funcall (cdr my-pair)))))

  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-hook
   'web-mode-hook
   '(lambda ()
      (setq web-mode-attr-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-enable-current-column-highlight t)
      (setq web-mode-enable-current-element-highlight t)
      (setq web-mode-markup-indent-offset 2)
      ;; Disable auto-indent after yank
      (setq web-mode-enable-auto-indentation nil)
      (setq css-indent-offset 2)
      (enable-minor-mode
       '("\\.jsx?\\'" . prettier-js-mode))
      (set-face-attribute 'web-mode-doctype-face nil :foreground
                          (face-foreground font-lock-function-name-face))
      (set-face-attribute 'web-mode-html-attr-name-face nil :foreground
                          (face-foreground font-lock-variable-name-face))
      (set-face-attribute 'web-mode-html-attr-value-face nil :foreground
                          (face-foreground font-lock-type-face)))))

(defun custom-modes-js ()
  "Setup javascript mode."
  ;;(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (setq js2-basic-offset 2)

  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override nil)
  (setq js2-strict-trailing-comma-warning nil)

  ;; disable auto-indent
  (add-hook 'js2-mode-hook (lambda () (electric-indent-local-mode -1)))

  ;; enable prettier on rjsx
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)

  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(json-jsonlist)))

  ;; flow auto complete
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-flow))

  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook #'add-node-modules-path)))


(defun custom-modes ()
  "Call out all the mode setup functions."
  (custom-modes-yaml)
  (custom-modes-web)
  (custom-modes-js))

(provide 'custom-modes)
;;; custom-modes.el ends here
