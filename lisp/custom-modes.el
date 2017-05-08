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

(defun custom-modes-yaml ()
  "Configuration for yaml-mode."
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  ;; Salt Stack Files
  (add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode)))

(defun custom-modes-web ()
  "Configuration for web-mode."
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

      (set-face-attribute 'web-mode-doctype-face nil :foreground
                          (face-foreground font-lock-function-name-face))
      (set-face-attribute 'web-mode-html-attr-name-face nil :foreground
                          (face-foreground font-lock-variable-name-face))
      (set-face-attribute 'web-mode-html-attr-value-face nil :foreground
                          (face-foreground font-lock-type-face)))))

(defun custom-modes-js ()
  "Setup javascript mode."
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

  (setq js2-basic-offset 2)

  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override nil)
  (setq js2-strict-trailing-comma-warning nil)

  (eval-after-load 'rjsx-mode
    '(add-hook 'js2-mode-hook #'add-node-modules-path)))


(defun custom-modes ()
  "Call out all the mode setup functions."
  (custom-modes-yaml)
  (custom-modes-web)
  (custom-modes-js))

(provide 'custom-modes)
;;; custom-modes.el ends here
