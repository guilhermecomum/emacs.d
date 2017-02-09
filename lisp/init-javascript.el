;;; Commentary:
;; Javascript file configurations

;;; Code:

(require 'company)
(require 'quickrun)
(require 'web-mode)
(require 'flycheck)
(require 'flycheck-flow)
(require 'add-node-modules-path)

;; flow auto complete
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-flow))

;; quickrun
(quickrun-add-command "babel"
                      '((:command . "babel-node")
                        (:exec    . "%c %s")))

(quickrun-set-default "javascript" "babel")

;; Custom fringe indicator
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'my-flycheck-fringe-indicator
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

(flycheck-define-error-level 'error
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'my-flycheck-fringe-indicator
  :fringe-face 'flycheck-fringe-error)

(flycheck-define-error-level 'warning
  :overlay-category 'flycheck-warning-overlay
  :fringe-bitmap 'my-flycheck-fringe-indicator
  :fringe-face 'flycheck-fringe-warning)

(flycheck-define-error-level 'info
  :overlay-category 'flycheck-info-overlay
  :fringe-bitmap 'my-flycheck-fringe-indicator
  :fringe-face 'flycheck-fringe-info)

;; add eslint and flow checkers to flycheck
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-flow 'web-mode)

;;disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))


(defun jsWithEslint ()
  "eslint for js files"
  (interactive)
  (web-mode)
  (web-mode-set-content-type "jsx")
  (flycheck-disable-checker 'javascript-flow)
  (flycheck-select-checker 'javascript-eslint)
  (flycheck-mode))

(defun jsWithEslintFlow ()
  "flow and eslint for js files"
  (interactive)
  (web-mode)
  (web-mode-set-content-type "jsx")
  (flycheck-select-checker 'javascript-eslint)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)
  (flycheck-mode))

(global-set-key (kbd "C-c j") 'jsWithEslint)
(global-set-key (kbd "C-c f") 'jsWithEslintFlow)

(add-to-list 'auto-mode-alist '("\\.js\\'"      . jsWithEslint))
(add-to-list 'magic-mode-alist '("/\\* @flow \\*/" . jsWithEslintFlow))

(add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
(add-to-list 'web-mode-comment-formats '("javascript" . "//" ))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-attr-indent-offset 2)

(eval-after-load 'web-mode
  '(add-hook 'js-mode-hook #'add-node-modules-path))

(provide 'init-javascript)
;;; init-javascript.el ends here
