;;; Commentary:
;; Javascript file configurations

;;; Code:

(require 'company)
(require 'quickrun)
(require 'web-mode)
(require 'flycheck)
(require 'flycheck-flow)

;; flow auto complete
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-flow))

;; quickrun
(quickrun-add-command "babel"
                      '((:command . "babel-node")
                        (:exec    . "%c %s")))

(quickrun-set-default "javascript" "babel")

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

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-attr-indent-offset 2)


(provide 'init-javascript)
;;; init-javascript.el ends here
