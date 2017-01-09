;;; Commentary:
;; company mode configuration

;;; Code:

(require 'company)

(add-hook 'after-init-hook 'global-company-mode)

;; bigger popup window
(setq company-tooltip-limit 20)

;; decrease delay before autocompletion popup shows
(setq company-idle-delay .3)


(provide 'init-company)
;;; init-company.el ends here
