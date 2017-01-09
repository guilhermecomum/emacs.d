;;; Commentary:
;; magit configuration

;;; Code:

(require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)


(provide 'init-magit)
;;; init-magit.el ends here
