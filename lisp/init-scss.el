;;; Commentary:
;; Scss file configurations

;;; Code:
(require 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)

(require 'flycheck-stylelint)

(provide 'init-scss)
;;; init-scss.el ends here
