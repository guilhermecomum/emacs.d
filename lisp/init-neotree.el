;;; Commentary:
;; Configuration for neotree (File explorer)

;;; Code:

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(provide 'init-neotree)
;;; init-neotree.el ends here


