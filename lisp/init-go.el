;;; Commentary:
;; configuration for go lang files

;;; Code:

(require 'quickrun)
(quickrun-add-command "go"
                      '((:command . "go")
                        (:exec    .  "go run %s")))


(provide 'init-go)
;;; init-go.el ends here
