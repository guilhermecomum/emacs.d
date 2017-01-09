;;; Commentary:
;;; Melpa configurations

;;; Code:

;; Setup melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa-stable" . "http://melpa.org/packages/")
   t)
  (package-initialize))


(provide 'init-melpa)
;;; init-melpa.el ends here
