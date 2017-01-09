;;; Commentary:
;; configuration for c++ files

;;; Code:

(require 'flycheck)
;; support c++11 in flycheck
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++11")))

;; Add C++ command for C11 and set it default in C++ file.
(quickrun-add-command "c++/c11"
                      '((:command . "g++")
                        (:exec    . ("%c -std=c++11 %o -o %e %s"
                                     "%e %a"))
                        (:remove  . ("%e")))
                      :default "c++")


(provide 'init-cpp)
;;; init-cpp.el ends here
