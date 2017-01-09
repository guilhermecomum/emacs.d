;;; Commentary:
;; Personal functions

;;; Code:

;; open files by M-x
(defun gtd ()
  "Gtd file."
  (interactive)
  (find-file "~/org/gtd.org")
  )

(defun todo ()
  "Todo file."
  (interactive)
  (find-file "~/org/todo.org")
  )

(defun init ()
  "Shortcut to reach init.el ."
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )


(provide 'init-functions)
;;; init-functions.el ends here
