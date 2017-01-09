;;; Commentary:
;; Personal key bindings

;;; Code:

;; Replacement for backspace, because backspace is very far
(global-set-key (kbd "C-j") 'backward-delete-char-untabify)

(global-set-key (kbd "RET") 'newline-and-indent)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; origami
(require 'origami)
(global-origami-mode t)
(global-set-key (kbd "C-c C-l") 'origami-toggle-node)

;; ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; quickrun
(require 'quickrun)
(define-key global-map (kbd "C-c C-k") 'quickrun)
(define-key global-map (kbd "C-c C-q") 'quickrun-shell)


(provide 'init-keys)
;;; init-keys.el ends here
