;;; Commentary:
;; Personal key bindings

;;; Code:

;; Replacement for backspace, because backspace is very far
(global-set-key (kbd "C-j") 'backward-delete-char-untabify)

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

;;; change window
(global-set-key [(C-tab)] 'other-window)

;;; buffer
(when (eq system-type 'darwin)
  (global-set-key (kbd "M-]") 'next-buffer)
  (global-set-key (kbd "M-[") 'previous-buffer))

(global-set-key [s-tab] 'next-buffer)
(global-set-key [S-s-iso-lefttab] 'previous-buffer)

;;; comments
(global-set-key [(ctrl c) (c)] 'comment-region)
(global-set-key [(ctrl c) (d)] 'uncomment-region)

;;; Navegation
(global-set-key (kbd "M-g") 'goto-line)

;;; Sort
(global-set-key (kbd "C-c s") 'sort-lines)

;;; highlight indentation column
(global-set-key (kbd "M-1") 'highlight-indentation-current-column-mode)

(provide 'init-keys)
;;; init-keys.el ends here
