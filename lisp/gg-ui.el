;;; init.el --- Configuration Themes for Emacs
;;
;; Author: Guilherme Guerra <guilherme.ga@gmail.com>
;;
;; Copyright (C) 2012-2020  Guilherme Guerra
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Entry point for my Emacs setup.  This module's main goal is to load
;; other files that do more specific setup work.
;;
;;; Code:

;; UI

(defun gg/ui/general ()
  "Setup theme."

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (use-package all-the-icons)
  (use-package doom-themes
    :config
    (doom-themes-neotree-config))

  (use-package dracula-theme
    :config
    (set-face-attribute 'default nil :font "Noto Sans Mono SemiCondensed SemiBold 13")
    :init
    (load-theme 'dracula t))

  (use-package nyan-mode
    :init
    (nyan-mode t))

  (use-package dimmer
    :init
    (dimmer-mode t)
    :config
    (setq dimmer-fraction 0.5))

  ;; No bars. Doing this first to avoid showing/hidding delay on start
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)

  ;; Misc
  (column-number-mode)              ;; Basic config for columns
  (setq ring-bell-function 'ignore) ;; No freaking bell
  (setq inhibit-splash-screen t)    ;; No splash screen
  (setq inhibit-startup-screen t))

(defun gg/ui/fringe ()
  "Configure the Fringe area."
  ;; Custom bitmap to be shown in the fringe area for lines with any
  ;; sort of linting issues
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000)))

  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info)

  ;;Get rid of the background color in the Fringe area
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

(defun gg/ui/modeline ()
  "Configuration for the modeline."
  (use-package doom-modeline
    :config
    (setq doom-modeline-height 35)
    (setq doom-modeline-bar-width 1)
    (doom-modeline-mode 1)))

(defun gg/ui/org-mode ()
  (lambda () (progn
               (setq left-margin-width 2)
               (setq right-margin-width 2)
               (set-window-buffer nil (current-buffer))))
  (setq org-startup-indented t
        org-bullets-bullet-list '("› ")
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 0))

(defun gg/ui ()
  "Entry point of UI configuration."
  (gg/ui/general)
  (gg/ui/fringe)
  (gg/ui/modeline)
  (gg/ui/org-mode))

(provide 'gg-ui)

;;; gg-ui.el ends here
