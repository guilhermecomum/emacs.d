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

  ;; Improve theme loading; from reddit (https://www.reddit.com/r/emacs/comments/4mzynd/what_emacs_theme_are_you_currently_using/d43c5cw)
  (defadvice load-theme (before clear-previous-themes activate)
    "Clear existing theme settings instead of layering them"
    (mapc #'disable-theme custom-enabled-themes))

  (use-package doom-themes
    :config
    (doom-themes-neotree-config)
    (set-face-attribute 'default nil :font "Noto Sans Mono 14")
    (set-face-attribute 'region nil :background "#000" :foreground "#ffffff")

    :init
    (setq selected-theme "doom-material")
    (load-theme (intern selected-theme) t)

    (defun gg-switch-theme()
      (interactive)
      (let* ((theme selected-theme)
             (change (if (string= theme "doom-material") "doom-solarized-light" "doom-material")))
        (load-theme (intern change) t)
        (setq selected-theme change)
        (message "Theme switched from %s to %s" theme change)))
    (global-set-key (kbd "<f9>") 'gg-switch-theme))

  (defun text-scale-twice ()(interactive)(progn(text-scale-adjust 0)(text-scale-decrease 2)))
  (add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'text-scale-twice)))

  ;; No bars. Doing this first to avoid showing/hidding delay on start
  (menu-bar-mode 0)
  (tool-bar-mode 0)

  ;; Misc
  (column-number-mode)              ;; Basic config for columns
  (setq ring-bell-function 'ignore) ;; No freaking bell
  (setq inhibit-splash-screen t)    ;; No splash screen
  (setq inhibit-startup-screen t))


(defun gg/ui/modeline ()
  "Configuration for the modeline."
  (use-package doom-modeline
    :config
    (setq doom-modeline-height 35)
    (set-face-background 'doom-modeline-bar (face-background 'mode-line))
    (setq doom-modeline-bar-width 1)
    (doom-modeline-mode 1)))

(defun gg/ui ()
  "Entry point of UI configuration."
  (gg/ui/general)
  (gg/ui/modeline))

(provide 'gg-ui)

;;; gg-ui.el ends here
