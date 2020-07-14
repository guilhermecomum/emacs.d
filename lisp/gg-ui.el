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

;; Theme

(defun gg/ui/general ()
  "Setup theme."

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
    (setq dimmer-fraction 0.5)))

(defun gg/ui ()
  "Entry point of UI configuration."
  (gg/ui/general))

(provide 'gg-ui)
;; gg-theme.el ends here
