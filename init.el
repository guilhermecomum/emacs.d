;;; init.el --- My Emacs Setup
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

;; Add `melpa` to `package-archives`.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; load path so that configs from lisp folder can be required
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load all the fun modules
(require 'gg-ui)
(gg/ui)
(require 'gg-general)
(gg/general)
(require 'gg-edit)
(gg/edit)
(require 'gg-modes)
(gg/modes)
(require 'gg-org)
(gg/org)
(require 'gg-tools)
(gg/tools)
;;; init.el ends here
