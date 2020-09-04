;;; init.el --- Configuration Tools for Emacs
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
;; Configuration for different tools, like elquery, hackernews, restclient.
;;
;;; Code:

(defun gg/tools/lpass ()
  "Configure Lastpass."
  (use-package lastpass
    :config
    ;; Set lastpass user
    (setq lastpass-user "guilherme.ga@gmail.com")
    (setq lastpass-multifactor-use-passcode t)

    ;; Enable lastpass custom auth-source
    (lastpass-auth-source-enable)
    (defun start-lastpass ()
      "Start lastpass on startup only if user want."
       (if (string-match (regexp-quote "Not") (lastpass-status)) (lastpass-login)))))

(defun gg/tools/todoist ()
  "Configure todoist."
  (start-lastpass)
  (use-package todoist)
  (add-hook 'lastpass-logged-in-hook
            (lambda ()
              (setq todoist-token (lastpass-getpass "todoist-api")))))

(defun gg/tools/slack ()
  "Setup slack."
  (start-lastpass)
  (add-to-list 'load-path (expand-file-name "site-lisp/helm-slack" user-emacs-directory))
  (require 'helm-slack)
  (use-package slack
    :init
    (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
    (setq slack-prefer-current-team t))
  (slack-register-team
   :name "ilera-slack"
   :default t
   :token (lastpass-getpass "slack-ilegra-api")
   :subscribed-channels '(tr-onvio-accounting)
   :full-and-display-names t)
  (slack-start)
  (global-set-key (kbd "C-c k") 'helm-slack)
  (global-set-key (kbd "C-c C-k u") 'helm-slack-unreads)
  (global-set-key (kbd "C-c C-k m") 'slack-message-embed-mention)
  (global-set-key (kbd "C-c C-k t") 'slack-thread-show-or-create)
  (global-set-key (kbd "C-c C-k e") 'slack-message-edit)
  (global-set-key (kbd "C-c C-k f") 'slack-file-upload)

  (use-package alert
    :commands (alert)
    :init
    (setq alert-default-style 'notifications)))

(defun gg/tools/google-translate ()
  "Setup Google Translate."
  (use-package google-translate)
  (global-set-key "\C-ct" 'google-translate-at-point)
  (setq google-translate-default-source-language "English")
  (setq google-translate-default-target-language "pt-br"))


(defun gg/tools ()
  (use-package elquery)
  (use-package hackernews)
  (use-package restclient)
  (use-package pocket-reader)
  ;; inhibit startup message
  (setq inhibit-startup-message t)

  (use-package fortune-cookie
    :config
    (fortune-cookie-mode))

  (gg/tools/lpass)
  (gg/tools/google-translate))

(provide 'gg-tools)
