;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Andrey Listopadov
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles.git

;;; Commentary:
;; Emacs 30+ early initialization configuration.

;;; Code:

(let ((original-gc-cons-threshold gc-cons-threshold))
  (setq
   gc-cons-threshold most-positive-fixnum
   inhibit-compacting-font-caches t
   message-log-max 16384
   package-enable-at-startup nil
   load-prefer-newer noninteractive)
  (add-hook
   'emacs-startup-hook
   (lambda nil
     (setq gc-cons-threshold original-gc-cons-threshold))))

(setq native-comp-async-report-warnings-errors nil)

(when (string-equal system-type "android")
  ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
    (setq exec-path (append exec-path (list termuxpath)))))


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
(package-initialize)
(setenv "LSP_USE_PLISTS" "true")
