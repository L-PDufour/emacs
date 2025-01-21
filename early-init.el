;;; early-init.el --- Early init file -*- lexical-binding: t -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT
;; Author: System Crafters Community

;;; Commentary:
;; Enhanced early-init.el with straight.el integration
;; and organized settings using use-package

;;; Code:

;; Performance optimization: Increase GC threshold during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Reset gc threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20)))) ; 20MB

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Core Emacs settings organized with use-package
(use-package emacs
  :init
  ;; UI preferences
  (setq visible-bell t
        frame-inhibit-implied-resize t)
  
  ;; Disable UI elements early
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  
  ;; Disable UI modes
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  
  ;; Performance settings
  (setq frame-inhibit-implied-resize t)
  
  ;; Startup optimizations
  (setq package-quickstart nil)
  
  :config
  ;; Any additional configurations that need to run after initialization
  )

(provide 'early-init)
;;; early-init.el ends here
