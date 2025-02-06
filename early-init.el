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
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
;; Reset after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)))) ;; Adjust this value as needed

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Faster startup
(setq load-prefer-newer t)
(setq inhibit-compacting-font-caches t)

;; File name handler optimization
(unless (daemonp)
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq file-name-handler-alist old-value)))))

;; Faster startup tweaks
(setq auto-mode-case-fold nil)
(setq frame-inhibit-implied-resize t)
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
(use-package straight
  :custom
  ;; add project and flymake to the pseudo-packages variable so straight.el doesn't download a separate version than what eglot downloads.
  (straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake xref))
  (straight-use-package-by-default t))
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

  ;; Pperformance settings
  (setq frame-inhibit-implied-resize t)
  ;; Disable startup screen
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        initial-scratch-message nil)
  ;; Startup optimizations
  (setq package-quickstart nil)
  (when (featurep 'native-compile)
    (setq native-comp-jit-compilation t
          package-native-compile t)
    (setq native-comp-async-report-warnings-errors 'silent))
  :config
  ;; Any additional configurations that need to run after initialization
  )

(provide 'early-init)
;;; early-init.el ends here
