;;; early-init.el --- Early init file -*- lexical-binding: t -*-
;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT
;; Author: System Crafters Community
;;; Commentary:
;; Enhanced early-init.el with straight.el integration
;;; Code:

;; Performance optimization: Increase GC threshold during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Reset GC after startup
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Faster startup optimizations
(setq load-prefer-newer t
      inhibit-compacting-font-caches t
      auto-mode-case-fold nil
      frame-inhibit-implied-resize t)

;; File name handler optimization
(unless (daemonp)
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq file-name-handler-alist old-value)))))

;; UI preferences (set via default-frame-alist for early application)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (push '(menu-bar-lines . 0) default-frame-alist) ;; Optional: also disable menu bar

;; Disable startup screen
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; Native-comp settings (simplified for Emacs 30)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent))


(provide 'early-init)
;;; early-init.el ends here
