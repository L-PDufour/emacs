;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-
;;; Commentary:
;; Tangled from config.org — do not edit by hand.
;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; IGC tuning (ignored on non-IGC builds)

(add-hook 'focus-out-hook #'garbage-collect)

(setq user-emacs-directory (expand-file-name "var/" "~/.emacs.d/"))
(unless (file-directory-p user-emacs-directory)
  (make-directory user-emacs-directory t))

(setq package-enable-at-startup nil)
(setq package-archives nil)

(when (featurep 'native-compile)
  (setq native-comp-deferred-compilation t
        native-comp-jit-compilation t
        native-comp-async-report-warnings-errors 'silent
        native-comp-warning-on-missing-source nil)
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (expand-file-name "eln-cache/" user-emacs-directory))))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

(setq auto-mode-case-fold nil
      read-process-output-max (* 1024 1024 4))

(setq idle-update-delay 1.0
      inhibit-compacting-font-caches t
      ffap-machine-p-known 'reject)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(set-language-environment "UTF-8")
(setq default-input-method nil)

(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/"))

(provide 'early-init)
;;; early-init.el ends here
