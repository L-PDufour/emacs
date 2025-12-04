;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Code:

;;; Performance: Garbage Collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(setq debug-on-error t)
;; Reset to reasonable values after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)   ; 16MB (reasonable default)
                  gc-cons-percentage 0.1)))

(setq igc-step-multiplier 2)               ; Adjust IGC aggressiveness
(setq igc-cons-threshold (* 32 1024 1024))
;; GC when focus is lost
(add-hook 'focus-out-hook #'garbage-collect)

;;; Directory Structure
(setq user-emacs-directory (expand-file-name "var/" "~/.emacs.d/"))

(unless (file-directory-p user-emacs-directory)
  (make-directory user-emacs-directory t))

;;; Package Management - Completely disable package.el for Nix
(setq package-enable-at-startup nil)
(setq package-archives nil)

;;; Native Compilation
(when (featurep 'native-compile)
  (setq native-comp-deferred-compilation t
        native-comp-jit-compilation t
        native-comp-async-report-warnings-errors 'silent
        native-comp-warning-on-missing-source nil)
  
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (expand-file-name "eln-cache/" user-emacs-directory))))

;;; UI Elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;;; Frame Settings
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;;; Startup Optimizations
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

;;; File Handling
(setq auto-mode-case-fold nil
      read-process-output-max (* 1024 1024))

;;; Misc Performance
(setq idle-update-delay 1.0
      inhibit-compacting-font-caches t
      ffap-machine-p-known 'reject)

;;; Bidirectional Text
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;;; Encoding
(set-language-environment "UTF-8")
(setq default-input-method nil)

;;; Custom File
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/"))

(provide 'early-init)
;;; early-init.el ends here
