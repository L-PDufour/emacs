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
(setq igc-step-multiplier 2)
(setq igc-cons-threshold (* 32 1024 1024))

(add-hook 'focus-out-hook #'garbage-collect)

(defvar my--file-name-handler-alist
  (default-toplevel-value 'file-name-handler-alist))

(unless noninteractive
  (set-default-toplevel-value
   'file-name-handler-alist
   (if (locate-file-internal "calc-loaddefs.el" load-path)
       nil
     (list (rassq 'jka-compr-handler my--file-name-handler-alist))))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (set-default-toplevel-value
               'file-name-handler-alist
               (delete-dups (append file-name-handler-alist
                                    my--file-name-handler-alist))))
            101))

(unless noninteractive
  ;; Hide the mode line until init installs the real one.
  (put 'mode-line-format 'initial-value
       (default-toplevel-value 'mode-line-format))
  (setq-default mode-line-format nil)

  (setq-default inhibit-redisplay t
                inhibit-message t)

  (defun my--reset-inhibited-vars ()
    "Restore redisplay, messages, and the mode line after startup."
    (setq-default inhibit-redisplay nil
                  inhibit-message nil)
    (unless (default-toplevel-value 'mode-line-format)
      (setq-default mode-line-format
                    (get 'mode-line-format 'initial-value)))
    (remove-hook 'post-command-hook #'my--reset-inhibited-vars))

  (add-hook 'post-command-hook #'my--reset-inhibited-vars -100)
  (add-hook 'emacs-startup-hook #'my--reset-inhibited-vars 101))

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

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

;; Never pop GUI dialogs — keep everything in the minibuffer.
(setq use-file-dialog nil
      use-dialog-box nil)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

(setq auto-mode-case-fold nil)
(winner-mode +1)

(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
  		   (equal (selected-window) (next-window)))
  	  (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") #'toggle-delete-other-windows)

(setq idle-update-delay 1.0
      inhibit-compacting-font-caches t
      ffap-machine-p-known 'reject)
(setq jit-lock-defer-time 0.05)
;; Skip expensive rendering mid-scroll
(setq fast-but-imprecise-scrolling t)
(set-language-environment "UTF-8")
(setq default-input-method nil)

;; From minimal-emacs.d
(setq load-prefer-newer t)           ; never load stale byte-code
(setq ad-redefinition-action 'accept) ; silence advice redefinition noise
;; PGTK (Wayland/Sway): don't stall waiting for GTK events
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/"))

(provide 'early-init)
;;; early-init.el ends here
