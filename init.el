;;; init.el --- Simplified Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Streamlined core Emacs configuration

;;; Code:

;; Package.el setup - ADD THIS SECTION
(require 'package)
(setq package-archives
	  '(("gnu" . "https://elpa.gnu.org/packages/")
		("melpa" . "https://melpa.org/packages/")
		("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ; Auto-install packages
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
;; Load custom modules
(setq read-process-output-max (* 4 1024 1024))
;; Core package configuration
;; Add this temporarily to your init.el before the module requires
;; (setq debug-on-error t)
(use-package emacs
  :ensure nil  ; Keep this as nil since emacs is built-in
  :custom
  (column-number-mode t)
  ;; Essential settings only
  (package-install-upgrade-built-in t)
  (enable-recursive-minibuffers t)
  (make-backup-files nil)
  (create-lockfiles nil)
  (delete-selection-mode 1)
  (auto-save-default nil)
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (process-adaptive-read-buffering nil)
  (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling for pixel precision.
  (history-length 25)                             ;; Set the length of the command history.
  (split-width-threshold 300)                     ;; Prevent automatic window splitting if the window width exceeds 300 pixels.
  (switch-to-buffer-obey-display-actions t)       ;; Make buffer switching respect display actions.
  (tab-always-indent 'complete)                   ;; Make the TAB key complete text instead of just indenting.
  (tab-width 4)                                   ;; Set the tab width to 4 spaces.
  (treesit-font-lock-level 4)                     ;; Use advanced font locking for Treesit mode.
  (truncate-lines t)                              ;; Enable line truncation to avoid wrapping long lines.
  (use-dialog-box nil)                            ;; Disable dialog boxes in favor of minibuffer prompts.
  (use-short-answers t)                           ;; Use short answers in prompts for quicker responses (y instead of yes)
  (warning-minimum-level :emergency)

  ;; Make it easy to cycle through previous items in the mark ring
  (set-mark-command-repeat-pop t)

  :config
  ;; (global-hl-line-mode 1)      ;; Enable highlight of the current line
  (global-auto-revert-mode 1)  ;; Enable global auto-revert mode to keep buffers up to date with their corresponding files.
  (indent-tabs-mode -1)        ;; Disable the use of tabs for indentation (use spaces instead).
  (recentf-mode 1)             ;; Enable tracking of recently opened files.
  (savehist-mode 1)            ;; Enable saving of command history.
  (save-place-mode 1)          ;; Enable saving the place in files for easier return.
  (winner-mode 1)              ;; Enable winner mode to easily undo window configuration changes.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.
  (setq ring-bell-function 'ignore)
  (modify-coding-system-alist 'file "" 'utf-8)

  (repeat-mode 1)
  (auto-save-visited-mode 1)
  (blink-cursor-mode 0)
  (let ((mono-font "FiraCode Nerd Font")
		(sans-font "DejaVu Sans"))
	;; Significantly larger font size for 4K screens
	;; Values between 180-240 are often good for 4K displays
	(set-face-attribute 'default nil :family mono-font :weight 'medium :height 170)
	(set-face-attribute 'fixed-pitch nil :family mono-font :height 1.0)
	(set-face-attribute 'variable-pitch nil :family sans-font :height 1.1))

  ;; Improve readability with increased line spacing
  (setq-default display-line-numbers-grow-only t
				display-line-numbers-type t
				display-line-numbers-width 2
				recentf-max-saved-items 50)
  (defun pulse-line (&rest _)
	"Pulse the current line."
	(pulse-momentary-highlight-one-line (point)))

  (dolist (command '(scroll-up-command
					 scroll-down-command
					 recenter-top-bottom
					 other-window))
	(advice-add command :after #'pulse-line))

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
	(load custom-file nil :nomessage))

  :hook ((prog-mode . subword-mode)
		 (prog-mode . font-lock-fontify-buffer)
		 ( after-init . (lambda ()
						  (message "Emacs has fully loaded. This code runs after startup.")

						  ;; Insert a welcome message in the *scratch* buffer displaying loading time and activated packages.
						  (with-current-buffer (get-buffer-create "*scratch*")
							(insert (format
									 ";;    Welcome to Emacs!
;;
;;    Loading time : %s
;;    Packages     : %s
"
									 (emacs-init-time)
									 (number-to-string (length package-activated-list)))))))
		 (prog-mode . display-line-numbers-mode)
		 (conf-mode . display-line-numbers-mode)
		 (org-mode . (lambda () (display-line-numbers-mode -1)))
		 (before-save . delete-trailing-whitespace)))

;; CHANGE: Remove :ensure nil from packages you want to install via package.el
(use-package diff-hl
  ;; :ensure nil  ; Remove this line to install from MELPA
  :hook
  ((after-init . global-diff-hl-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package undo-fu-session
  ;; :ensure nil  ; Remove this line to install from MELPA
  :config
  ;; Store undo history files in your Emacs directory
  (setq undo-fu-session-directory (expand-file-name "undo-fu-session" user-emacs-directory))

  ;; Create the directory if it doesn't exist
  (unless (file-exists-p undo-fu-session-directory)
	(make-directory undo-fu-session-directory t))

  ;; Configure session settings
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-global-mode t)

  ;; Enable globally
  (global-undo-fu-session-mode))

;; testing
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(global-eldoc-mode 1) ; eglot uses this too

(electric-pair-mode 1) ; auto parens in pairs

(add-hook 'before-save-hook 'whitespace-cleanup) ; auto strip whitespace

(delete-selection-mode) ; allow highlight and backspace over text like a normal editor

(setq-default
 frame-resize-pixelwise t ; support better certain window managers like ratpoison

 ;; better security
 gnutls-verify-error t
 gnutls-min-prime-bits 2048

 ;; dont expire a passphrase
 password-cache-expiry nil

 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 require-final-newline t
 ediff-window-setup-function 'ediff-setup-windows-plain

 tramp-default-method "ssh"
 tramp-copy-size-limit nil

 vc-follow-symlinks t ; open symlinks, don't ask confusing questions

 ring-bell-function 'ignore ; be quiet

 browse-url-browser-function 'eww-browse-url ; use a text browser --great for clicking documentation links
 )
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(setq jit-lock-defer-time 0.05)
;;; testing stop
(use-package vundo
  ;; :ensure nil  ; Remove this line to install from MELPA
  :config
  (global-set-key (kbd "C-x u") 'vundo))

(use-package editorconfig
  :diminish ""
  ;; :ensure nil  ; Remove this line to install from MELPA
  :config
  (editorconfig-mode 1))

(use-package diminish
  ;; :ensure nil  ; Remove this line to install from MELPA
  :config
  (diminish 'line-number-mode))

(use-package dape
  ;; :ensure nil  ; Remove this line to install from MELPA
  :defer t)

;; Load completion system first - other modules depend on it
(require 'my-completion)
(require 'my-markdown)
;; Load core utilities and themes early
(require 'my-themes)
(require 'my-which-key)
(require 'my-utils)

;; Load editing enhancements
(require 'my-editing-utils)
(require 'my-treesit)

(require 'my-meow)

;; or

;; Load programming support (depends on completion)
(require 'my-prog)
;; (require 'my-lsp)
;; Load language-specific modes
(require 'my-elisp)
(require 'my-go)
(require 'my-lua)
(require 'my-nix)
(require 'my-web)

;; Load version control (can be heavy)
(require 'my-magit)

;; Load applications
(require 'my-dired)
(require 'my-shell)
(require 'my-org)

;; Load modal editing last (affects all modes)

;; Load custom keybindings last
(require 'my-keybinds)

;; Commented out modules
(require 'my-llm)
;; (require 'my-spelling)
;; (require 'my-evil)
;; (require 'my-lsp)

(provide 'init)
;;; init.el ends here
