;;; init.el --- Simplified Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Streamlined core Emacs configuration

;;; Code:

;; Load custom modules

(setq read-process-output-max (* 4 1024 1024))
;; Core package configuration
(use-package emacs
  :ensure nil
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
  (global-hl-line-mode 1)      ;; Enable highlight of the current line
  (global-auto-revert-mode 1)  ;; Enable global auto-revert mode to keep buffers up to date with their corresponding files.
  (indent-tabs-mode -1)        ;; Disable the use of tabs for indentation (use spaces instead).
  (recentf-mode 1)             ;; Enable tracking of recently opened files.
  (savehist-mode 1)            ;; Enable saving of command history.
  (save-place-mode 1)          ;; Enable saving the place in files for easier return.
  (winner-mode 1)              ;; Enable winner mode to easily undo window configuration changes.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.

  (modify-coding-system-alist 'file "" 'utf-8)

  (repeat-mode 1)
  (auto-save-visited-mode 1)
  (blink-cursor-mode 0)

  (let ((mono-font "FiraCode Nerd Font")
        (sans-font "DejaVu Sans"))
    (set-face-attribute 'default nil :family mono-font :weight 'medium :height 160)
    (set-face-attribute 'fixed-pitch nil :family mono-font)
    (set-face-attribute 'variable-pitch nil :family sans-font))
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

  :hook (( after-init . (lambda ()
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
         (conf-mode .display-line-numbers-mode)
         (org-mode . (lambda () (display-line-numbers-mode -1)))
         (before-save . delete-trailing-whitespace)))


(use-package diff-hl
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode)
  :custom
  (undo-fu-session-directory "~/.emacs.d/undo-fu-session")
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :config
  (global-set-key (kbd "C-x u") 'vundo))
;; (use-package elisp-demos
;;   :straight t
;;   :after helpful
;;   :config
;;   (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; (use-package helpful
;;   :straight t
;;   :bind
;;   (([remap describe-command] . helpful-command)
;;    ([remap describe-function] . helpful-callable)
;;    ([remap describe-key] . helpful-key)
;;    ([remap describe-symbol] . helpful-symbol)
;;    ([remap describe-variable] . helpful-variable)
;;    ("C-h F" . helpful-function)
;;    :map helpful-mode-map
;;    ([remap revert-buffer] . helpful-update))
;;   :init
;;   (keymap-global-set "C-h K" #'describe-keymap))

;; (use-package breadcrumb
;;   :straight t
;;   :config
;;   (breadcrumb-mode))



;; (defun my--server ()
;;   (unless (server-running-p)
;;     (server-start)))

;; (use-package server
;;   :ensure nil   ; Good - server is built into Emacs
;;   :hook
;;   (after-init . my--server))



;; Improve terminal experience

;; Environment management


;; Terminal popup
;; (use-package popper
;;   :bind (("C-`"   . popper-toggle)
;;          ("M-`"   . popper-cycle)
;;          ("C-M-`" . popper-toggle-type))
;;   :init
;;   (setq-default popper-reference-buffers
;;                 '("\\*Messages\\*"
;;                   "Output\\*$"
;;                   "\\*Async Shell Command\\*"
;;                   help-mode
;;                   compilation-mode
;;                   "*eat*"
;;                   "^\\*eldoc.*\\*.*$" eldoc-mode
;;                   ))
;;   (popper-mode +1)
;;   (popper-echo-mode +
(require 'my-themes)
(require 'my-dired)
(require 'my-editing-utils)
(require 'my-markdown)
(require 'my-treesit)
(require 'my-completion)
(require 'my-elisp)
(require 'my-go)
(require 'my-llm)
(require 'my-lua)
(require 'my-magit)
(require 'my-nix)
(require 'my-shell)
;; (require 'my-spelling)
(require 'my-utils)

(require 'my-web)
(require 'my-which-key)
(require 'my-prog)
(require 'my-org)

(require 'my-keybinds)


;; Evil mode configuration
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; Set up leader key before evil loads
  (setq evil-want-leader t)
  (setq evil-leader/leader "SPC")
  (setq evil-default-state 'emacs) ;; Use Emacs state as default
  :config
  ;; Make evil active in prog-mode and all modes derived from it
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-mode 1))
;; Evil Collection for better integration with many Emacs packages
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Evil commentary for commenting code (gcc, gc motion)
(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

;; Evil surround for working with pairs (like parentheses, quotes)
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; For Vim-like multiple cursors (optional)
(use-package evil-mc
  :ensure t
  :after evil
  :config
  (global-evil-mc-mode 1))

;; For Vim-like undo behavior (optional)
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC")

  ;; Define your leader key bindings
  (evil-leader/set-key
	"f" '(:ignore t :which-key "files")
	"ff" 'find-file
	"fs" 'save-buffer

	"b" '(:ignore t :which-key "buffers")
	"bb" 'switch-to-buffer
	"bk" 'kill-buffer

	"w" '(:ignore t :which-key "windows")
	"wl" 'windmove-right
	"wh" 'windmove-left
	"wk" 'windmove-up
	"wj" 'windmove-down
	"wd" 'delete-window

	"g" '(:ignore t :which-key "git")
	"gs" 'magit-status

	";" 'comment-dwim))
(global-set-key (kbd "C-z") 'evil-toggle-state)
(provide 'init)
;;; init.el ends here
