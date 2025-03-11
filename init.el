;;; init.el --- Simplified Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Streamlined core Emacs configuration

;;; Code:

;; Load custom modules


;; Core package configuration
(use-package emacs
  :custom
  ;; Essential settings only
  (package-install-upgrade-built-in t)
  (enable-recursive-minibuffers t)
  (make-backup-files nil)
  (auto-save-default nil)
  (read-process-output-max (* 4 1024 1024))
  (process-adaptive-read-buffering nil)
  ;;  (create-lockfiles nil)

  ;; UI preferences
  ;; (scroll-conservatively 101)
  ;; (scroll-margin 0)
  ;; (display-line-numbers-width 3)

  ;; Editor behavior
  (indent-tabs-mode nil)
  (tab-width 4)
  ;; (fill-column 80)
  ;; (require-final-newline t)
  ;; Make it easy to cycle through previous items in the mark ring
  (set-mark-command-repeat-pop t)

  :config
  ;; Enable useful global modes
  ;; Enable diff highlighting in margins

  (global-auto-revert-mode 1)
  (delete-selection-mode 1)
  (repeat-mode 1)
  (savehist-mode 1)
  (auto-save-visited-mode 1)
  (setopt use-short-answers t)
  (save-place-mode 1)
  (blink-cursor-mode 0)
  (global-hl-line-mode)
  (recentf-mode t)
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

  :hook ((prog-mode . display-line-numbers-mode)
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




(provide 'init)
;;; init.el ends here
