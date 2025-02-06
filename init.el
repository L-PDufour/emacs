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
  (global-auto-revert-mode 1)
  (delete-selection-mode 1)
  (repeat-mode 1)
  (savehist-mode 1)
  (auto-save-visited-mode 1)
  (global-visual-line-mode 1)
  (setopt use-short-answers t)
  (save-place-mode 1)
  (global-display-line-numbers-mode 1)
  (recentf-mode t)
  (setq-default recentf-max-saved-items 50)

  ;; Custom file handling
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file nil :nomessage))

  :hook (( prog-mode . display-line-numbers-mode)
         (before-save . delete-trailing-whitespace)))

;; Essential packages
(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :hook (markdown-mode . visual-line-mode))

(use-package magit)

(use-package avy
  :bind
  ("C-;" . avy-goto-char-2)
  ("C-c C-j" . avy-resume))

;; Font configuration
(let ((mono-font "FiraCode Nerd Font")
      (sans-font "DejaVu Sans"))
  (set-face-attribute 'default nil :family mono-font :weight 'medium :height 160)
  (set-face-attribute 'fixed-pitch nil :family mono-font)
  (set-face-attribute 'variable-pitch nil :family sans-font))



(straight-use-package 'pdf-tools)

;; Install org-pdf-tools
(pdf-tools-install)
;; Load modular configurations
(require 'my-themes)
(require 'my-prog)
(require 'my-org)
(require 'my-dired)
(require 'my-term)
(require 'my-ui)
(require 'my-lisp)
(require 'my-completion)
(require 'my-keybinds)

;; Spell checking
(use-package ispell
  :config
  (setq ispell-program-name "/run/current-system/sw/bin/aspell"
        ispell-dictionary "en")
  (dolist (hook '(text-mode-hook))
    (add-hook hook #'flyspell-mode)))

(defun my--server ()
  (unless (server-running-p)
    (server-start)))

(use-package server
  :ensure nil   ; Good - server is built into Emacs
  :hook
  (after-init . my--server))
(use-package tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(use-package gptel
  :straight t
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'test
        gptel-backend(gptel-make-openai "llama-cpp"
                       :stream t
                       :protocol "http"
                       :host "localhost:8080"
                       :models '(test))))


  (provide 'init)
;;; init.el ends here
