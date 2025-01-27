;;; init.el --- Simplified Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Streamlined core Emacs configuration

;;; Code:

;; Load custom modules
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules"))

;; Core package configuration
(use-package emacs
  :custom
  ;; Essential settings only
  (package-install-upgrade-built-in t)
  (enable-recursive-minibuffers t)
  (make-backup-files nil)
  (auto-save-default t)
  (create-lockfiles nil)
  
  ;; UI preferences
  (scroll-conservatively 101)
  (scroll-margin 0)
  (display-line-numbers-width 3)
  
  ;; Editor behavior
  (indent-tabs-mode nil)
  (tab-width 4)
  (fill-column 80)
  (require-final-newline t)
  
  :config
  ;; Enable useful global modes
  (global-auto-revert-mode 1)
  (delete-selection-mode 1)
  (save-place-mode 1)
  (global-display-line-numbers-mode 1)
  
  ;; Custom file handling
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file nil :nomessage)))

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
  (set-face-attribute 'default nil :family mono-font :height 160)
  (set-face-attribute 'fixed-pitch nil :family mono-font)
  (set-face-attribute 'variable-pitch nil :family sans-font))

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

(provide 'init)
;;; init.el ends here
