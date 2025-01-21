;;; init.el --- Crafted Emacs Base Example -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Base example init.el (extended from the info file).
;;; Basic example of loading a module.

;;; Code:

;;; Initial phase
;;; init.el --- Core initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Core Emacs initialization and configuration settings

;;; Code:

;; Core Emacs settings using use-package
(use-package emacs
  :custom
  ;; Previous settings
  (package-install-upgrade-built-in t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (auto-save-default nil)
  (create-locfiles nil)
  (make-backup-files nil)
  (load-prefer-newer t)
  
  ;; Buffer behavior
  (global-auto-revert-non-file-buffers t)
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (eshell-scroll-to-bottom-on-input 'this)
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  
  ;; Completion settings
  (tab-always-indent 'complete)
  (completion-cycle-threshold 3)
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completions-detailed t)
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  
  ;; Window and scroll behavior
  (fast-but-imprecise-scrolling t)
  (scroll-conservatively 101)
  (scroll-margin 0)
  (scroll-preserve-screen-position t)
  (Man-notify-method 'aggressive)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  
  ;; Kill ring and editing
  (kill-do-not-save-duplicates t)
  (bookmark-save-flag 1)
  
  :init
  ;; Minibuffer settings
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  
  ;; Various mode enables
  (global-auto-revert-mode 1)
  (delete-selection-mode 1)
  (global-so-long-mode 1)
  (savehist-mode 1)
  (pixel-scroll-precision-mode 1)  ; Smooth scrolling
  (winner-mode 1)
  (repeat-mode 1)                  ; Make commands repeatable 
  
  ;; Tab and bidirectional settings
  (setq-default indent-tabs-mode nil
                bidi-paragraph-direction 'left-to-right
                bidi-inhibit-bpa t)
  
  ;; Auto window scroll
  (setq auto-window-vscroll nil)
  
  :config
  ;; Custom file configuration
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file nil :nomessage))
  
  ;; Window display configurations
  (add-to-list 'display-buffer-alist
               '("^\\*Dictionary\\*"
                 (display-buffer-in-side-window)
                 (side . left)
                 (window-width . 70)))
  
  (add-to-list 'display-buffer-alist
               '("\\*Help\\*"
                 (display-buffer-reuse-window display-buffer-pop-up-window)))
  
  (add-to-list 'display-buffer-alist
               '("\\*Completions\\*"
                 (display-buffer-reuse-window display-buffer-pop-up-window)
                 (inhibit-same-window . t)
                 (window-height . 10)))
  
  ;; Key bindings
  (keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)
  (keymap-global-set "M-#" #'dictionary-lookup-definition)
  
  
  
  ;; Hooks
  (add-hook 'after-init-hook #'recentf-mode)
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))

;; Optional packages with their configurations
(use-package compat
  :straight t
  :when (version< emacs-version "29"))

(use-package project
  :straight (:type built-in))

;; Save customizations after init
(defun save-customizations-after-init ()
  "Save and reload customizations after init."
  (when custom-file
    (customize-save-customized)
    (load custom-file :noerror)))

(add-hook 'after-init-hook #'save-customizations-after-init)

;; Spell checking configuration
(with-eval-after-load 'ispell
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))


;; Add custom modules directory to load path if it exists
(let ((custom-modules (expand-file-name "modules" user-emacs-directory)))
  (when (file-directory-p custom-modules)
    (add-to-list 'load-path custom-modules)))

;; Compatibility settings for older Emacs versions
(use-package compat
  :straight t
  :when (version< emacs-version "29"))

;; Save customizations after init
(defun save-customizations-after-init ()
  "Save and reload customizations after init."
  (when custom-file
    (customize-save-customized)
    (load custom-file :noerror)))

(add-hook 'after-init-hook #'save-customizations-after-init)

;; Auto-insert template for Elisp files
(with-eval-after-load 'autoinsert
  (define-auto-insert
    "\\.el$"
    '("Emacs Lisp header"
      ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str
      (make-string (max 2 (- 80 (current-column) 27)) ?\s)
      "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
      "\n\n"
      ";; Copyright (C) " (format-time-string "%Y") "\n"
      ";; SPDX-License-Identifier: MIT\n"
      "\n"
      ";;; Commentary:\n"
      ";; " _ "\n"
      "\n"
      ";;; Code:\n\n"
      "\n"
      "(provide '" (file-name-base (buffer-file-name)) ")\n"
      ";;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")))





;;; Configuration phase
;; Some example modules to configure Emacs. Don't blindly copy these,
;; they are here for example purposes. Find the modules which work
;; for you.


;; (crafted-emacs-load-modules '(completion ui ide tartup org lisp)) 

;;; Optional configuration

;; Profile emacs startup
(defun ce-base-example/display-startup-time ()
  "Display the startup time after Emacs is fully initialized."
  (message "Crafted Emacs loaded in %s."
           (emacs-init-time)))
(add-hook 'emacs-startup-hook #'ce-base-example/display-startup-time)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom-modules"))


(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :hook (markdown-mode . visual-line-mode))  ;; Better line wrapping for markdown

;; (use-package undo-fu
;;   :ensure t)

;; (use-package undo-fu-session
;;   :ensure t
;;   :after undo-fu
;;   :config
;;   (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

;; (undo-fu-session-global-mode)

;; (require 'my-meow)

(use-package avy
  :bind
  ("C-;" . avy-goto-char-2)
  ("C-c C-j" . avy-resume))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)

(let ((mono-spaced-font "FiraCode Nerd Font")
      (proportionately-spaced-font "DejaVu Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 160 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0 :weight 'medium)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0 :weight 'medium))

(defun eglot-open-link ()
  "Open markdown link at point in the `eldoc-doc-buffer'."
  (interactive)
  (let ((url (get-text-property (point) 'help-echo)))
    (if url
	(browse-url-xdg-open url)
      (message "No URL found at point"))))



(save-place-mode 1)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.3))

(use-package magit)

(use-package pdf-tools)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package templ-ts-mode
  :straight t)
  
(use-package ispell
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en")
  ;; Optional: specify the full path if needed
  (setq ispell-program-name "/run/current-system/sw/bin/aspell")
  ;; For debugging: let's see what dictionaries ispell can find
  (ispell-change-dictionary "en" t)

  ;; Enable flyspell for real-time spell checking
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1)))))


(require 'my-themes)
(require 'my-prog)
(require 'my-org)
(require 'my-dired)
(require 'my-term)
(require 'my-ui)
(require 'my-lisp)
(require 'my-completion)
(require 'my-keybinds)                                        ; Enable for comments in code

(provide 'init)
;;; init.el ends here
