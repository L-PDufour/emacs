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


(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install))

(use-package elisp-demos
  :straight t
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package helpful
  :straight t
  :bind
  (([remap describe-command] . helpful-command)
   ([remap describe-function] . helpful-callable)
   ([remap describe-key] . helpful-key)
   ([remap describe-symbol] . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ("C-h F" . helpful-function)
   :map helpful-mode-map
   ([remap revert-buffer] . helpful-update))
  :init
  (keymap-global-set "C-h K" #'describe-keymap))

(use-package breadcrumb
  :straight t
  :config
  (breadcrumb-mode))

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

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package base16-theme
  :config
  (load-theme 'base16-catppuccin-frappe t))
(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

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
(use-package eshell
  :init
  (defun bedrock/setup-eshell ()
    (keymap-set eshell-mode-map "C-r" 'consult-history)
    ;; Make completion more like modern shells
    (setq eshell-hist-ignoredups t)
    (setq eshell-scroll-to-bottom-on-input t)
    (setq eshell-prefer-lisp-functions nil)
    ;; Command aliases
    (setq eshell-aliases-file (expand-file-name "~/.emacs.d/eshell/aliases"))
    ;; Better prompt
    (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
    (setq eshell-prompt-function
          (lambda ()
            (concat (abbreviate-file-name (eshell/pwd))
                    (if (= (user-uid) 0) " # " " $ ")))))
  :hook
  ((eshell-mode . bedrock/setup-eshell)
   (eshell-mode . (lambda () (setenv "TERM" "xterm-256color")))))

;; Improve terminal experience
(use-package eat
  :custom
  (eat-term-name "xterm-256color")  ; Better color support
  (eat-kill-buffer-on-exit t)       ; Clean up when terminal exits
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode))

;; Environment management
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; Terminal popup
(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq-default popper-reference-buffers
                '("\\*Messages\\*"
                  "Output\\*$"
                  "\\*Async Shell Command\\*"
                  help-mode
                  compilation-mode
                  "*eat*"
                  "^\\*eldoc.*\\*.*$" eldoc-mode
                  ))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package dired
  :straight nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)
   ;; Auto refresh dired when file changes
   (dired-mode . auto-revert-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  ;; Better sorting
  (setq dired-listing-switches "-agho --group-directories-first")
  ;; Copy/paste between dired buffers in split windows
  (setq dired-dwim-target t)
  ;; Only show interesting files
  (setq dired-omit-files "^\\.[^.].*")
  (setq dired-omit-verbose nil)
  ;; Better deletion feedback
  (setq dired-deletion-confirmer #'y-or-n-p))

(use-package dired-subtree
  :requires dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove)))

(use-package trashed
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))


(require 'my-prog)
(require 'my-org)
(require 'my-completion)
(require 'my-keybinds)




(provide 'init)
;;; init.el ends here
