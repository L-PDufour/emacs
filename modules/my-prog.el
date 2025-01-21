;;; my-prog.el ---                                   -*- lexical-binding: t; -*-
;; Go development configuration using use-package
(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode)
  (treesit-auto-install-all))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package ibuffer-project
  :straight t
  :hook (ibuffer . (lambda ()
                     (setq ibuffer-filter-groups 
                           (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :config
  (setq ibuffer-project-use-cache t))

;; Keybindings for error navigation
(with-eval-after-load 'prog-mode
  (keymap-set prog-mode-map "C-c e n" #'flymake-goto-next-error)
  (keymap-set prog-mode-map "C-c e p" #'flymake-goto-prev-error))

(use-package go-mode
  :hook ((go-mode . flymake-mode)
         (go-mode . flymake-show-buffer-diagnostics)
         (go-mode . eglot-ensure)))

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package eglot
  :straight (:type built-in)
  :after (corfu cape)
  :init
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  :config
  (setq eglot-autoshutdown t)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point    ; LSP completions
                       #'cape-file                    ; File paths
                       ))))
  :hook ((eglot-managed-mode . mp-eglot-eldoc)
         (eglot-managed-mode . my/eglot-capf)))

(use-package eldoc
  :straight (:type built-in)
  :init
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc.*\\*$"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 10)
                 (preserve-size . (nil . t))))
  :hook
  (prog-mode . eldoc-mode))

(use-package xref
  :straight (:type built-in))
;; If you're using Crafted Emacs, you might want to keep this line
;; at the start of your config:
;; (require 'crafted-ide-config nil :noerror)
(provide 'my-prog)
;;; my-prog.el ends here
;; Copyright (C) 2025  desktop

;; Author: desktop <desktop@nixos>
