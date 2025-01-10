;;; my-prog.el ---                                   -*- lexical-binding: t; -*-
;; Go development configuration using use-package

(require 'crafted-ide-config nil :noerror)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(use-package go-mode
  :ensure t
  :hook ((go-mode . flymake-mode)
         (go-mode . flymake-show-buffer-diagnostics)
         (go-mode . eglot-ensure)))

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :ensure t)

(use-package eglot
  :after (corfu cape)
  :init
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  :config
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
  :ensure nil
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

;; If you're using Crafted Emacs, you might want to keep this line
;; at the start of your config:
;; (require 'crafted-ide-config nil :noerror)
(provide 'my-prog)
;;; my-prog.el ends here
;; Copyright (C) 2025  desktop

;; Author: desktop <desktop@nixos>
