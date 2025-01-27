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


(use-package go-mode
  :hook ((go-mode . flymake-mode)
         (go-mode . flymake-show-buffer-diagnostics)
         (go-mode . eglot-ensure)))

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package eglot
  :straight (:type built-in)
  :after (corfu cape orderless)
  :config
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))
  (setq eglot-autoshutdown t)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point    ; LSP completions
                       #'tempel-                       #'cape-file                    ; File paths
                       ))))
  :hook ((eglot-managed-mode . my/eglot-capf)))

(use-package eldoc
  :straight (:type built-in))

(use-package flymake
  :straight (:type built-in)
  :bind ( :map flymake-mode-map
          ("C-c e n" . flymake-goto-next-error)
          ("C-c e p" . flymake-goto-previous-error))
  :hook (prog-mode . flymake-mode))

(use-package eldoc-box
  :after
  (eglot eldoc)
  :bind (:map eglot-mode-map
              ("C-M-k" . my/eldoc-box-scroll-up)
              ("C-M-j" . my/eldoc-box-scroll-down)
              ("M-h" . eldoc-box-eglot-help-at-point))
  :config
  (setq eldoc-box-max-pixel-height 600)
  (defun my/eldoc-box-scroll-up ()
    "Scroll up in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-down 3))))
  (defun my/eldoc-box-scroll-down ()
    "Scroll down in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-up 3))))
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  )

;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)
(use-package eglot-tempel
  :preface (eglot-tempel-mode)
  :init
  (eglot-tempel-mode t))
;; Optional: Use the Corfu completion UI
(use-package xref
  :straight (:type built-in))
;; If you're using Crafted Emacs, you might want to keep this line
;; at the start of your config:
;; (require 'crafted-ide-config nil :noerror)
(provide 'my-prog)
;;; my-prog.el ends here
;; Copyright (C) 2025  desktop

;; Author: desktop <desktop@nixos>
