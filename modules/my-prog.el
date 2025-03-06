;;; my-prog.el ---                                   -*- lexical-binding: t; -*-
;; Go development configuration using use-package
(use-package treesit-auto
  :straight t
  :config
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (css-mode . css-ts-mode)
          (js-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (sh-mode . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)
          ;; Add other mappings as needed
          ))
  (global-treesit-auto-mode)
  (treesit-auto-install-all))

(use-package ibuffer-project
  :straight t
  :hook (ibuffer . (lambda ()
                     (setq ibuffer-filter-groups
                           (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :config
  (setq ibuffer-project-use-cache t))

(use-package sly
  :straight t
  :config
  (require 'sly-quicklisp "sly-quicklisp" :no-error)
  (require 'sly-repl-ansi-color "sly-repl-ansi-color" :no-error)
  (require 'sly-asdf "sly-asdf" :no-error)
  :hook (lisp-mode . sly-editing-mode))

(use-package aggressive-indent
  :straight t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (scheme-mode . aggressive-indent-mode)))


(use-package package-lint
  :straight t)


(use-package geiser-guile
  :straight t
  :init
  (setq scheme-program-name "guile"))


(use-package macrostep-geiser
  :straight t
  :after geiser-guile)


(use-package rainbow-delimiters
  :straight t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)))
(use-package expand-region
  :bind ("C-+" . er/expand-region))

(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :mode ("\\.css\\'" . web-mode)
  :config

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t))

(use-package go-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  :hook ((before-save . gofmt-before-save)
         (go-mode . (lambda ()
                      (setq indent-tabs-mode t) ; Use tabs for indentation
                      (setq tab-width 4)        ; Set tab width to 4 spaces
                      (setq gofmt-command "goimports"))) ; Use goimports for formatting;
         (go-mode . subword-mode)
         (go-mode . flymake-mode)
         (go-mode . flymake-show-buffer-diagnostics)
         (go-mode . eglot-ensure)))

(use-package eldoc
  :ensure nil
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-message-function #'message))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package eglot
  :straight (:type built-in)
  :after (corfu cape tempel)
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config

  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-file))))

  ;; Orderless styling for Eglot completions
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))

  ;; Automatically shut down Eglot servers when no longer needed
  (setq eglot-autoshutdown t)

  ;; Wrap Eglot completion to prevent caching issues
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :hook
  (eglot-managed-mode . my/eglot-capf))

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
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
  (add-hook 'text-mode-hook 'tempel-setup-capf))


(use-package tempel-collection
  :after tempel)

(use-package eglot-tempel
  :preface (eglot-tempel-mode)
  :init
  (eglot-tempel-mode t))

(use-package flymake
  :straight (:type built-in)
  :bind ( :map flymake-mode-map
          ("C-c e e" . consult-flymake)
          ("C-c e n" . flymake-goto-next-error)
          ("C-c e p" . flymake-goto-prev-error))
  :hook (prog-mode . flymake-mode))

(use-package eldoc-box
  :after (eglot eldoc)
  :bind (
         ("C-M-k" . my/eldoc-box-scroll-up)
         ("C-M-j" . my/eldoc-box-scroll-down)
         ("C-h ." . eldoc-box-help-at-point))
  :config
  (setq eldoc-box-max-pixel-height 600)

  (defun my/eldoc-box-scroll-up ()
    "Scroll up in `eldoc-box` if it is open."
    (interactive)
    (when (and eldoc-box--frame (frame-live-p eldoc-box--frame))
      (with-selected-frame eldoc-box--frame
        (scroll-down 3))))

  (defun my/eldoc-box-scroll-down ()
    "Scroll down in `eldoc-box` if it is open."
    (interactive)
    (when (and eldoc-box--frame (frame-live-p eldoc-box--frame))
      (with-selected-frame eldoc-box--frame
        (scroll-up 3)))))

(add-to-list 'eglot-server-programs
             '(html-mode . ("vscode-html-language-server" "--stdio")))
(add-to-list 'eglot-server-programs
             '(css-mode . ("vscode-css-language-server" "--stdio")))
(add-to-list 'eglot-server-programs
             '((js-mode typescript-mode) . ("typescript-language-server" "--stdio")))
(add-to-list 'eglot-server-programs
             '(web-mode . ("vscode-html-language-server" "--stdio")))
(setq eglot-workspace-configuration
      '((:javascript . (:workspaceFolder :autoDiscoverRootFiles))))
;; Enable eglot for JavaScript
(add-hook 'html-mode-hook 'eglot-ensure)
(add-hook 'web-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)

;; Set up Cape with Corfu for JavaScript/TypeScript
(add-hook 'js-mode-hook
          (lambda ()
            ;; Enable Corfu completion
            (corfu-mode 1)
            ;; Combine completion sources
            (setq-local completion-at-point-functions
                        (list (cape-capf-super
                               #'eglot-completion-at-point
                               #'cape-file
                               #'cape-dabbrev)))))
(provide 'my-prog)
;;; my-prog.el ends here
;; Copyright (C) 2025  desktop

;; Author: desktop <desktop@nixos>
