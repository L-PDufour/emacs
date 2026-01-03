;;; my-lang.el --- Simplified Emacs Configuration -*- no-byte-compile: t; lexical-binding: t; -*-
;;; code


;;;; Nix

(use-package nix-mode
  :ensure nil
  :mode "\\.nix\\'"
  :config
  ;; LSP support via nixd
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(nix-mode . ("nixd"))))
  
  ;; Configure formatter for Nix files
  (with-eval-after-load 'apheleia
    (setf (alist-get 'nixfmt-rfc-style apheleia-formatters)
          '("nixfmt"))
    (setf (alist-get 'nix-mode apheleia-mode-alist)
          'nixfmt-rfc-style)))

(use-package lua-mode
  :ensure nil
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package go-eldoc
  :ensure nil
  :hook (go-mode . go-eldoc-setup))

;; Go indentation configuration
;; Go requires tabs, not spaces
(defun my-go-mode-setup ()
  "Configure Go mode to use tabs for indentation."
  (setq-local indent-tabs-mode t)  ; Use tabs
  (setq-local tab-width 4))        ; Display tabs as 4 spaces wide

(add-hook 'go-mode-hook #'my-go-mode-setup)
(add-hook 'go-ts-mode-hook #'my-go-mode-setup)

(use-package templ-ts-mode
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/templ-ts-mode"  ; Clone your fork here
  :mode "\\.templ\\'")

(use-package markdown-mode
  :ensure nil
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package geiser
  :ensure nil
  :config
  ;; Set Guile as the default implementation
  (setq geiser-default-implementation 'guile)
  
  ;; Better REPL experience
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
  (setq geiser-repl-query-on-kill-p nil)
  
  ;; Show more in the REPL
  (setq geiser-repl-use-other-window t)
  (setq geiser-repl-query-on-exit-p nil))

(use-package geiser-guile
  :ensure nil
  :after geiser
  :config
  ;; Specify Guile binary if needed
  ;; (setq geiser-guile-binary "guile")
  
  ;; Load init file in REPL for custom procedures
  ;; (setq geiser-guile-init-file "~/.guile")
  )

(provide 'my-lang)
;;; my-lang.el ends here
