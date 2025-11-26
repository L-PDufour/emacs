;;; my-lang.el --- Simplified Emacs Configuration -*- no-byte-compile: t; lexical-binding: t; -*-
;;; code


;;;; Nix

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  ;; Add LSP support via nil (Nix Language Server)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(nix-mode . ("nixd"))))

  ;; Configure formatter for Nix files
  (with-eval-after-load 'apheleia
	(setf (alist-get 'nixpkgs-fmt apheleia-formatters)
          '("nixpkgs-fmt"))
	;; Or use alejandra if preferred
	;; (setf (alist-get 'alejandra apheleia-formatters)
	;;       '("alejandra"))
	(setf (alist-get 'nix-mode apheleia-mode-alist)
          'nixpkgs-fmt)))

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package go-eldoc
  :ensure t
  :hook (go-mode . go-eldoc-setup))

(use-package templ-ts-mode
  :load-path "~/.emacs.d/site-lisp/templ-ts-mode"  ; Clone your fork here
  :mode "\\.templ\\'")

(use-package markdown-mode
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))


(provide 'my-lang)
;;; my-lang.el ends here
