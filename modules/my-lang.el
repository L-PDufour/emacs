;;; my-lang.el --- Simplified Emacs Configuration -*- no-byte-compile: t; lexical-binding: t; -*-
;;; code


;;;; Nix

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure)
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

(use-package go-mode
  :mode "\\.go\\'")

(use-package templ-ts-mode
  :vc (:url "https://github.com/L-PDufour/templ-ts-mode.git" :rev :newest)
  :mode "\\.templ\\'"
  :mode "\\.templ\\'"
  :config;; Try a specific commit/tag if the latest doesn't work
  (setq treesit-language-source-alist
		'((templ . ("https://github.com/vrischmann/tree-sitter-templ" "main")))))

(use-package markdown-mode
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))


(unless (package-installed-p 'templ-ts-mode)
  (package-vc-install "https://github.com/L-PDufour/templ-ts-mode.git"))
(provide 'my-lang)
;;; my-lang.el ends here
