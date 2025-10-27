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
  :mode "\\.templ\\'"
  :config
  ;; Add this to your config to check current settings
  (defun debug-templ-indent ()
    "Debug templ indentation settings."
    (interactive)
    (when (eq major-mode 'templ-ts-mode)
      (message "tab-width: %s, indent-tabs-mode: %s, go-ts-mode-indent-offset: %s"
               tab-width indent-tabs-mode
               (if (boundp 'go-ts-mode-indent-offset) go-ts-mode-indent-offset "unbound"))))
  ;; Set up tree-sitter language source (the mode sets this itself, but we can override)
  (setq treesit-language-source-alist
        '((templ . ("https://github.com/vrischmann/tree-sitter-templ" "main"))))

  ;; Configure indentation to match what templ fmt expects
  (defun my-templ-mode-setup ()
    "Configure indentation settings for templ files to match templ fmt."
    ;; Override the mode's default settings AFTER the mode setup runs
    (setq-local tab-width 4)
    (setq-local indent-tabs-mode t)  ; templ fmt likely uses tabs

    ;; The key insight: modify the existing indent rules rather than replacing them
    ;; Check what go-ts-mode-indent-offset is set to and ensure it matches tab-width
    (when (boundp 'go-ts-mode-indent-offset)
      (setq-local go-ts-mode-indent-offset 4)))

  ;; Use after-hook to ensure our settings apply AFTER the mode's setup
  (add-hook 'templ-ts-mode-hook #'my-templ-mode-setup 90))

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
