;;; my-nix.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


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
;; Use alejandra instead if preferred
;; (setf (alist-get 'nix-mode apheleia-mode-alist)
;;       'alejandra))

(provide 'my-nix)
;;; my-nix.el ends here
