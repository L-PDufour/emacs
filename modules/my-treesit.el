;;; my-treesit.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package treesit-auto
  :config
  ;; Disable automatic grammar installation since Nix provides them
  (setq treesit-auto-install nil)
  
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (css-mode . css-ts-mode)
          (js-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (sh-mode . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)))
  
  ;; Let treesit-auto set up mode remapping, but use Nix grammars
  (global-treesit-auto-mode))

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate"
            :rev :newest)
  :custom
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))

(provide 'my-treesit)
;;; my-treesit.el ends here
