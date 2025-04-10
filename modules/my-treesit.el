;;; my-treesit.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

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
(use-package combobulate
  :straight (:host github :repo "mickeynp/combobulate" :branch "development")
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))

(provide 'my-treesit)
;;; my-treesit.el ends here
