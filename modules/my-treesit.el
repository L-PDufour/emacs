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

(use-package expand-region
  :bind ("C-+" . er/expand-region))

(provide 'my-treesit)
;;; my-treesit.el ends here
