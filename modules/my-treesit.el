;;; my-treesit.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Font-lock level (3 is good balance)
(setq treesit-font-lock-level 3)

;; Mode remapping
(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (css-mode . css-ts-mode)
        (js-mode . js-ts-mode)
        (js-json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (sh-mode . bash-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (tsx-mode . tsx-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (toml-mode . toml-ts-mode)
        (go-mode . go-ts-mode)
        (rust-mode . rust-ts-mode)))

(provide 'my-treesit)
;;; my-treesit.el ends here
