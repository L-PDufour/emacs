;;; my-go.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package go-mode
  :mode "\\.go\\'")

(use-package templ-ts-mode
  :mode "\\.templ\\'"
  :config;; Try a specific commit/tag if the latest doesn't work
  (setq treesit-language-source-alist
		'((templ . ("https://github.com/vrischmann/tree-sitter-templ" "main")))))

(provide 'my-go)

;;; my-go.el ends here
