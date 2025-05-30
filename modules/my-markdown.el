;;; my-markdown.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package markdown-mode
  :ensure nil
  :demand t
  :mode ("\\.md\\'" . markdown-mode)
  :hook ((markdown-mode . visual-line-mode)
		 (markdown-mode . eldoc-mode)))

(provide 'my-markdown)
;;; my-markdown.el ends here
