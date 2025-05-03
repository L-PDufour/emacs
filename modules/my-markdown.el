;;; my-markdown.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package markdown-mode
  :ensure nil
  :mode ("\\.md\\'" . markdown-mode)
  :hook (markdown-mode . visual-line-mode))

(provide 'my-markdown)
;;; my-markdown.el ends here
