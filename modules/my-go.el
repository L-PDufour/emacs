;;; my-go.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package go-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))


(provide 'my-go)

;;; my-go.el ends here
