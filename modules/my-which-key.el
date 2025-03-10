;;; my-which-key.el --- Which-key configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup and configuration for which-key package

;;; Code:

(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

(provide 'my-which-key)
;;; my-which-key.el ends here
