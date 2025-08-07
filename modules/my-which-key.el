;;; my-which-key.el --- Which-key configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup and configuration for which-key package

;;; Code:

(use-package which-key
  :ensure nil     ;; This is built-in, no need to fetch it.
  :defer t        ;; Defer loading Which-Key until after init.
  :hook
  (after-init . which-key-mode)) ;; Enable which-key mode after initialization.

(provide 'my-which-key)
;;; my-which-key.el ends here
