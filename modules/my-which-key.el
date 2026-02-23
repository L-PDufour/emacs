;;; my-which-key.el --- Which-key configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup and configuration for which-key package

;;; Code:

;; (use-package which-key
;;   :ensure nil     ;; This is built-in, no need to fetch it.
;;   :defer t        ;; Defer loading Which-Key until after init.
;;   :config
;;   (setq which-key-idle-delay 1.5)
;;   :diminish
;;   :hook
;;   (after-init . which-key-mode)) ;; Enable which-key mode after initialization.

(with-eval-after-load 'embark
  (setq prefix-help-command #'embark-prefix-help-command))

(with-eval-after-load 'vertico-multiform
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (vertico-multiform-mode))
(provide 'my-which-key)
;;; my-which-key.el ends here
