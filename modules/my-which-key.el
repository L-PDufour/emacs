;;; my-which-key.el --- Which-key configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup and configuration for which-key package

;;; Code:

(use-package which-key
  :ensure nil ; builtin
  :diminish
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))


(provide 'my-which-key)
;;; my-which-key.el ends here
