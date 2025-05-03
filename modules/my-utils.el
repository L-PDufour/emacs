;;; my-utils.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package pdf-tools
  :ensure nil)

(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package envrc
  :ensure nil
  :diminish envrc-mode
  :hook (after-init . envrc-global-mode))

(provide 'my-utils)
;;; my-utils.el ends here
