;;; my-utils.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install))

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package envrc
  :diminish envrc-mode
  :hook (after-init . envrc-global-mode))

(provide 'my-utils)
;;; my-utils.el ends here
