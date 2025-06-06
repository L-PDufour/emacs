;;; my-utils.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package pdf-tools
  :ensure nil
  :mode ("\\.pdf\\'" . pdf-view-mode))

(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package envrc
  :diminish envrc-mode
  :hook (after-init . envrc-global-mode))

(provide 'my-utils)
;;; my-utils.el ends here
