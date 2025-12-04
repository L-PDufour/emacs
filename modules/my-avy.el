;;; my-avy.el --- -*- lexical-binding: t -*-
;;; Code:

(use-package avy
  :ensure nil
  :bind
  ("C-;" . avy-goto-char-2)
  ("C-c C-j" . avy-resume))

(provide 'my-avy)
;;; my-avy.el ends here
