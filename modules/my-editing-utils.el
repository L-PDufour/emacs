;;; my-editing-utils.el --- -*- lexical-binding: t -*-
;;; Commentary:
;; Setup and configuration for which-key package

;;; Code:
(use-package avy
  :bind
  ("C-;" . avy-goto-char-2)
  ("C-c C-j" . avy-resume))

(provide 'my-editing-utils)
;;; my-editing-utils.el ends here
