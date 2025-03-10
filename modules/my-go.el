;;; my-go.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package go-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  :hook ((before-save . gofmt-before-save)
         (go-mode . (lambda ()
                      (setq indent-tabs-mode t) ; Use tabs for indentation
                      (setq tab-width 4)        ; Set tab width to 4 spaces
                      (setq gofmt-command "goimports"))) ; Use goimports for formatting;
         (go-mode . subword-mode)
         (go-mode . flymake-mode)
         (go-mode . flymake-show-buffer-diagnostics)
         (go-mode . eglot-ensure)))

(provide 'my-go)

;;; my-go.el ends here
