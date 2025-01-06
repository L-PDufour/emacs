;;; my-prog.el ---                                   -*- lexical-binding: t; -*-
;; Go development configuration using use-package

(require 'crafted-ide-config nil :noerror)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(use-package go-mode
  :ensure t
  :hook ((go-mode . flymake-mode)
         (go-mode . flymake-show-buffer-diagnostics)
         (go-mode . eglot-ensure)))

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :ensure t)


(use-package flymake
  :config
  ;; Configure flymake diagnostics window
  (add-to-list 'display-buffer-alist
               '("^\\*Flymake diagnostics"
                 (display-buffer-reuse-window display-buffer-pop-up-window)
                 (window-height . 10))))


;; If you're using Crafted Emacs, you might want to keep this line
;; at the start of your config:
;; (require 'crafted-ide-config nil :noerror)
(provide 'my-prog)
;;; my-prog.el ends here
;; Copyright (C) 2025  desktop

;; Author: desktop <desktop@nixos>
