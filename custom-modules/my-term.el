;;; my-term.el --- terminal things                   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eshell
  :ensure nil
  :init
  (defun bedrock/setup-eshell ()
    (keymap-set eshell-mode-map "C-r" 'consult-history)
    ;; Make completion more like modern shells
    (setq eshell-hist-ignoredups t)
    (setq eshell-scroll-to-bottom-on-input t)
    (setq eshell-prefer-lisp-functions nil)
    ;; Command aliases
    (setq eshell-aliases-file (expand-file-name "~/.emacs.d/eshell/aliases"))
    ;; Better prompt
    (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
    (setq eshell-prompt-function
          (lambda ()
            (concat (abbreviate-file-name (eshell/pwd))
                   (if (= (user-uid) 0) " # " " $ ")))))
  :hook 
  ((eshell-mode . bedrock/setup-eshell)
   (eshell-mode . (lambda () (setenv "TERM" "xterm-256color")))))

;; Improve terminal experience
(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm-256color")  ; Better color support
  (eat-kill-buffer-on-exit t)       ; Clean up when terminal exits
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode))

;; Environment management
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode)
  :config
  (setq envrc-none-lighter nil)
  (setq envrc-on-lighter " env"))

;; Terminal popup
(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          "*eat*"
          "^\\*eldoc.*\\*.*$" eldoc-mode
          ))
  (popper-mode +1)
  (popper-echo-mode +1))  

(provide 'my-term)

;;; my-term.el ends here
;; Copyright (C) 2025  desktop

;; Author: desktop <desktop@nixos>
