;;; my-shell.el ---  -*- lexical-binding: t -*-
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

(use-package eat
  :ensure nil
  :custom
  (eat-term-name "xterm-256color")  ; Better color support
  (eat-kill-buffer-on-exit t)       ; Clean up when terminal exits
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode))

(provide 'my-shell)
;;; my-shell.el ends here
