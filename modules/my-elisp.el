;;; my-elisp.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package sly
  :straight t
  :config
  (require 'sly-quicklisp "sly-quicklisp" :no-error)
  (require 'sly-repl-ansi-color "sly-repl-ansi-color" :no-error)
  (require 'sly-asdf "sly-asdf" :no-error)
  :hook (lisp-mode . sly-editing-mode))

(use-package aggressive-indent
  :straight t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (scheme-mode . aggressive-indent-mode)))


(use-package package-lint
  :straight t)


(use-package geiser-guile
  :straight t
  :init
  (setq scheme-program-name "guile"))


(use-package macrostep-geiser
  :straight t
  :after geiser-guile)


(use-package rainbow-delimiters
  :straight t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)))

(provide 'my-elisp)
;;; my-elisp.el ends here
