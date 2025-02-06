;;; my-lisp.el --- Crafted Completion Configuration -*- lexical-binding: t; -*-
;;; Lisp Development Configuration

;; ElDoc for all Lisp modes
(use-package eldoc
  :straight t
  :hook ((emacs-lisp-mode . eldoc-mode)
         (lisp-mode . eldoc-mode)
         (scheme-mode . eldoc-mode)))
(use-package sly
  :straight t
  :config
  (require 'sly-quicklisp "sly-quicklisp" :no-error)
  (require 'sly-repl-ansi-color "sly-repl-ansi-color" :no-error)
  (require 'sly-asdf "sly-asdf" :no-error)
  :hook (lisp-mode . sly-editing-mode))
;; Aggressive indentation for Lisp modes
(use-package aggressive-indent
  :straight t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (scheme-mode . aggressive-indent-mode)))

;; Package linting for Emacs Lisp
(use-package package-lint
  :straight t)

;; Geiser for Guile (Guix uses Guile Scheme)
(use-package geiser-guile
  :straight t
  :init
  (setq scheme-program-name "guile"))

;; Optional but recommended for Guile development
(use-package macrostep-geiser
  :straight t
  :after geiser-guile)

;; Optional: For better parentheses handling
(use-package rainbow-delimiters
  :straight t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)))

;; Optional: For structural editing

(provide 'my-lisp)

;;; my-lisp.el ends here
