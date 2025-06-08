;;; my-elisp.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
		 (scheme-mode .	aggressive-indent-mode)))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
		 (scheme-mode .	aggressive-indent-mode)))

(provide 'my-elisp)
;;; my-elisp.el ends her
