;;; my-elisp.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package aggressive-indent
  :ensure nil
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (scheme-mode . aggressive-indent-mode)))

(use-package rainbow-delimiters
  :ensure nil
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)))

(provide 'my-elisp)
;;; my-elisp.el ends here
