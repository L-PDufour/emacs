;;; my-elisp.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'package)
(unless package-archive-contents
  (package-refresh-contents))

(use-package aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode . (lambda ()
                              (when (fboundp 'aggressive-indent-mode)
                                (aggressive-indent-mode))))
         (scheme-mode . (lambda ()
                          (when (fboundp 'aggressive-indent-mode)
                            (aggressive-indent-mode))))))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode . (lambda ()
                              (when (fboundp 'rainbow-delimiters-mode)
                                (rainbow-delimiters-mode))))
         (scheme-mode . (lambda ()
                          (when (fboundp 'rainbow-delimiters-mode)
                            (rainbow-delimiters-mode))))))

(provide 'my-elisp)
;;; my-elisp.el ends her
