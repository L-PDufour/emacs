;;; my-web.el --- Web development configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; This module configures web development tools in Emacs with LSP support via Eglot
;;; Code:

;; Web-mode setup
;; (use-package web-mode
;;   :after eglot
;;   :mode (("\\.html?\\'" . web-mode)
;;          ("\\.css\\'" . web-mode)
;;          ("\\.jsx\\'" . web-mode)
;;          ("\\.tsx\\'" . web-mode))
;;   :config
;;   (setq web-mode-enable-auto-pairing t)
;;   (setq web-mode-enable-css-colorization t)
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   (add-to-list 'eglot-server-programs
;;                '(web-mode . ,(eglot-alternatives '(("vscode-html-language-server" "--stdio")
;;                                                    ("html-languageserver" "--stdio"))))))

;; JS/TS mode setup
(use-package js2-mode
  :mode (("\\.js\\'" . js-mode)
         ("\\.ts\\'" . typescript-mode))
  :config
  (setq js-indent-level 4
		))

(provide 'my-web)
;;; my-web.el ends here
