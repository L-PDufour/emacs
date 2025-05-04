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


(use-package flymake-eslint
  :ensure nil
  :custom
  (flymake-eslint-prefer-json-diagnostics t)
  :hook
  ((js-mode js-ts-mode typescript-mode typescript-ts-mode) .
   (lambda ()
     ;; Make exec-path local to this buffer
     (make-local-variable 'exec-path)
     ;; Try to find eslint_d in the project's node_modules
     (let ((project-eslint (expand-file-name "node_modules/.bin" (project-root (project-current)))))
       (when (file-directory-p project-eslint)
         (add-to-list 'exec-path project-eslint)))
     ;; Enable eslint checker
     (flymake-eslint-enable))))

(use-package apheleia
  :ensure nil
  :diminish
  :config
  (apheleia-global-mode +1)
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath)))


(provide 'my-web)
;;; my-web.el ends here
