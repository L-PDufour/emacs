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
  ; :ensure nil
  ; :after flymake
  :custom
  (flymake-eslint-executable-name "eslint_d")
  :hook
  ;; Use a more robust hook that ensures proper loading order
  (eglot-managed-mode . (lambda ()
                          (when (derived-mode-p 'typescript-ts-mode 'web-mode 'js-ts-mode)
                            ;; Add a small delay to ensure other modes are fully loaded
                            (run-with-idle-timer 0.5 nil 'flymake-eslint-enable)))))




(provide 'my-web)
;;; my-web.el ends here
