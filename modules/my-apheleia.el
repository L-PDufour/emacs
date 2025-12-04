;;; my-apheleia.el --- Code formatting configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Strategy: Use Apheleia for languages with dedicated formatters,
;; fall back to Eglot for everything else

;;; Code:

(use-package apheleia
  :diminish
  :config
  (apheleia-global-mode 1)

  ;; Add custom formatters
  (add-to-list 'apheleia-formatters
               '(templ-format "templ" "fmt" filepath))
  (add-to-list 'apheleia-formatters
               '(deno-format "deno" "fmt" filepath))

  ;; Configure prettierd formatter
  (setf (alist-get 'prettierd apheleia-formatters)
		'("prettierd" filepath))
  ;; Update mode associations to use prettierd instead of prettier
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'deno-format)
  (setf (alist-get 'templ-ts-mode apheleia-mode-alist) 'templ-format)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'deno-format))


(provide 'my-apheleia)
;;; my-apheleia.el ends here
