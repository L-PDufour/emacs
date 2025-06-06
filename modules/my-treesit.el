;;; my-treesit.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package treesit-auto
  :config
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (css-mode . css-ts-mode)
          (js-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (sh-mode . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)
          ;; Add other mappings as needed
          ))
  (global-treesit-auto-mode)
  (treesit-auto-install-all))

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate"
       :rev :newest)
  ;; :config
  ;; ;; Add a function to find-file-hook to clear highlighting in .js files
  ;; (add-hook 'find-file-hook
  ;;           (lambda ()
  ;;             (when (and buffer-file-name
  ;;                        (string-match-p "\\.js\\'" buffer-file-name))
  ;;               (combobulate-highlight-clear))))
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))


(provide 'my-treesit)
;;; my-treesit.el ends here
