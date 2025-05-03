;;; my-spelling.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package ispell
  :ensure nil
  :config
  (setq ispell-program-name "/run/current-system/sw/bin/aspell"
        ispell-dictionary "en")
  (dolist (hook '(text-mode-hook))
    (add-hook hook #'flyspell-mode)))

(provide 'my-spelling)
;;; my-spelling.el ends here
