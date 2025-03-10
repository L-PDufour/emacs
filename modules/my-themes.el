;;; my-themes.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package base16-theme
  :config
  (load-theme 'base16-catppuccin-frappe t))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'my-themes)
;;; my-themes.el ends here
