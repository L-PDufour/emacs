;;; my-themes.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package base16-theme
  :ensure nil
  :config
  (load-theme 'base16-catppuccin-frappe t))

(use-package nerd-icons
  :ensure nil)

(use-package nerd-icons-completion
  :ensure nil
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure nil
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure nil
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'my-themes)
;;; my-themes.el ends here
