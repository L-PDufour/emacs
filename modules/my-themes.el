;;; my-themes.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package catppuccin-theme
  :ensure nil
  :custom
  (catppuccin-flavor 'frappe)
  :hook
  (after-init . catppuccin-reload))

(use-package nerd-icons
  :ensure nil
  :config
  ;; Ensure nerd-icons is properly loaded before other packages use it
  (require 'nerd-icons))

(use-package nerd-icons-completion
  :ensure nil
  :after (marginalia nerd-icons)
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure nil
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure nil
  :after (dired nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'my-themes)
;;; my-themes.el ends here
