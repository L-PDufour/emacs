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

(use-package visual-fill-column
  :ensure nil
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  (visual-fill-column-enable-sensible-window-split t)
  :bind
  ("C-c t c" . visual-fill-column-mode)  ;; Toggle centered mode with C-c t c
  :config

  (global-visual-fill-column-mode 1)
  )

(provide 'my-themes)
;;; my-themes.el ends here
