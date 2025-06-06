;;; my-themes.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; (use-package base16-theme
;;   :ensure nil
;;   :config
;;   (load-theme 'base16-catppuccin-frappe t))

(use-package catppuccin-theme
  :custom
  (catppuccin-flavor 'frappe)
  :hook
  (after-init . catppuccin-reload))


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

(use-package visual-fill-column
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
