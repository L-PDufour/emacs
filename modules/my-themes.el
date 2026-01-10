;;; my-themes.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package catppuccin-theme
  :ensure nil
  :custom
  (catppuccin-flavor 'frappe)
  :hook
  (after-init . catppuccin-reload)
  :config
  ;; Customize tempel face colors
  (with-eval-after-load 'tempel
    (set-face-attribute 'tempel-default nil
                        :inherit 'font-lock-keyword-face
                        :foreground "#a6d189")  ; Green from catppuccin-frappe
    (set-face-attribute 'tempel-field nil
                        :inherit 'highlight
                        :background "#414559"   ; Surface2 from catppuccin-frappe
                        :foreground "#e5c890")  ; Yellow from catppuccin-frappe
    (set-face-attribute 'tempel-form nil
                        :inherit 'font-lock-function-name-face
                        :foreground "#81c8be")))

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
