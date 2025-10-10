;;; my-themes.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (use-package base16-theme
;;   :ensure nil
;;   :config
;;   (load-theme 'base16-catppuccin-frappe t))

;; (use-package catppuccin-theme
;;   :custom
;;   (catppuccin-flavor 'frappe)
;;   :hook
;;   (after-init . catppuccin-reload))
(use-package modus-themes
  :ensure nil
  :defer t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts nil)
  (modus-themes-prompts '(bold intense))
  (modus-themes-common-palette-overrides
   `((accent-0 "#89b4fa")
     (accent-1 "#89dceb")
     (bg-active bg-main)
     (bg-added "#364144")
     (bg-added-refine "#4A5457")
     (bg-changed "#3e4b6c")
     (bg-changed-refine "#515D7B")
     (bg-completion "#45475a")
     (bg-completion-match-0 "#1e1e2e")
     (bg-completion-match-1 "#1e1e2e")
     (bg-completion-match-2 "#1e1e2e")
     (bg-completion-match-3 "#1e1e2e")
     (bg-hl-line "#2a2b3d")
     (bg-hover-secondary "#585b70")
     (bg-line-number-active unspecified)
     (bg-line-number-inactive "#1e1e2e")
     (bg-main "#1e1e2e")
     (bg-mark-delete "#443245")
     (bg-mark-select "#3e4b6c")
     (bg-mode-line-active "#181825")
     (bg-mode-line-inactive "#181825")
     (bg-prominent-err "#443245")
     (bg-prompt unspecified)
     (bg-prose-block-contents "#313244")
     (bg-prose-block-delimiter bg-prose-block-contents)
     (bg-region "#585b70")
     (bg-removed "#443245")
     (bg-removed-refine "#574658")
     (bg-tab-bar      "#1e1e2e")
     (bg-tab-current  bg-main)
     (bg-tab-other    "#1e1e2e")
     (border-mode-line-active nil)
     (border-mode-line-inactive nil)
     (builtin "#89b4fa")
     (comment "#9399b2")
     (constant  "#f38ba8")
     (cursor  "#f5e0dc")
     (date-weekday "#89b4fa")
     (date-weekend "#fab387")
     (docstring "#a6adc8")
     (err     "#f38ba8")
     (fg-active fg-main)
     (fg-completion "#cdd6f4")
     (fg-completion-match-0 "#89b4fa")
     (fg-completion-match-1 "#f38ba8")
     (fg-completion-match-2 "#a6e3a1")
     (fg-completion-match-3 "#fab387")
     (fg-heading-0 "#f38ba8")
     (fg-heading-1 "#fab387")
     (fg-heading-2 "#f9e2af")
     (fg-heading-3 "#a6e3a1")
     (fg-heading-4 "#74c7ec")
     (fg-line-number-active "#b4befe")
     (fg-line-number-inactive "#7f849c")
     (fg-link  "#89b4fa")
     (fg-main "#cdd6f4")
     (fg-mark-delete "#f38ba8")
     (fg-mark-select "#89b4fa")
     (fg-mode-line-active "#bac2de")
     (fg-mode-line-inactive "#585b70")
     (fg-prominent-err "#f38ba8")
     (fg-prompt "#cba6f7")
     (fg-prose-block-delimiter "#9399b2")
     (fg-prose-verbatim "#a6e3a1")
     (fg-region "#cdd6f4")
     (fnname    "#89b4fa")
     (fringe "#1e1e2e")
     (identifier "#cba6f7")
     (info    "#94e2d5")
     (keyword   "#cba6f7")
     (keyword "#cba6f7")
     (name "#89b4fa")
     (number "#fab387")
     (property "#89b4fa")
     (string "#a6e3a1")
     (type      "#f9e2af")
     (variable  "#fab387")
     (warning "#f9e2af")))
  :config
  (modus-themes-with-colors
    (custom-set-faces
     `(change-log-acknowledgment ((,c :foreground "#b4befe")))
     `(change-log-date ((,c :foreground "#a6e3a1")))
     `(change-log-name ((,c :foreground "#fab387")))
     `(diff-context ((,c :foreground "#89b4fa")))
     `(diff-file-header ((,c :foreground "#f5c2e7")))
     `(diff-header ((,c :foreground "#89b4fa")))
     `(diff-hunk-header ((,c :foreground "#fab387")))
     `(gnus-button ((,c :foreground "#8aadf4")))
     `(gnus-group-mail-3 ((,c :foreground "#8aadf4")))
     `(gnus-group-mail-3-empty ((,c :foreground "#8aadf4")))
     `(gnus-header-content ((,c :foreground "#7dc4e4")))
     `(gnus-header-from ((,c :foreground "#cba6f7")))
     `(gnus-header-name ((,c :foreground "#a6e3a1")))
     `(gnus-header-subject ((,c :foreground "#8aadf4")))
     `(log-view-message ((,c :foreground "#b4befe")))
     `(match ((,c :background "#3e5768" :foreground "#cdd6f5")))
     `(modus-themes-search-current ((,c :background "#f38ba8" :foreground "#11111b" ))) ;; :foreground "#cdd6f4" -- Catppuccin default, not that visible...
     `(modus-themes-search-lazy ((,c :background "#3e5768" :foreground "#cdd6f5")))     ;; :foreground "#cdd6f4" :background "#94e2d5" -- Catppuccin default, not that visible...
     `(newsticker-extra-face ((,c :foreground "#9399b2" :height 0.8 :slant italic)))
     `(newsticker-feed-face ((,c :foreground "#f38ba8" :height 1.2 :weight bold)))
     `(newsticker-treeview-face ((,c :foreground "#cdd6f4")))
     `(newsticker-treeview-selection-face ((,c :background "#3e5768" :foreground "#cdd6f5")))
     `(tab-bar ((,c :background "#1e1e2e" :foreground "#bac2de")))
     `(tab-bar-tab ((,c :background "#1e1e2e" :underline t)))
     `(tab-bar-tab-group-current ((,c :background "#1e1e2e" :foreground "#bac2de" :underline t)))
     `(tab-bar-tab-group-inactive ((,c :background "#1e1e2e" :foreground "#9399b2"))))
    `(tab-bar-tab-inactive ((,c :background "#1e1e2e" :foreground "#a6adc8")))
    `(vc-dir-file ((,c :foreground "#89b4fa")))
    `(vc-dir-header-value ((,c :foreground "#b4befe"))))
  :init
  (load-theme 'modus-vivendi t))
(use-package nerd-icons
  :ensure t
  :config
  ;; Ensure nerd-icons is properly loaded before other packages use it
  (require 'nerd-icons))

(use-package nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :after (dired nerd-icons)
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
  (global-visual-fill-column-mode 1))

(provide 'my-themes)
;;; my-themes.el ends here
