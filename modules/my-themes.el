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
   `((accent-0 "#8caaee")
     (accent-1 "#81c8be")
     (bg-active bg-main)
     (bg-added "#3b4252")
     (bg-added-refine "#4c566a")
     (bg-changed "#4c5a7d")
     (bg-changed-refine "#5d6f94")
     (bg-completion "#414559")
     (bg-completion-match-0 "#303446")
     (bg-completion-match-1 "#303446")
     (bg-completion-match-2 "#303446")
     (bg-completion-match-3 "#303446")
     (bg-hl-line "#292c3c")
     (bg-hover-secondary "#51576d")
     (bg-line-number-active unspecified)
     (bg-line-number-inactive "#303446")
     (bg-main "#303446")
     (bg-mark-delete "#4a3540")
     (bg-mark-select "#4c5a7d")
     (bg-mode-line-active "#232634")
     (bg-mode-line-inactive "#232634")
     (bg-prominent-err "#4a3540")
     (bg-prompt unspecified)
     (bg-prose-block-contents "#414559")
     (bg-prose-block-delimiter bg-prose-block-contents)
     (bg-region "#51576d")
     (bg-removed "#4a3540")
     (bg-removed-refine "#5d4454")
     (bg-tab-bar "#303446")
     (bg-tab-current bg-main)
     (bg-tab-other "#303446")
     (border-mode-line-active nil)
     (border-mode-line-inactive nil)
     (builtin "#8caaee")
     (comment "#737994")
     (constant "#e78284")
     (cursor "#f2d5cf")
     (date-weekday "#8caaee")
     (date-weekend "#ef9f76")
     (docstring "#a5adce")
     (err "#e78284")
     (fg-active fg-main)
     (fg-completion "#c6d0f5")
     (fg-completion-match-0 "#8caaee")
     (fg-completion-match-1 "#e78284")
     (fg-completion-match-2 "#a6d189")
     (fg-completion-match-3 "#ef9f76")
     (fg-heading-0 "#e78284")
     (fg-heading-1 "#ef9f76")
     (fg-heading-2 "#e5c890")
     (fg-heading-3 "#a6d189")
     (fg-heading-4 "#81c8be")
     (fg-line-number-active "#babbf1")
     (fg-line-number-inactive "#626880")
     (fg-link "#8caaee")
     (fg-main "#c6d0f5")
     (fg-mark-delete "#e78284")
     (fg-mark-select "#8caaee")
     (fg-mode-line-active "#a5adce")
     (fg-mode-line-inactive "#51576d")
     (fg-prominent-err "#e78284")
     (fg-prompt "#ca9ee6")
     (fg-prose-block-delimiter "#737994")
     (fg-prose-verbatim "#a6d189")
     (fg-region "#c6d0f5")
     (fnname "#8caaee")
     (fringe "#303446")
     (identifier "#ca9ee6")
     (info "#81c8be")
     (keyword "#ca9ee6")
     (name "#8caaee")
     (number "#ef9f76")
     (property "#8caaee")
     (string "#a6d189")
     (type "#e5c890")
     (variable "#ef9f76")
     (warning "#e5c890")))
  :init
  (load-theme 'modus-vivendi t)
  :config
  (modus-themes-with-colors
    (custom-set-faces
     `(change-log-acknowledgment ((,c :foreground "#babbf1")))
     `(change-log-date ((,c :foreground "#a6d189")))
     `(change-log-name ((,c :foreground "#ef9f76")))
     `(diff-context ((,c :foreground "#8caaee")))
     `(diff-hl-insert ((,c :background "#a6d189" :foreground "#a6d189")))
     `(diff-hl-delete ((,c :background "#e78284" :foreground "#e78284")))
     `(diff-hl-change ((,c :background "#8caaee" :foreground "#8caaee")))
     `(diff-file-header ((,c :foreground "#f4b8e4")))
     `(diff-header ((,c :foreground "#8caaee")))
     `(diff-hunk-header ((,c :foreground "#ef9f76")))
     `(diff-added ((,c :background "#a6d189")))
     `(diff-removed ((,c :background "#e78284")))
     `(diff-indicator-added ((,c :foreground "#a6d189")))
     `(diff-indicator-removed ((,c :foreground "#e78284")))
     `(diff-refine-added ((,c :background "#e78284")))
     `(diff-refine-removed ((,c :background "#e78284")))
     `(gnus-button ((,c :foreground "#8caaee")))
     `(gnus-group-mail-3 ((,c :foreground "#8caaee")))
     `(gnus-group-mail-3-empty ((,c :foreground "#8caaee")))
     `(gnus-header-content ((,c :foreground "#81c8be")))
     `(gnus-header-from ((,c :foreground "#ca9ee6")))
     `(gnus-header-name ((,c :foreground "#a6d189")))
     `(gnus-header-subject ((,c :foreground "#8caaee")))
     `(log-view-message ((,c :foreground "#babbf1")))
     `(match ((,c :background "#414559" :foreground "#c6d0f5")))
     `(modus-themes-search-current ((,c :background "#e78284" :foreground "#232634")))
     `(modus-themes-search-lazy ((,c :background "#414559" :foreground "#c6d0f5")))
     `(newsticker-extra-face ((,c :foreground "#737994" :height 0.8 :slant italic)))
     `(newsticker-feed-face ((,c :foreground "#e78284" :height 1.2 :weight bold)))
     `(newsticker-treeview-face ((,c :foreground "#c6d0f5")))
     `(newsticker-treeview-selection-face ((,c :background "#414559" :foreground "#c6d0f5")))
     `(tab-bar ((,c :background "#303446" :foreground "#a5adce")))
     `(tab-bar-tab ((,c :background "#303446" :underline t)))
     `(tab-bar-tab-group-current ((,c :background "#303446" :foreground "#a5adce" :underline t)))
     `(tab-bar-tab-group-inactive ((,c :background "#303446" :foreground "#737994")))
     `(tab-bar-tab-inactive ((,c :background "#303446" :foreground "#a5adce")))
     `(vc-dir-file ((,c :foreground "#8caaee")))
     `(vc-dir-header-value ((,c :foreground "#babbf1"))))))

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
