;;; modus-frappe-theme.el --- Catppuccin Frappé theme using Modus engine -*- lexical-binding:t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Your Name <your-email@example.com>
;; Maintainer: Your Name <your-email@example.com>
;; URL: https://github.com/yourusername/modus-frappe-theme
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, theme, accessibility

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Catppuccin Frappé color scheme implemented using the Modus themes engine.
;; This provides full integration with Modus themes' semantic color system
;; while maintaining Catppuccin Frappé's distinctive color palette.

;;; Code:

(require 'modus-themes)

(defvar modus-frappe-palette
  (modus-themes-generate-palette
   ;; BASE-COLORS: Define the core Catppuccin Frappé colors
   '((bg-main "#303446")        ; Catppuccin Frappé base
     (fg-main "#c6d0f5")        ; Catppuccin Frappé text
     (red "#e78284")            ; Catppuccin Frappé red
     (red-warmer "#ea999c")     ; Slightly warmer red
     (red-cooler "#e78284")     ; Keep same as base red
     (green "#a6d189")          ; Catppuccin Frappé green
     (green-warmer "#a6d189")   ; Keep same
     (green-cooler "#a6d189")   ; Keep same
     (yellow "#e5c890")         ; Catppuccin Frappé yellow
     (yellow-warmer "#ef9f76")  ; Catppuccin Frappé peach
     (yellow-cooler "#e5c890")  ; Keep same
     (blue "#8caaee")           ; Catppuccin Frappé blue
     (blue-warmer "#8caaee")    ; Keep same
     (blue-cooler "#85c1dc")    ; Catppuccin Frappé sky
     (magenta "#ca9ee6")        ; Catppuccin Frappé mauve
     (magenta-warmer "#f4b8e4") ; Catppuccin Frappé pink
     (magenta-cooler "#ca9ee6") ; Keep same
     (cyan "#81c8be")           ; Catppuccin Frappé teal
     (cyan-warmer "#81c8be")    ; Keep same
     (cyan-cooler "#81c8be"))   ; Keep same
   nil  ; COOL-OR-WARM-PREFERENCE (auto-detected from bg-main)
   nil  ; CORE-PALETTE (use default modus-vivendi as base)
   ;; MAPPINGS: Optional semantic mappings (uncomment to customize)
   ;; '((cursor red-warmer)
   ;;   (bg-mode-line-active bg-main)
   ;;   (comment yellow-cooler))
   )
  "Palette for the Modus Frappé theme.")

(modus-themes-theme
 modus-frappe
 'modus-frappe-themes
 "Catppuccin Frappé dark theme using Modus engine."
 'dark
 'modus-frappe-palette
 nil
 nil)

(provide-theme 'modus-frappe)

;;; modus-frappe-theme.el ends here
