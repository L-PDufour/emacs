;;; my-keybinds.el ---                               -*- lexical-binding: t; -*-
;; Create keymaps for different categories
(defvar my-file-keymap (make-keymap) "Keymap for file operations")
(defvar my-buffer-keymap (make-keymap) "Keymap for buffer operations")
(defvar my-window-keymap (make-keymap) "Keymap for window operations")
(defvar my-search-keymap (make-keymap) "Keymap for search operations")
(defvar my-code-keymap (make-keymap) "Keymap for code operations")

;; Create aliases for easier reference
(defalias 'file my-file-keymap)
(defalias 'buffer my-buffer-keymap)
(defalias 'window my-window-keymap)
(defalias 'search my-search-keymap)
(defalias 'code my-code-keymap)

;; Bind the main prefix keys
(keymap-set global-map "C-c f" 'file)    ; File operations
(keymap-set global-map "C-c b" 'buffer)  ; Buffer operations
(keymap-set global-map "C-c w" 'window)  ; Window operations
(keymap-set global-map "C-c s" 'search)  ; Search operations
(keymap-set global-map "C-c l" 'code)    ; Code operations

;; File operations (C-c f ...)
(define-key my-file-keymap (kbd "f") 'find-file)                  ; find file
(define-key my-file-keymap (kbd "r") 'consult-recent-file)        ; recent files
(define-key my-file-keymap (kbd "s") 'save-buffer)               ; save file
(define-key my-file-keymap (kbd "c") 'write-file)                ; save as
(define-key my-file-keymap (kbd "p") 'project-find-file)         ; project files

;; Buffer operations (C-c b ...)
(define-key my-buffer-keymap (kbd "b") 'switch-to-buffer)        ; switch buffer
(define-key my-buffer-keymap (kbd "k") 'kill-buffer)             ; kill buffer
(define-key my-buffer-keymap (kbd "r") 'revert-buffer)           ; reload buffer
(define-key my-buffer-keymap (kbd "l") 'list-buffers)            ; list buffers

;; Window operations (C-c w ...)
(define-key my-window-keymap (kbd "v") 'split-window-right)      ; vertical split
(define-key my-window-keymap (kbd "s") 'split-window-below)      ; horizontal split
(define-key my-window-keymap (kbd "h") 'windmove-left)           ; move left
(define-key my-window-keymap (kbd "j") 'windmove-down)           ; move down
(define-key my-window-keymap (kbd "k") 'windmove-up)             ; move up
(define-key my-window-keymap (kbd "l") 'windmove-right)          ; move right
(define-key my-window-keymap (kbd "q") 'delete-window)           ; close window
(define-key my-window-keymap (kbd "o") 'delete-other-windows)    ; close others

;; Search operations (C-c s ...)
(define-key my-search-keymap (kbd "s") 'consult-line)            ; search in buffer
(define-key my-search-keymap (kbd "p") 'consult-ripgrep)         ; search in project
(define-key my-search-keymap (kbd "f") 'consult-find)            ; find files
(define-key my-search-keymap (kbd "g") 'consult-grep)            ; grep search

;; Code operations (C-c c ...)
(define-key my-code-keymap (kbd "d") 'xref-find-definitions)     ; go to definition
(define-key my-code-keymap (kbd "r") 'xref-find-references)      ; find references
(define-key my-code-keymap (kbd "f") 'format-buffer)             ; format buffer
(define-key my-code-keymap (kbd "a") 'eglot-code-actions)        ; code actions
(define-key my-code-keymap (kbd "h") 'display-local-help)        ; help at point

(provide 'my-keybinds)
;; Copyright (C) 2025  desktop

;; Author: desktop <desktop@nixos>
;; Keywords: 
