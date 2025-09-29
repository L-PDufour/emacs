;;; my-keybinds.el --- Custom keybindings -*- lexical-binding: t; -*-


(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (deactivate-mark))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(defun my-project-find-file ()
  "Find file in current project with fallback to current directory.
If in a project, use `project-find-file`.
If not in a project, fallback to `consult-find` in current directory."
  (interactive)
  (if (and (fboundp 'project-current) (project-current))
      (call-interactively #'project-find-file)  ;; Fixed: Now using call-interactively
    (message "No project detected, falling back to consult-find in current directory")
    (if (fboundp 'consult-find)
        (consult-find default-directory)
      (call-interactively #'find-file))))


(defun my-project-find-buffer ()
  "Enhanced project-aware buffer management.
Opens consult-buffer with project filtering if in a project."
  (interactive)
  (if (and (fboundp 'project-current) (project-current))
      (consult-project-buffer)
    (call-interactively #'consult-buffer)))

(defvar my-file-keymap (make-sparse-keymap) "Keymap for file operations")
(defvar my-buffer-keymap (make-sparse-keymap) "Keymap for buffer operations")
(defvar my-window-keymap (make-sparse-keymap) "Keymap for window operations")
(defvar my-search-keymap (make-sparse-keymap) "Keymap for search operations")
(defvar my-code-keymap (make-sparse-keymap) "Keymap for code operations")

;; Ensure all required packages are loaded before binding
(require 'consult nil t)
(require 'project nil t)
(require 'winner nil t)
(require 'xref nil t)
(require 'eglot nil t)

;; Direct global bindings instead of aliases
(global-set-key (kbd "C-c f") my-file-keymap)
;; (global-set-key (kbd "C-c b") my-buffer-keymap)
(global-set-key (kbd "C-c w") my-window-keymap)
(global-set-key (kbd "C-c s") my-search-keymap)
(global-set-key (kbd "C-c l") my-code-keymap)

;; File operations (C-c f ...)
(define-key my-file-keymap (kbd "a") #'my-project-find-file)
(define-key my-file-keymap (kbd "b") #'my-project-find-buffer)
(define-key my-file-keymap (kbd "r") #'consult-recent-file)
;; (define-key my-file-keymap (kbd "s") #'save-buffer)
;; (define-key my-file-keymap (kbd "c") #'write-file)
;; (define-key my-file-keymap (kbd "p") #'project-find-file)

;; Rest of your keybindings...
;; (define-key my-buffer-keymap (kbd "b") #'switch-to-buffer)
;; (define-key my-buffer-keymap (kbd "k") #'kill-buffer)
;; (define-key my-buffer-keymap (kbd "r") #'revert-buffer)
;; (define-key my-buffer-keymap (kbd "l") #'list-buffers)

;; Window operations
(define-key my-window-keymap (kbd "v") #'split-window-right)
(define-key my-window-keymap (kbd "s") #'split-window-below)
(define-key my-window-keymap (kbd "h") #'windmove-left)
(define-key my-window-keymap (kbd "j") #'windmove-down)
(define-key my-window-keymap (kbd "k") #'windmove-up)
(define-key my-window-keymap (kbd "l") #'windmove-right)
(define-key my-window-keymap (kbd "u") #'winner-undo)
(define-key my-window-keymap (kbd "r") #'winner-redo)
(define-key my-window-keymap (kbd "q") #'delete-window)
(define-key my-window-keymap (kbd "o") #'delete-other-windows)

;; Search operations
(define-key my-search-keymap (kbd "l") #'consult-line)
(define-key my-search-keymap (kbd "G") #'consult-ripgrep)
(define-key my-search-keymap (kbd "g") #'consult-grep)
(define-key my-search-keymap (kbd "o") #'consult-outline)              ; search headings/functions
(define-key my-search-keymap (kbd "i") #'consult-imenu)                ; search structure via imenu
(define-key my-search-keymap (kbd "m") #'consult-mark)                 ; search marks
(define-key my-search-keymap (kbd "M") #'consult-global-mark)                 ; search marks
(define-key my-search-keymap (kbd "r") #'consult-history)
;; Code operations
(define-key my-code-keymap (kbd "d") #'xref-find-definitions)
(define-key my-code-keymap (kbd "r") #'xref-find-references)
(define-key my-code-keymap (kbd "f") #'apheleia-format-buffer)
(define-key my-code-keymap (kbd "a") #'eglot-code-actions)
(define-key my-code-keymap (kbd "h") #'display-local-help)
(define-key my-code-keymap (kbd "o") #'eglot-code-action-organize-imports)
;; Global keys
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(global-set-key (kbd "C-g") #'prot/keyboard-quit-dwim)

;; Add a debug message to confirm loading
(message "My keybinds module loaded successfully")

(provide 'my-keybinds)
;;; my-keybinds.el ends here
