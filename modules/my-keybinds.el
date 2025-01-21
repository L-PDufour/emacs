;;; my-keybinds.el ---                               -*- lexical-binding: t; -*-

(defun consult-ripgrep-at-point ()
  "Ripgrep the symbol at point from the project root or current directory."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (default-directory (if (fboundp 'project-root)
                                (or (project-root (project-current))
                                    default-directory)
                              default-directory)))
    (if symbol
        (consult-ripgrep default-directory symbol)
      (user-error "No symbol at point"))))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.
The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.
The DWIM behaviour of this command is as follows:
- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- When an eldoc buffer exists, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

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
(define-key my-window-keymap (kbd "u") 'winner-undo)
(define-key my-window-keymap (kbd "r") 'winner-redo)
(define-key my-window-keymap (kbd "q") 'delete-window)           ; close window
(define-key my-window-keymap (kbd "o") 'delete-other-windows)    ; close others
;; Window management bindings

;; Search operations (C-c s ...)
(define-key my-search-keymap (kbd "s") 'consult-line)            ; search in buffer
(define-key my-search-keymap (kbd "p") 'consult-ripgrep)         ; search in project
(define-key my-search-keymap (kbd "f") 'consult-find)            ; find files
(define-key my-search-keymap (kbd "g") 'consult-grep)            ; grep search
(define-key my-search-keymap (kbd "w") 'consult-ripgrep-at-point)            ; grep search

;; Code operations (C-c c ...)
(define-key my-code-keymap (kbd "d") 'xref-find-definitions)     ; go to definition
(define-key my-code-keymap (kbd "r") 'xref-find-references)      ; find references
(define-key my-code-keymap (kbd "f") 'format-buffer)             ; format buffer
(define-key my-code-keymap (kbd "a") 'eglot-code-actions)        ; code actions
(define-key my-code-keymap (kbd "h") 'display-local-help)        ; help at point


(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

(provide 'my-keybinds)

;;; my-keybinds.el ends here
