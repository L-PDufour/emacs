;; config.el

;; Print "Hello, World!" to the *Messages* buffer
(message "Hello, World!")

;; Additionally, create a startup buffer with "Hello, World!"
(with-current-buffer (get-buffer-create "*hello-world*")
  (erase-buffer)
  (insert "Hello, World!")
  (display-buffer (current-buffer)))

;; Basic Emacs configuration
(setq inhibit-startup-screen t)  ; Disable the default startup screen
(setq initial-scratch-message nil)  ; Empty the initial *scratch* buffer
(menu-bar-mode -1)  ; Disable the menu bar
(tool-bar-mode -1)  ; Disable the toolbar
(scroll-bar-mode -1)  ; Disable visible scrollbar
(column-number-mode t)  ; Show column number in the mode line

;; You can add more configuration here
