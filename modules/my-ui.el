;;; my-ui.el --- Crafted Completion Configuration -*- lexical-binding: t; -*-

;;; UI Configuration

(use-package helpful
  :straight t
  :bind
  (([remap describe-command] . helpful-command)
   ([remap describe-function] . helpful-callable)
   ([remap describe-key] . helpful-key)
   ([remap describe-symbol] . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ("C-h F" . helpful-function)
   :map helpful-mode-map
   ([remap revert-buffer] . helpful-update))
  :init
  (keymap-global-set "C-h K" #'describe-keymap))

;; Enable line numbers for programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;; Disable line numbers for org mode
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Set line number display properties
(setq-default display-line-numbers-grow-only t
              display-line-numbers-type t
              display-line-numbers-width 2)

;; Pulse effect when changing focus
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
                  scroll-down-command
                  recenter-top-bottom
                  other-window))
  (advice-add command :after #'pulse-line))

(use-package elisp-demos
  :straight t
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package breadcrumb
  :straight t
  :config
  (breadcrumb-mode))

(provide 'my-ui)
;;; my-ui.el ends here
