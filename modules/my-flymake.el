;;; my-flymake.el --- Flymake configuration -*- lexical-binding: t; -*-
;;; Commentary:


;;; Code:

(use-package flymake
  :ensure nil
  :defer t
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error) (warning "»" compilation-warning)
     (note "»" compilation-info)))
  :config
  (defun consult-flymake-project ()
    "Jump to Flymake diagnostic in project."
    (interactive)
    (consult-flymake t))
  :bind (:map flymake-mode-map
			  ("C-c e e" . consult-flymake)
			  ("C-c e l" . consult-flymake-project)
			  ("C-c e n" . flymake-goto-next-error)
			  ("C-c e p" . flymake-goto-prev-error))
  :hook (prog-mode . flymake-mode))
(provide 'my-flymake)
;;; my-flymake.el ends here

