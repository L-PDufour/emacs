;;; my-dired.el --- Dired                            -*- lexical-binding: t; -*-
;;; Commentary: Dired
;;; Code:

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)
   ;; Auto refresh dired when file changes
   (dired-mode . auto-revert-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  ;; Better sorting
  (setq dired-listing-switches "-agho --group-directories-first")
  ;; Copy/paste between dired buffers in split windows
  (setq dired-dwim-target t)
  ;; Only show interesting files
  (setq dired-omit-files "^\\.[^.].*")
  (setq dired-omit-verbose nil)
  ;; Better deletion feedback
  (setq dired-deletion-confirmer #'y-or-n-p))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; Configure directory extension.
(use-package vertico-directory
  :ensure nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(provide 'my-dired)
;;; my-dired.el ends here
;; Copyright (C) 2025  desktop

;; Author: desktop <desktop@nixos>
