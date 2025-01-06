;;; init.el --- Crafted Emacs Base Example -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Base example init.el (extended from the info file).
;;; Basic example of loading a module.

;;; Code:

;;; Initial phase
(setq auto-save-default nil)
(setq make-backup-files nil)
;; Load the custom file if it exists.  Among other settings, this will
;; have the list `package-selected-packages', so we need to load that
;; before adding more packages.  The value of the `custom-file'
;; variable must be set appropriately, by default the value is nil.
;; This can be done here, or in the early-init.el file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Bootstrap crafted-emacs in init.el
;; Adds crafted-emacs modules to the `load-path', sets up a module
;; writing template, sets the `crafted-emacs-home' variable.
;; Adjust the path (e.g. to an absolute one)
;; depending where you cloned Crafted Emacs.
(load "~/.emacs.d/crafted-emacs/modules/crafted-init-config")

;;; Packages phase
;; Collect list of packages to install. Do not just blindly copy this
;; list, instead think about what you need and see if there is a
;; module which provides the list of packages needed. This phase is
;; not needed if manage the installed packages with Guix or Nix. It
;; is also not needed if you do not need Crafted Emacs to install
;; packages for a module, for example,
;; `crafted-speedbar-config' does not require any packages to
;; be installed.
;; Add package definitions for completion packages
;; to `package-selected-packages'.


;; Manually select "ef-themes" package

;; Install the packages listed in the `package-selected-packages' list.
(package-install-selected-packages :noconfirm)
  (defun crafted-emacs-load-modules (modules)
    "Initialize crafted-emacs modules.

  MODULES is a list of module names without the -packages or
  -config suffixes.  Note that any user-provided packages should be
  added to `package-selected-packages' before invoking this
  function."
    (dolist (m modules)
      (require (intern (format "crafted-%s-packages" m)) nil :noerror))
    (package-install-selected-packages :noconfirm)
    (dolist (m modules)
      (require (intern (format "crafted-%s-config" m)) nil :noerror)))
;;; Configuration phase
;; Some example modules to configure Emacs. Don't blindly copy these,
;; they are here for example purposes. Find the modules which work
;; for you.


(crafted-emacs-load-modules '(defaults completion ui ide startup org lisp))
(crafted-ide-configure-tree-sitter)
;;; Optional configuration

;; Profile emacs startup
(defun ce-base-example/display-startup-time ()
  "Display the startup time after Emacs is fully initialized."
  (message "Crafted Emacs loaded in %s."
           (emacs-init-time)))
(add-hook 'emacs-startup-hook #'ce-base-example/display-startup-time)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom-modules"))

(with-eval-after-load 'crafted-ide-config
      (crafted-ide-configure-tree-sitter '(go
                                           java
                                           javascript
                                           lua
                                           latex
                                           markdown
                                           python
                                           typescript)))


(require 'my-meow)
(require 'my-keybinds)
(require 'my-themes)
(require 'my-prog)

(use-package avy
  :ensure t
  :bind
  ("C-;" . avy-goto-char-2))
;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)
(let ((mono-spaced-font "FiraCode Nerd Font")
      (proportionately-spaced-font "DejaVu Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 160)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))
(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

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

(crafted-ide-eglot-auto-ensure-all)
(crafted-ide-configure-tree-sitter)
(customize-set-variable 'corfu-auto-delay 0.25)
(provide 'init)
;;; init.el ends here
