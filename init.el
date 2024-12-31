;;; init.el --- Crafted Emacs Base Example -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Base example init.el (extended from the info file).
;; Basic example of loading a module.

;;; Code:

;;; Initial phase

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
(add-to-list 'package-selected-packages 'ef-themes)

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




;; Some more configurations that don't require packages to be installed


(crafted-emacs-load-modules '(defaults completion ui ide startup org lisp))
(crafted-ide-configure-tree-sitter)
;;; Optional configuration

;; Profile emacs startup
(defun ce-base-example/display-startup-time ()
  "Display the startup time after Emacs is fully initialized."
  (message "Crafted Emacs loaded in %s."
           (emacs-init-time)))
(add-hook 'emacs-startup-hook #'ce-base-example/display-startup-time)

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)

;;; _
(provide 'init)
;;; init.el ends here
