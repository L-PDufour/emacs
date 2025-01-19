;;; init.el --- Crafted Emacs Base Example -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Base example init.el (extended from the info file).
;;; Basic example of loading a module.

;;; Code:

;;; Initial phase
(use-package emacs
  :custom
  (package-install-upgrade-built-in t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (auto-save-default nil)
  (create-locfiles nil)
  (make-backup-files nil)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold 16777216 ; 16mb
                    gc-cons-percentage 0.1))))

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


(crafted-emacs-load-modules '(defaults completion ui ide tartup org lisp))
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

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :hook (markdown-mode . visual-line-mode))  ;; Better line wrapping for markdown

;; (use-package undo-fu
;;   :ensure t)

;; (use-package undo-fu-session
;;   :ensure t
;;   :after undo-fu
;;   :config
;;   (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

;; (undo-fu-session-global-mode)

;; (require 'my-meow)
(require 'my-keybinds)
(require 'my-themes)
(require 'my-prog)
(require 'my-org)
(require 'my-dired)
(require 'my-term)

(use-package avy
  :ensure t
  :bind
  ("C-;" . avy-goto-char-2)
  ("C-c C-j" . avy-resume))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)

(let ((mono-spaced-font "FiraCode Nerd Font")
      (proportionately-spaced-font "DejaVu Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 160 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0 :weight 'medium)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0 :weight 'medium))

(crafted-ide-eglot-auto-ensure-all)
(crafted-ide-configure-tree-sitter)

(defun eglot-open-link ()
  "Open markdown link at point in the `eldoc-doc-buffer'."
  (interactive)
  (let ((url (get-text-property (point) 'help-echo)))
    (if url
	(browse-url-xdg-open url)
      (message "No URL found at point"))))



(save-place-mode 1)

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.3))

(use-package magit
  :ensure t)

(use-package pdf-tools
  :ensure t)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package templ-ts-mode
 :ensure t)
  
(use-package ispell
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en")
  ;; Optional: specify the full path if needed
  (setq ispell-program-name "/run/current-system/sw/bin/aspell")
  ;; For debugging: let's see what dictionaries ispell can find
  (ispell-change-dictionary "en" t)

  ;; Enable flyspell for real-time spell checking
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1)))))
                                        ; Enable for comments in code
(provide 'init)
;;; init.el ends here
