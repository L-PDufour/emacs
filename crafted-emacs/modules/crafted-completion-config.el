;;; crafted-completion-config.el --- Crafted Completion Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Setup completion packages. Completion in this sense is more like
;; narrowing, allowing the user to find matches based on minimal
;; inputs and "complete" the commands, variables, etc from the
;; narrowed list of possible choices.

;;; Code:

;;; Vertico
;; Enable vertico
(use-package vertico
  :ensure t
  :config
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)
  :custom
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :hook (after-init . vertico-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;; Marginalia
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

;;; Consult
;; Since Consult doesn't need to be required, we assume the user wants these
;; setting if it is installed (regardless of the installation method).
(when (locate-library "consult")
  ;; Set some consult bindings
  (keymap-global-set "C-s" 'consult-line)
  (keymap-set minibuffer-local-map "C-r" 'consult-history)

  (setq completion-in-region-function #'consult-completion-in-region))

;;; Orderless

(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;; Embark
(when (require 'embark nil :noerror)

  (keymap-global-set "<remap> <describe-bindings>" #'embark-bindings)
  (keymap-global-set "C-." 'embark-act)

  ;; Use Embark to show bindings in a key prefix with `C-h`
  (setq prefix-help-command #'embark-prefix-help-command)

  (when (require 'embark-consult nil :noerror)
    (with-eval-after-load 'embark-consult
      (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))))

;;; Corfu
(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("<tab>" . corfu-complete))
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 2)
  (corfu-on-exact-match  'quit)
  (tab-always-indent 'complete)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  ;; Disable auto completion
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1)
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  ;; Sort by input history
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;; Cape
(use-package cape
  :init
  ;; Only add file and elisp-block completion globally
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  
  :config
   ;; Define the function to ignore elisp keywords
  (defun ignore-elisp-keywords (sym)
    (not (keywordp sym)))

  ;; Set up elisp-specific completion with keyword filtering
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list (cape-capf-predicate 
                                #'elisp-completion-at-point
                                #'ignore-elisp-keywords))))))


(provide 'crafted-completion-config)
;;; crafted-completion.el ends here

