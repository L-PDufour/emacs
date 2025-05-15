;;; my-completion.el --- Crafted Completion Configuration -*- lexical-binding: t; -*-

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
  :ensure nil
  :bind (:map vertico-map
              ("M-;" . my/vertico-smart-insert ))
  :config
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)
  (defun my/vertico-smart-insert ()
    "Insert the current candidate and set up a transient keymap
     that will exit the minibuffer if M-; is pressed again."
    (interactive)
    (vertico-insert)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-;") #'exit-minibuffer)
      (set-transient-map map t)))
  :custom
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :hook (after-init . vertico-mode))

;; Configure directory extension.
(use-package vertico-directory
  :ensure nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure nil
  :hook (after-init . marginalia-mode))

(use-package consult
  :ensure nil
  :bind (("C-s" . consult-line)
		 ([remap goto-line] . consult-goto-line)
         :map minibuffer-local-map
         ("C-r" . consult-history)

         ;; Add bindings to the M-s (search-map)
         :map search-map  ; This is the M-s prefix map
         ("f" . consult-find)
         ("g" . consult-grep)
         ("r" . consult-ripgrep)
         ("l" . consult-line)
         ("o" . consult-outline)
         ("m" . consult-mark)
         ("i" . consult-imenu)
         ("e" . consult-flymake)
         ("k" . consult-keep-lines)
         ("u" . consult-focus-lines))

  :init
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package orderless
  :ensure nil
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :ensure nil
  :bind
  (("C-." . embark-act)
   ([remap describe-bindings] . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure nil
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure nil
  :hook ((after-init . global-corfu-mode)
         (eshell-mode . (lambda ()
                          (setq-local corfu-auto nil)
                          (corfu-mode))))
  :bind (:map corfu-map
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("C-SPC" . corfu-insert-separator)
              ("M-;" . corfu-complete))
  :init
  (setq corfu-auto t
        corfu-cycle t
        corfu-auto-prefix 2
        corfu-count 12
        corfu-auto-delay 0.2
        corfu-preselect 'directory
        corfu-on-exact-match nil
        corfu-preview-current nil
        corfu-min-width 20
        corfu-quit-no-match 'separator
        corfu-popupinfo-delay '(1.25 . 0.5))
  :config
  ;; Load extensions
  (require 'corfu-history)
  (require 'corfu-popupinfo)

  ;; Properly enable the modes
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)

  (setq text-mode-ispell-word-completion nil)

  ;; Enable terminal support
  (unless (display-graphic-p)
    (corfu-terminal-mode 1))

  ;; Add to savehist - do this BEFORE savehist-mode is activated
  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; Make sure history is saved
  (with-eval-after-load 'savehist
    (unless (member 'corfu-history savehist-additional-variables)
      (add-to-list 'savehist-additional-variables 'corfu-history))))

(use-package cape
  :ensure nil
  :demand t    ; Load immediately, not lazily
  :bind ("C-c p" . cape-prefix-map)
  :custom
  (text-mode-ispell-word-completion nil)
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history))

(with-eval-after-load 'cape

  (defun my/ignore-elisp-keywords (cand)
	(or (not (keywordp cand))
		(eq (char-after (car completion-in-region--data)) ?:)))

  (defun my/setup-elisp ()
	(interactive)
    (setq-local completion-at-point-functions
                (list (cape-capf-super
					   (cape-capf-predicate
                        #'elisp-completion-at-point
                        #'my/ignore-elisp-keywords)
					   #'cape-dabbrev)
					  #'cape-file))
    (setq-local cape-dabbrev-min-length 5))

  (add-hook 'emacs-lisp-mode-hook #'my/setup-elisp))
;; Set up elisp-specific completion with keyword filtering
(provide 'my-completion)
;;; my-completion.el ends here
