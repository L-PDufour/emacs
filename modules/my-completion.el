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
  :bind (:map vertico-map
              ("M-;" . my/vertico-smart-insert)
              ;; Add directory navigation bindings directly here
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :config
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)

  ;; Enable vertico-directory extension
  (require 'vertico-directory)

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
  :hook ((after-init . vertico-mode)
         ;; Tidy shadowed file names
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package marginalia
  :hook (after-init . marginalia-mode))


  (use-package consult
	;; Replace bindings. Lazily loaded by `use-package'.
	:bind (;; C-c bindings in `mode-specific-map'
           ("C-c M-x" . consult-mode-command)
           ("C-c h" . consult-history)
           ("C-c k" . consult-kmacro)
           ("C-c m" . consult-man)
           ("C-c i" . consult-info)
           ([remap Info-search] . consult-info)
           ;; C-x bindings in `ctl-x-map'
           ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
           ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
           ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
           ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
           ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ;; M-g bindings in `goto-map'
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
           ;; M-s bindings in `search-map'
           ("M-s d" . consult-find)                  ;; Alternative: consult-fd
           ("M-s c" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
           ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
           ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
           ;; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history)                 ;; orig. next-matching-history-element
           ("M-r" . consult-history))                ;; orig. previous-matching-history-element

	;; Enable automatic preview at point in the *Completions* buffer. This is
	;; relevant when you use the default completion UI.
	:hook (completion-list-mode . consult-preview-at-point-mode)

	;; The :init configuration is always executed (Not lazy)
	:init

	;; Tweak the register preview for `consult-register-load',
	;; `consult-register-store' and the built-in commands.  This improves the
	;; register formatting, adds thin separator lines, register sorting and hides
	;; the window mode line.
	(advice-add #'register-preview :override #'consult-register-window)
	(setq register-preview-delay 0.5)

	;; Configure other variables and modes in the :config section,
	;; after lazily loading the package.
	:config
	;; Optionally configure preview. The default value
	;; is 'any, such that any key triggers the preview.
	;; (setq consult-preview-key 'any)
	;; (setq consult-preview-key '("S-<down>" "S-<up>"))
	;; For some commands and buffer sources it is useful to configure the
	;; :preview-key on a per-command basis using the `consult-customize' macro.
	(consult-customize
	 consult-ripgrep consult-git-grep consult-grep consult-man
	 consult-bookmark consult-recent-file consult-xref
	 consult--source-bookmark consult--source-file-register
	 consult--source-recent-file consult--source-project-recent-file
	 :preview-key '(:debounce 0.5 any "M-."))

	;; Optionally configure the narrowing key.
	;; Both < and C-+ work reasonably well.
	(setq consult-narrow-key "<") ;; "C-+"

	;; Optionally make narrowing help available in the minibuffer.
	;; You may want to use `embark-prefix-help-command' or which-key instead.
	;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
	)

  (use-package orderless
	:custom
	;; Configure a custom style dispatcher (see the Consult wiki)
	;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
	;; (orderless-component-separator #'orderless-escapable-split-on-space)
	(completion-styles '(orderless basic))
	(completion-category-defaults nil)
	(completion-category-overrides '((file (styles partial-completion)))))

  (use-package embark
	:bind
	(("C-." . embark-act)
	 ([remap describe-bindings] . embark-bindings))
	:config
	;; Try to configure prefix-help-command only if the function exists
	(if (fboundp 'embark-prefix-help-command)
		(setq prefix-help-command #'embark-prefix-help-command)
      (message "Warning: embark-prefix-help-command not available in this version of Embark")))

  (use-package embark-consult
	:after (embark consult)
	:hook
	(embark-collect-mode . consult-preview-at-point-mode))

  (use-package corfu
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
