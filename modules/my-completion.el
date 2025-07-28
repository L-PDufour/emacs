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
			  ("M-;" . vertico-insert)
			  ;; Add directory navigation bindings directly here
			  ("RET" . vertico-directory-enter)
			  ("DEL" . vertico-directory-delete-char)
			  ("M-DEL" . vertico-directory-delete-word))
  :config
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)
  :hook ((after-init . vertico-mode)
		 (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package marginalia
  :hook (after-init . marginalia-mode))(use-package marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))


(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
		 ("C-c M-x" . consult-mode-command)
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
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Aggressive asynchronous that yield instantaneous results. (suitable for
  ;; high-performance systems.) Note: Minad, the author of Consult, does not
  ;; recommend aggressive values.
  ;; Read: https://github.com/minad/consult/discussions/951
  ;;
  ;; However, the author of minimal-emacs.d uses these parameters to achieve
  ;; immediate feedback from Consult.
  (setq consult-async-input-debounce 0.02
        consult-async-input-throttle 0.05
        consult-async-refresh-delay 0.02)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)))


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
   ("M-." . embark-dwim)
   ([remap describe-bindings] . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Corfu enhances in-buffer completion by displaying a compact popup with
;; current candidates, positioned either below or above the point. Candidates
;; can be selected by navigating up or down.

(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)
  :bind (:map corfu-map
			  ("M-n" . corfu-popupinfo-scroll-up)
			  ("M-p" . corfu-popupinfo-scroll-down)
			  ("C-SPC" . corfu-insert-separator)
			  ("M-;" . corfu-complete))
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :init
  (setq corfu-auto t
		corfu-cycle t
		;; corfu-auto-prefix 2
		corfu-count 12
		corfu-auto-delay 0.2
		;; corfu-preselect 'directory
		;; corfu-on-exact-match nil
		;; corfu-preview-current nil
		corfu-min-width 20
		corfu-quit-no-match 'separator
		corfu-popupinfo-delay '(1.25 . 0.5))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  (global-corfu-mode))

(use-package cape
  :commands (cape-dabbrev cape-file cape-elisp-block)
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
