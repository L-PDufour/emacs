;;; my-prog.el --- Programming support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Core programming configuration with Eglot and completion
;;; Code:
(use-package project
  :ensure nil
  :bind ( :map project-prefix-map
          ("e" . project-eshell))
  :custom
  ;; The commands in `project-switch-commands' must be found in
  ;; `project-prefix-map'
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find directory")
     (project-vc-dir "VC-Dir")
     (project-eshell "Eshell")
     (eat-project "EAT")
     (project-any-command "Other"))))


;; Project management for ibuffer
(use-package ibuffer-project
  :ensure nil
  :hook (ibuffer . (lambda ()
                     (setq ibuffer-filter-groups
                           (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :config
  (setq ibuffer-project-use-cache t))

;;; Xref
(use-package xref
  :ensure nil
  ;; :bind ("C-M-?". xref-find-references-and-replace) ; Emacs 29.1
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (xref-show-xrefs-function #'xref-show-definitions-buffer)
  (xref-file-name-display 'project-relative)
  (xref-search-program 'ripgrep)
  (xref-history-storage 'xref-window-local-history) ; Per-window history of `xref-go-*'
  :config
  ;; We remove the fallback backend, `etags--xref-backend', which prompts the
  ;; user for an etags table -- this is undesirable for me.
  (setq-default xref-backend-functions nil)
  ;; Then add `elisp--xref-backend' as the global value of
  ;; `xref-backend-functions', which means it is run when the local value ends
  ;; with `t'. See (info "(elisp) Running Hooks") for an explanation.
  (add-hook 'xref-backend-functions #'elisp--xref-backend))
  ;;; Consult-xref-stack
(use-package consult-xref-stack
  :ensure nil
  :bind (([remap xref-go-back] . krisb-consult-xref-stack-backward)
         ([remap xref-go-forward] . krisb-consult-xref-stack-forward))
  :config
  (defun krisb-consult-xref-stack-backward (arg)
    "Call `xref-go-back' or `consult-xref-stack-backward' when called with ARG."
    (interactive "p")
    (call-interactively
     (if (< 1 arg) 'consult-xref-stack-backward 'xref-go-back)))

  (defun krisb-consult-xref-stack-forward (arg)
    "Call `xref-go-forward' or `consult-xref-stack-forward' when called with ARG."
    (interactive "p")
    (call-interactively
     (if (< 1 arg) 'consult-xref-stack-forward 'xref-go-forward))))

;;; Dumber-jump
;; A lean fork of dumb-jump.
(use-package dumber-jump
  :ensure nil
  :ensure-system-package (rg . ripgrep)
  :custom
  (dumber-jump-default-project user-emacs-directory)
  :init
  ;; Add to global value so it is used as a fallback (when local value ends in
  ;; t)
  (add-hook 'xref-backend-functions #'dumber-jump-xref-activate)
  :config
  (setopt dumber-jump-project-denoters
          (cl-remove-duplicates
           (append dumber-jump-project-denoters project-vc-extra-root-markers))))

(use-package tempel
  :ensure nil
  :custom
  (tempel-trigger-prefix "<")
  :bind (:map tempel-map
              ("M-+" . tempel-complete)
              ("M-*" . tempel-insert)
              ("C-k" . tempel-next)
              ("C-j" . tempel-previous))
  :config
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))


(use-package tempel-collection
  :ensure nil
  :after tempel)

(use-package eglot-tempel
  :ensure nil
  :after (eglot tempel)  ;; Make sure eglot and tempel are loaded first
  :config                ;; Use :config instead of :init
  (when (fboundp 'eglot-tempel-mode)
    (eglot-tempel-mode 1)))  ;; Use 1 instead of t for the mode

;;; Eldoc
(use-package eldoc
  :ensure nil
  :diminish
  :bind ( :map help-map
          ("\." . eldoc-doc-buffer))
  :custom
  (eldoc-print-after-edit nil)
  (eldoc-idle-delay 1)
  (eldoc-documentation-strategy
   'eldoc-documentation-compose-eagerly) ; Mash multiple sources together and display eagerly
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit) ; Also respects `max-mini-window-height'
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-help-at-pt t))                 ; Emacs 31.

(use-package eglot-signature-eldoc-talkative
  :ensure nil
  :after (eglot flymake eldoc)
  :config
  (defun my-eglot-specific-eldoc ()

	;; Use custom documentation-functions (with custom priorities, given
	;; by order):
	(setq-local eldoc-documentation-functions
				(list
				 #'eglot-signature-eldoc-talkative
				 #'eglot-hover-eldoc-function
				 t
				 #'flymake-eldoc-function))

	;; Optionally, in echo-area, only show the most important
	;; documentation:
	;; (setq-local eldoc-documentation-strategy
	;;   #'eldoc-documentation-enthusiast)
	)

  (add-hook 'eglot-managed-mode-hook #'my-eglot-specific-eldoc))

(defun my/eglot-capf ()
  "Set up 'completion-at-point-functions' for eglot with tempel integration."
  (interactive)
  ;; Ensure all required packages are loaded
  (require 'eglot nil t)
  (require 'cape nil t)
  (require 'tempel nil t)

  ;; Only proceed if all required packages are available
  (if (and (featurep 'eglot) (featurep 'cape) (featurep 'tempel))
      (progn
        (message "Setting up Eglot with Tempel integration")
        (setq-local completion-at-point-functions
                    (list (cape-capf-super
                           #'eglot-completion-at-point
						   #'cape-dabbrev
                           #'tempel-complete
                           #'cape-file)))
        (message "CAPF set to: %S" completion-at-point-functions))
    ;; Error message if any package is missing
    (let ((missing (cond
                    ((not (featurep 'eglot)) "eglot")
                    ((not (featurep 'cape)) "cape")
                    ((not (featurep 'tempel)) "tempel")
                    (t nil))))
      (when missing
        (message "Cannot set up eglot-capf: Package %s is not loaded" missing)))))
;; Core Eglot configuration - keep this in my-prog.el
(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  (eglot-code-action-indications '(eldoc-hint margin))
  (eglot-code-action-indicator "  α ")
  :config
  (add-to-list 'eglot-server-programs '(((js-mode :language-id "javascript")
										 (js-ts-mode :language-id "javascript")
										 (tsx-ts-mode :language-id "typescriptreact")
										 (typescript-ts-mode :language-id "typescript")
										 (typescript-mode :language-id "typescript"))
										"vtsls" "--stdio"))
  (fset 'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event

  (setq completion-category-overrides '((eglot (styles orderless))
										(eglot-capf (styles orderless))))

  ;; Automatically shut down Eglot servers when no longer needed
  (setq eglot-autoshutdown t)

  ;; Wrap Eglot completion to prevent caching issues
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :hook
  (eglot-managed-mode . my/eglot-capf))

(use-package eglot-booster
  :ensure nil
  :after eglot
  :config
  (eglot-booster-mode 1))

;; Flymake for error checking

(use-package flymake
  :ensure nil
  :after consult
  :bind (:map flymake-mode-map
			  ("C-c e e" . consult-flymake)
			  ("C-c e n" . flymake-goto-next-error)
			  ("C-c e p" . flymake-goto-prev-error))
  :hook (prog-mode . flymake-mode))

(defun eglot-open-link ()
  "Open markdown link at point in the eldoc buffer."
  (interactive)
  (let ((url (get-text-property (point) 'help-echo)))
	(if url
		(browse-url url)
	  (message "No URL found at point"))))

(provide 'my-prog)
;;; my-prog.el ends here
