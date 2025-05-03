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

;;; Eldoc
(use-package eldoc
  :ensure nil
  :diminish
  :bind ( :map help-map
          ("\." . eldoc-doc-buffer))
  :custom
  (eldoc-print-after-edit nil)
  (eldoc-idle-delay 0.2)
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


;; Core Eglot configuration - keep this in my-prog.el
(use-package eglot
  :ensure nil
  :after (project tempel)
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  (eglot-code-action-indications '(eldoc-hint margin))
  (eglot-code-action-indicator "  α ")
  :config
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-mode typescript-ts-mode)
                 "typescript-language-server" "--stdio"))
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  (defun my/eglot-capf ()
	(setq-local completion-at-point-functions
				(cons (cape-capf-super
                       #'eglot-completion-at-point  ;; Give eglot highest priority
                       #'cape-file
                       #'cape-dabbrev             ;; Move dabbrev after eglot
                       #'tempel-complete)
                      completion-at-point-functions)))
  ;; Define the central completion function that all modes can use
  ;; (defun my/eglot-capf ()
  ;;   (setq-local completion-at-point-functions
  ;;               (list (cape-capf-super
  ;;                      #'eglot-completion-at-point
  ;;                      #'tempel-expand
  ;;                      #'cape-file))))

  (setq completion-category-overrides '((eglot (styles orderless))
										(eglot-capf (styles orderless))))

  ;; Automatically shut down Eglot servers when no longer needed
  (setq eglot-autoshutdown t)

  ;; Wrap Eglot completion to prevent caching issues
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :hook
  (eglot-managed-mode . my/eglot-capf))

;; (use-package eglot-booster
;;   :straight (:type git
;; 				   :host github
;; 				   :repo "jdtsmith/eglot-booster")

;;   :after eglot
;;   :config
;;   (eglot-booster-mode 1))

(use-package tempel
  :ensure nil
  :bind (:map tempel-map
              ("M-+" . tempel-complete)
              ("M-*" . tempel-insert)
              ("C-k" . tempel-next)
              ("C-j" . tempel-previous))
  :config
  ;; Only add tempel to completion-at-point-functions after it's loaded
  (require 'tempel)
  (defun tempel-setup-basic ()
    "Set up tempel for completion."
    (add-hook 'completion-at-point-functions #'tempel-expand 90 t))

  ;; Add hooks after defining the function
  (add-hook 'conf-mode-hook #'tempel-setup-basic)
  (add-hook 'prog-mode-hook #'tempel-setup-basic)
  (add-hook 'text-mode-hook #'tempel-setup-basic))

(use-package tempel-collection
  :ensure nil
  :after tempel)

(use-package eglot-tempel
  :ensure nil
  :after (eglot tempel)
  :config
  ;; Remove this line: (eglot-tempel-mode)
  ;; Add a conditional check instead:
  (when (fboundp 'eglot-tempel-mode)
    (eglot-tempel-mode)))

;; Flymake for error checking

(use-package flymake
  :ensure nil
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
