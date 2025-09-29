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
										; (eat-project "EAT")
	 (project-any-command "Other"))))


(use-package xref
  :ensure nil
										; :after consult
  :custom
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  (xref-file-name-display 'project-relative)
  (xref-search-program 'ripgrep))

;; A lean fork of dumb-jump.
(use-package tempel
  ;; :custom
  ;; (tempel-trigger-prefix "<")
  :bind (:map tempel-map
			  ("M-+" . tempel-complete)
			  ("M-*" . tempel-insert)
			  ("M-n" . tempel-next)
			  ("M-p" . tempel-previous))
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
  :after tempel)

(use-package eglot-tempel
  :after (eglot tempel)  ;; Make sure eglot and tempel are loaded first
  :config                ;; Use :config instead of :init
  (eglot-tempel-mode 1))  ;; Use 1 instead of t for the ode

;;; Eldoc
(use-package eldoc
  :diminish
  :ensure t
  :defer t
  :config
  (setq eldoc-idle-delay 0)                  ;; Automatically fetch doc help
  (setq eldoc-echo-area-display-truncation-message nil)
  :init
  (global-eldoc-mode))

(defun my/eglot-capf ()
  "Set up completion with buffer validation."
  (when (and (buffer-live-p (current-buffer))
			 (bound-and-true-p eglot--managed-mode))
	(setq-local completion-at-point-functions
				(list #'eglot-completion-at-point
					  #'tempel-complete
					  #'cape-dabbrev
					  #'cape-file))))

(use-package eglot
  :ensure nil
  :custom
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t)
  (eglot-code-action-indications '(eldoc-hint margin))
  (eglot-code-action-indicator "  α ")
  (eglot-ignored-server-capabilities '(:documentFormattingProvider :documentRangeFormattingProvider))
  :config
  ;; (add-to-list 'eglot-server-programs
  ;;              '(templ-ts-mode . ("lspx" "--lsp" "vscode-html-language-server --stdio " "--lsp" "templ lsp")))
  (add-to-list 'eglot-server-programs
               '(templ-ts-mode . ("templ" "lsp")))
  ;; (fset 'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  (setq-default eglot-workspace-configuration
                (list
                 (cons :deno  (list
                               :enable t
                               :lint t
                               :unstable t))
                 (cons :html  (list :includeLanguages (list :templ "html")
                                    ))))

  ;; Rest of your configuration...
  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript")
                  (typescript-mode :language-id "typescript"))
                 "deno" "lsp"))

  ;; Performance optimizations
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))
  (setq eglot-autoshutdown t)
  (setq eglot-events-buffer-size 0) ; Disable event logging
  (setq eglot-sync-connect nil)     ; Async connection

  ;; Reduce file watching
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  :hook
  (eglot-managed-mode . my/eglot-capf))

;;; INDENT-GUIDE
;; The `indent-guide' package provides visual indicators for indentation levels
;; in programming modes, making it easier to see code structure at a glance.
;; It draws vertical lines (by default, a character of your choice) at each
;; level of indentation, helping to improve readability and navigation within
;; the code.
(use-package indent-guide
  :defer t
  :ensure t
  :hook
  (prog-mode . indent-guide-mode)  ;; Activate indent-guide in programming modes.
  :config
  (setq indent-guide-char "│"))

(use-package flymake
  :ensure nil
  :defer t
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error) (warning "»" compilation-warning)
     (note "»" compilation-info)))
  :config
  (defun consult-flymake-project ()
    "Jump to Flymake diagnostic in project."
    (interactive)
    (consult-flymake t))
  :bind (:map flymake-mode-map
			  ("C-c e e" . consult-flymake)
			  ("C-c e l" . consult-flymake-project)
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

(use-package apheleia
  :diminish
  :config
  (apheleia-global-mode 1)

  ;; Add custom formatters
  (add-to-list 'apheleia-formatters
               '(templ-format "templ" "fmt" filepath))
  (add-to-list 'apheleia-formatters
               '(deno-format "deno" "fmt" filepath))

  ;; Configure prettierd formatter
  (setf (alist-get 'prettierd apheleia-formatters)
		'("prettierd" filepath))
  (add-to-list 'apheleia-formatters
               '(gofmt "gofmt"))
  (add-to-list 'apheleia-mode-alist
               '(go-mode . gofmt))
  (add-to-list 'apheleia-mode-alist
               '(go-ts-mode . gofmt))
  ;; Update mode associations to use prettierd instead of prettier
  (setf (alist-get 'js-mode apheleia-mode-alist) 'prettierd)
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'deno-format)
  (setf (alist-get 'typescript-mode apheleia-mode-alist) 'prettierd)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettierd)
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettierd)
  (setf (alist-get 'json-mode apheleia-mode-alist) 'prettierd)
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'prettierd)
  (setf (alist-get 'css-mode apheleia-mode-alist) 'prettierd)
  (setf (alist-get 'css-ts-mode apheleia-mode-alist) 'prettierd)
  (setf (alist-get 'html-mode apheleia-mode-alist) 'prettierd)
  (setf (alist-get 'web-mode apheleia-mode-alist) 'prettierd))
(provide 'my-prog)
;;; my-prog.el ends here
