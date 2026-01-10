;;; my-prog.el --- Programming support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Core programming configuration with Eglot and completion
;;; Code:
(use-package project
  :ensure nil
  :config
  (setq project-vc-extra-root-markers '(".project"))
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
  :custom
  ;; Remove trigger prefix so tempel shows completions without needing "<"
  (tempel-trigger-prefix nil)
  :bind (:map tempel-map
			  ("M-+" . tempel-complete)
			  ("M-*" . tempel-insert)
			  ("M-n" . tempel-next)
			  ("M-p" . tempel-previous))
  :config
  ;; Note: We don't add tempel to completion-at-point-functions here
  ;; because eglot buffers will use my/eglot-capf instead
  (defun tempel-setup-capf ()
	;; Add the Tempel Capf to `completion-at-point-functions' for non-eglot buffers
	(setq-local completion-at-point-functions
				(cons #'tempel-expand
					  completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))


(use-package tempel-collection
  :after tempel)

;; Disable eglot-tempel-mode - we handle tempel integration manually in my/eglot-capf
;; (use-package eglot-tempel
;;   :after (eglot tempel)
;;   :config
;;   (eglot-tempel-mode 1))


(defun my/eglot-capf ()
  "Set up completion at point with Eglot and Tempel combined using Super Capf.
This merges LSP completions with Tempel snippets into one unified list."
  (when (and (buffer-live-p (current-buffer))
             (bound-and-true-p eglot--managed-mode))
    (setq-local completion-at-point-functions
                (list
                 ;; Super Capf - Merges Eglot + Tempel + File into ONE list
                 ;; Using cape-super-capf which creates a merged CAPF
                 (cape-super-capf
                  #'tempel-expand            ; Tempel templates (exact prefix matching)
                  #'eglot-completion-at-point ; LSP completions
                  #'cape-file)               ; File path completions

                 ;; Fallback - Dabbrev as last resort when nothing else matches
                 #'cape-dabbrev))))

;; (use-package eglot-booster
;;   :vc (:url "https://github.com/jdtsmith/eglot-booster"
;;             :rev :newest)
;;   :after eglot
;;   :config
;;   (setq eglot-booster-io-only t)
;;   (eglot-booster-mode))

(use-package eglot
  :ensure nil
  :custom
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t)
  (eglot-code-action-indications '(eldoc-hint margin))
  (eglot-code-action-indicator "  α ")
  ( completion-category-overrides '((eglot (styles orderless))
                                    (eglot-capf (styles orderless))))
  (eglot-autoshutdown t)
  (jsonrpc-event-hook nil)
  (eglot-events-buffer-size 0) ; Disable event logging
  (eglot-sync-connect 0)     ; Async connection
  (eglot-report-progress nil)   ; Disable progress reports
  (eglot-ignored-server-capabilities '(:documentFormattingProvider :documentRangeFormattingProvider))
  :config
  ;; (add-to-list 'eglot-server-programs
  ;;              '(templ-ts-mode . ("lspx" "--lsp" "vscode-html-language-server --stdio " "--lsp" "templ lsp")))
  (add-to-list 'eglot-server-programs
               '(templ-ts-mode . ("templ" "lsp")))
  ;; Register Deno LSP with proper language IDs
  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript")
                  (typescript-mode :language-id "typescript"))
                 "deno" "lsp"
                 :initializationOptions
                 (:enable t
                          :lint t
                          :unstable t)))

  ;; Configure workspace settings
  (setq-default eglot-workspace-configuration
                '((:deno . ((:enable . t)
                            (:lint . t)
                            (:unstable . t)))
                  (:javascript . ((:inlayHints . ((:parameterNames . ((:enabled . "all")))
                                                  (:parameterTypes . ((:enabled . t)))
                                                  (:variableTypes . ((:enabled . t)))
                                                  (:propertyDeclarationTypes . ((:enabled . t)))
                                                  (:functionLikeReturnTypes . ((:enabled . t)))
                                                  (:enumMemberValues . ((:enabled . t)))))
                                  (:suggest . ((:completeFunctionCalls . t)))))
                  (:typescript . ((:inlayHints . ((:parameterNames . ((:enabled . "all")))
                                                  (:parameterTypes . ((:enabled . t)))
                                                  (:variableTypes . ((:enabled . t)))
                                                  (:propertyDeclarationTypes . ((:enabled . t)))
                                                  (:functionLikeReturnTypes . ((:enabled . t)))
                                                  (:enumMemberValues . ((:enabled . t)))))
                                  (:suggest . ((:completeFunctionCalls . t)))))
                  (:html . ((:includeLanguages . ((:templ . "html")))))))

  :hook
  (eglot-managed-mode . my/eglot-capf))

;;; INDENT-GUIDE
;; The `indent-guide' package provides visual indicators for indentation levels
;; in programming modes, making it easier to see code structure at a glance.
;; It draws vertical lines (by default, a character of your choice) at each
;; level of indentation, helping to improve readability and navigation within
;; the code.
(use-package highlight-indent-guides
  :ensure nil
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'bitmap)  ; No character issues
  (setq highlight-indent-guides-responsive 'top))



(defun eglot-open-link ()
  "Open markdown link at point in the eldoc buffer."
  (interactive)
  (let ((url (get-text-property (point) 'help-echo)))
	(if url
		(browse-url url)
	  (message "No URL found at point"))))


(provide 'my-prog)
;;; my-prog.el ends here
