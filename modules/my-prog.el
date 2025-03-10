;;; my-prog.el --- Programming support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Core programming configuration with Eglot and completion
;;; Code:

;; Project management for ibuffer
(use-package ibuffer-project
  :straight t
  :hook (ibuffer . (lambda ()
                     (setq ibuffer-filter-groups
                           (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :config
  (setq ibuffer-project-use-cache t))


(use-package eldoc
  :straight (:type built-in)

  :init
  ;; Configure how the eldoc buffer should be displayed
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc\\*$" ; Match exactly the eldoc buffer name
                 (display-buffer-at-bottom)
                 (window-height . 5) ; Small buffer (5 lines tall)
                 (preserve-size . (nil . t)) ; Don't let it resize automatically
                 (dedicated . t))) ; Make it a dedicated window
  :config
  ;; Set up eldoc to prefer the buffer display and avoid echo area for longer docs
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (setq eldoc-echo-area-use-multiline-p nil)     ; Don't use echo area for multi-line
  (setq eldoc-echo-area-prefer-doc-buffer t)     ; Prefer dedicated buffer
  (setq eldoc-echo-area-display-truncation-message nil) ; Don't show truncation message

  ;; You can adjust this to control when eldoc will use a buffer vs. the echo area
  (setq eldoc-echo-area-prefer-doc-buffer t)

  ;; Optional: Make the buffer dedicated to ensure it stays small
  (add-hook 'eldoc-doc-buffer-hook
            (lambda ()
              (when-let ((win (get-buffer-window eldoc--doc-buffer-name)))
                (set-window-dedicated-p win t)))))
;; ElDoc for documentation display
;; ElDoc for documentation display
;; (use-package eldoc
;;   :ensure nil  ;; Built-in package, no need to ensure
;;   :hook (prog-mode . eldoc-mode)
;;   :custom
;;   ;; Control how eldoc displays documentation
;;   (eldoc-echo-area-use-multiline-p t)    ;; Allow multi-line messages
;;   (eldoc-echo-area-prefer-doc-buffer t)  ;; Prefer displaying docs in a buffer for large docs
;;   (eldoc-echo-area-display-truncation-message nil)  ;; Don't show truncation message
;;   (eldoc-echo-area-preferred-display 'bottom)  ;; Display in the bottom part of echo area

;;   ;; Idle time in seconds before documentation is displayed
;;   (eldoc-idle-delay 0.2)

;;   ;; Adjust documentation level of detail (can be verbose for some LSP servers)
;;   (eldoc-documentation-strategy 'eldoc-documentation-default)

;;   :config
;;   ;; Use message function to display docs in the echo area
;;   (setq eldoc-message-function #'message)

;;   ;; Optional: Make the eldoc documentation-buffer more readable
;;   (add-hook 'eldoc-documentation-functions
;;             #'eldoc-documentation-default))




;; Core Eglot configuration - keep this in my-prog.el
(use-package eglot
  :straight (:type built-in)
  ;; :after (corfu cape tempel)
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event

  ;; Define the central completion function that all modes can use
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-file
                       #'cape-dabbrev))))

  ;; Register language servers for web development
  ;; HTML

  ;; ;; JavaScript/TypeScript
  ;; (add-to-list 'eglot-server-programs
  ;;              '((js-mode typescript-mode ) . ("typescript-language-server" "--stdio")))

  ;; ;; Configure workspace settings for JavaScript/TypeScript
  ;; (setq eglot-workspace-configuration
  ;;       '((:javascript . (:format (:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
  ;;                                                                                        :insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis nil)
  ;;                                 :workspaceFolder :autoDiscoverRootFiles))))

  ;; Orderless styling for Eglot completions
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))

  ;; Automatically shut down Eglot servers when no longer needed
  (setq eglot-autoshutdown t)

  ;; Wrap Eglot completion to prevent caching issues
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :hook
  (eglot-managed-mode . my/eglot-capf))
;; Enable Eglot for all relevant programming modes
;; (web-mode . eglot-ensure)
;; (css-mode . eglot-ensure)
;; (js-mode . eglot-ensure)
;; (typescript-mode . eglot-ensure))

;; Tempel for template expansion
(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook
  ((conf-mode. tempel-setup-capf)
   (prog-mode . tempel-setup-capf)
   (text-mode . tempel-setup-capf)))

(use-package tempel-collection
  :after tempel)

(use-package eglot-tempel
  :preface (eglot-tempel-mode)
  :init
  (eglot-tempel-mode t))

;; Flymake for error checking
(use-package flymake
  :straight (:type built-in)
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
