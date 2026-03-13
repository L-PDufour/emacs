;;; init.el --- Main configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Tangled from config.org — do not edit by hand.
;; Based on Emacs-Kick by Rahul Martim Juliato, adapted for NixOS + IGC + Sway.
;;; Code:

(setq package-archives nil)
(setq package-enable-at-startup nil)

(require 'package)
(package-initialize)
(require 'use-package)
(setq use-package-always-ensure nil)

(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/"))
(when (file-exists-p custom-file)
  (load custom-file :noerror :nomessage))

(setq auto-save-list-file-prefix (expand-file-name "autosave/" user-emacs-directory))
(setq backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq savehist-file (expand-file-name "history" user-emacs-directory))
(setq project-list-file (expand-file-name "projects.eld" user-emacs-directory))
(setq transient-history-file (expand-file-name "transient/history.el" user-emacs-directory))
(setq transient-levels-file (expand-file-name "transient/levels.el" user-emacs-directory))
(setq transient-values-file (expand-file-name "transient/values.el" user-emacs-directory))
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

(use-package emacs
  :ensure nil
  :custom
  (auto-save-default nil)
  (column-number-mode t)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (delete-selection-mode 1)
  (global-auto-revert-mode 1)
  (inhibit-startup-message t)
  (make-backup-files nil)
  (pixel-scroll-precision-mode t)        ;; Smooth scrolling on pgtk/Wayland
  (ring-bell-function 'ignore)
  (split-width-threshold 170)
  (tab-width 4)
  (treesit-font-lock-level 4)
  (use-short-answers t)
  (blink-cursor-mode nil)

  :hook
  (prog-mode . display-line-numbers-mode)

  :config
  (let ((mono-font "FiraCode Nerd Font")
		(sans-font "DejaVu Sans"))
	;; Semi-bold is usually a good middle ground
	(set-face-attribute 'default nil :family mono-font :weight 'semi-bold :height 180)
	(set-face-attribute 'fixed-pitch nil :family mono-font :weight 'semi-bold :height 180)
	(set-face-attribute 'variable-pitch nil :family sans-font :height 180))
  ;; Font — adjust family/size to taste

  ;; Skip special buffers with ]b / [b
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  ;; Custom file
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; Pretty vertical border
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  ;; Default coding
  (modify-coding-system-alist 'file "" 'utf-8)

  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when scroll-bar-mode (scroll-bar-mode -1))
  (global-hl-line-mode 1)
  (global-auto-revert-mode 1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode 1)
  (repeat-mode 1)
  (file-name-shadow-mode 1)

  ;; Compilation
  (setq compilation-scroll-output t)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

  ;; Welcome message in *scratch*
  (add-hook 'after-init-hook
            (lambda ()
              (with-current-buffer (get-buffer-create "*scratch*")
                (insert (format ";;    Welcome to Emacs!\n;;\n;;    Loading time : %s\n"
                                (emacs-init-time)))))))

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     ("\\*\\(Flymake diagnostics\\|xref\\|Completions\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1)))))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)))

(use-package dired-subtree
  :ensure nil
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-remove)))

(use-package isearch
  :ensure nil
  :config
  (setq isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil
        search-whitespace-regexp ".*?")
  :bind (("C-s" . isearch-forward)
         ("C-r" . isearch-backward)))

(use-package eldoc
  :ensure nil
  :diminish
  :custom
  (eldoc-idle-delay 0.5)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil)
  :init
  (global-eldoc-mode))

(use-package flymake
  :ensure nil
  :defer t
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error)
     (warning "»" compilation-warning)
     (note "»" compilation-info)))
  (put 'flymake-goto-next-error 'repeat-map 'flymake-goto-next-error)
  (put 'flymake-goto-prev-error 'repeat-map 'flymake-goto-prev-error  i)
  :config
  (defun consult-flymake-project ()
	"Jump to Flymake diagnostic in project."
	(interactive)
	(consult-flymake t))
  :bind (:map flymake-mode-map
			  ("C-c e e" . consult-flymake)
			  ("C-c e l" . consult-flymake-project)
			  ("C-c e n" . flymake-goto-next-error)
			  ("C-c e p" . flymake-goto-prev-error)))

(use-package org
  :ensure nil
  :defer t
  :custom
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-return-follows-link t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-directory "~/Sync/personal/")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . auto-save-mode))
  :bind ("C-c c" . org-capture))

(use-package org-appear
  :ensure nil
  :hook (org-mode . org-appear-mode))

(use-package which-key
  :ensure nil
  :defer t
  :custom
  (which-key-idle-delay 1.0)
  :diminish
  :hook (after-init . which-key-mode))

(use-package smerge-mode
  :ensure nil
  :defer t
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-previous)))

(use-package vertico
  :ensure nil
  :hook (after-init . vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle nil)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :config
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face '(:foreground "#80adf0" :weight bold))
                   "  ")
                 cand))))

(use-package orderless
  :ensure nil
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure nil
  :hook (after-init . marginalia-mode))

(use-package consult
  :ensure nil
  :defer t
  :bind (("C-x b"   . consult-buffer)
         ("M-y"     . consult-yank-pop)
         ("M-g g"   . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s g"   . consult-grep))
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark
  :ensure nil
  :defer t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)))

(use-package embark-consult
  :ensure nil
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure nil
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  (corfu-popupinfo-delay 0.5)
  (corfu-on-exact-match nil)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1))

(use-package cape
  :ensure nil
  :bind ("C-c p" . cape-prefix-map)
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history))

(use-package catppuccin-theme
  :ensure nil
  :custom
  (catppuccin-flavor 'frappe)
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package nerd-icons
  :ensure nil
  :defer t)

(use-package nerd-icons-dired
  :ensure nil
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure nil
  :after (:all nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure nil
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package doom-modeline
  :ensure nil
  :defer t
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-name t)
  (doom-modeline-vcs-max-length 25)
  (doom-modeline-icon t)
  :hook (after-init . doom-modeline-mode))

(setq treesit-font-lock-level 4)

(setq major-mode-remap-alist
      '((c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (css-mode        . css-ts-mode)
        (js-mode         . js-ts-mode)
        (js-json-mode    . json-ts-mode)
        (python-mode     . python-ts-mode)
        (sh-mode         . bash-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode       . yaml-ts-mode)
        (toml-mode       . toml-ts-mode)
        (go-mode         . go-ts-mode)
        (rust-mode       . rust-ts-mode)))

(use-package eglot
  :ensure nil
  :init
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-complete
                       #'cape-file))))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

  :custom
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-report-progress nil)
  (eglot-ignored-server-capabilities
   '(:documentFormattingProvider :documentRangeFormattingProvider))

  :config
  ;; Deno for JS/TS
  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript"))
                 "deno" "lsp"
                 :initializationOptions (:enable t :lint t :unstable t)))

  ;; Nix via nixd
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd"))))

(use-package consult-eglot
  :ensure nil
  :after eglot
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)))

(use-package xref
  :ensure nil
  :custom
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  (xref-file-name-display 'project-relative)
  (xref-search-program 'ripgrep))

(use-package project
  :ensure nil
  :config
  (setq project-vc-extra-root-markers '(".project"))
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find directory")
     (project-vc-dir "VC-Dir")
     (project-eshell "Eshell")
     (project-any-command "Other"))))

;; (use-package yasnippet
;;   :custom (yas-keymap-disable-hook
;;            (lambda () (and (frame-live-p corfu--frame)
;;                            (frame-visible-p corfu--frame))))
;;   :hook (after-init . yas-global-mode))

;; (use-package yasnippet-snippets
;;   :ensure nil
;;   :after yasnippet)

;; (use-package yasnippet-capf
;;   :ensure nil
;;   :after cape)

;; Configure Tempel
(use-package tempel
  :ensure nil
  :custom
  (tempel-path (expand-file-name "templates" user-emacs-directory))
  (tempel-trigger-prefix "<")
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :bind (:map tempel-map
              ("M-n" . tempel-next)
              ("M-p" . tempel-previous))
  :hook ((prog-mode . tempel-setup-capf)
         (text-mode . tempel-setup-capf)
         (org-mode  . tempel-setup-capf))
  :init
  (defun tempel-setup-capf ()
    "Add `tempel-complete' to `completion-at-point-functions'."
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions))))
;; Optional: Add tempel-collection if you want ready-made templates.
(use-package tempel-collection
  :after tempel
  :ensure nil)

;; Optional: Use the Corfu completion UI

(use-package eglot-tempel
  :after tempel
  :ensure nil
  :init
  (eglot-tempel-mode t))

(use-package apheleia
  :ensure nil
  :diminish
  :config
  (apheleia-global-mode 1)
  (add-to-list 'apheleia-formatters
               '(templ-format "templ" "fmt" filepath))
  (setf (alist-get 'nixfmt-rfc-style apheleia-formatters)
        '("nixfmt"))
  (setf (alist-get 'nix-mode apheleia-mode-alist)
        'nixfmt-rfc-style)
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'deno-format)
  (setf (alist-get 'templ-ts-mode apheleia-mode-alist) 'templ-format)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'deno-format))

(use-package indent-bars
  :ensure nil
  :hook ((prog-mode . indent-bars-mode))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-pattern ".")
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-highlight-current-depth '(:blend 0.5)))

(use-package nix-mode
  :ensure nil
  :mode "\\.nix\\'")

(use-package lua-mode
  :ensure nil
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package templ-ts-mode
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/templ-ts-mode"
  :mode "\\.templ\\'")

(use-package go-eldoc
  :ensure nil
  :hook (go-mode . go-eldoc-setup))

(defun my-go-mode-setup ()
  "Go requires tabs."
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4))
(add-hook 'go-mode-hook #'my-go-mode-setup)
(add-hook 'go-ts-mode-hook #'my-go-mode-setup)

(use-package markdown-mode
  :ensure nil
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package geiser
  :ensure nil
  :config
  (setq geiser-default-implementation 'guile))

(use-package geiser-guile
  :ensure nil
  :after geiser)

(use-package rainbow-delimiters
  :ensure nil
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dotenv-mode
  :ensure nil
  :defer t)

(use-package magit
  :ensure nil
  :defer t)

(use-package diff-hl
  :ensure nil
  :hook
  ((after-init  . global-diff-hl-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  :custom
  (diff-hl-side 'left)
  (diff-hl-margin-symbols-alist
   '((insert . "┃") (delete . "-") (change . "┃")
     (unknown . "┆") (ignored . "i"))))

(defun my-smarter-move-beginning-of-line (arg)
  "Move point to first non-whitespace character, or beginning of line.
Press again to toggle between indentation and column 0."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun my-duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (let ((column (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (forward-line 1)
    (yank)
    (move-to-column column)))

(defun my-move-line-up ()
  "Move current line up one line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun my-move-line-down ()
  "Move current line down one line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun my-comment-or-uncomment ()
  "Toggle comment on current line or region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun my-indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-a")       #'my-smarter-move-beginning-of-line)
(global-set-key (kbd "C-c d")     #'my-duplicate-line)
(global-set-key (kbd "M-<up>")    #'my-move-line-up)
(global-set-key (kbd "M-<down>")  #'my-move-line-down)
(global-set-key (kbd "C-;")       #'my-comment-or-uncomment)
(global-set-key (kbd "C-c i")     #'my-indent-buffer)

;; (defvar my-go-back-history '()
;;   "Stack of markers for forward navigation after going back.")

;; (defun my-go-back ()
;;   "Go back: try xref history first, then fall back to mark ring."
;;   (interactive)
;;   (let ((old (point-marker)))
;;     (condition-case nil
;;         (progn (xref-go-back) (push old my-go-back-history))
;;       (error
;;        (if mark-ring
;;            (progn
;;              (push old my-go-back-history)
;;              (goto-char (car mark-ring))
;;              (setq mark-ring (cdr mark-ring)))
;;          (message "No back history")))))
;;   (recenter))

;; (defun my-go-forward ()
;;   "Go forward through the go-back history."
;;   (interactive)
;;   (if my-go-back-history
;;       (let ((marker (pop my-go-back-history)))
;;         (when (marker-buffer marker)
;;           (switch-to-buffer (marker-buffer marker))
;;           (goto-char marker)
;;           (recenter)))
;;     (message "No forward history")))

;; (global-set-key (kbd "C-o") #'my-go-back)
;; (global-set-key (kbd "M-i") #'my-go-forward)  ;; M-i because C-i = TAB in terminal
;; (when (display-graphic-p)
;;   (global-set-key (kbd "C-i") #'my-go-forward))

(defvar my-auto-center-commands
  '(xref-find-definitions my-go-back my-go-forward
						  isearch-forward isearch-backward
						  consult-line consult-imenu
						  beginning-of-buffer end-of-buffer
						  next-error previous-error)
  "Commands after which the cursor auto-centers.")

(defun my-auto-center-advice (&rest _)
  "Recenter window after certain navigation commands."
  (when (memq this-command my-auto-center-commands)
    (recenter)))

(dolist (cmd my-auto-center-commands)
  (advice-add cmd :after #'my-auto-center-advice))

(defvar sexp-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'forward-sexp)
    (define-key map (kbd "b") #'backward-sexp)
    (define-key map (kbd "u") #'backward-up-list)
    (define-key map (kbd "d") #'down-list)
    (define-key map (kbd "n") #'forward-list)
    (define-key map (kbd "p") #'backward-list)
    (define-key map (kbd "a") #'beginning-of-defun)
    (define-key map (kbd "e") #'end-of-defun)
    (define-key map (kbd "SPC") #'mark-sexp)
    (define-key map (kbd "k") #'kill-sexp)
    (define-key map (kbd "t") #'transpose-sexps)
    map)
  "Repeat map for S-expression navigation.")

(dolist (cmd '(forward-sexp backward-sexp backward-up-list down-list
							forward-list backward-list beginning-of-defun end-of-defun
							mark-sexp kill-sexp transpose-sexps))
  (put cmd 'repeat-map 'sexp-repeat-map))

(defvar window-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'other-window)
    (define-key map (kbd "h") #'windmove-left)
    (define-key map (kbd "j") #'windmove-down)
    (define-key map (kbd "k") #'windmove-up)
    (define-key map (kbd "l") #'windmove-right)
    (define-key map (kbd "=") #'balance-windows)
    (define-key map (kbd "+") #'enlarge-window)
    (define-key map (kbd "-") #'shrink-window)
    (define-key map (kbd ">") #'enlarge-window-horizontally)
    (define-key map (kbd "<") #'shrink-window-horizontally)
    (define-key map (kbd "0") #'delete-window)
    (define-key map (kbd "1") #'delete-other-windows)
    (define-key map (kbd "2") #'split-window-below)
    (define-key map (kbd "3") #'split-window-right)
    map)
  "Repeat map for window operations.")

(dolist (cmd '(other-window windmove-left windmove-down windmove-up windmove-right
							balance-windows enlarge-window shrink-window
							enlarge-window-horizontally shrink-window-horizontally
							delete-window delete-other-windows
							split-window-below split-window-right))
  (put cmd 'repeat-map 'window-repeat-map))

(defvar buffer-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<right>") #'next-buffer)
    (define-key map (kbd "<left>")  #'previous-buffer)
    map)
  "Repeat map for buffer cycling.")

(put 'next-buffer 'repeat-map 'buffer-repeat-map)
(put 'previous-buffer 'repeat-map 'buffer-repeat-map)

(defvar xref-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ",") #'my-go-back)
    (define-key map (kbd ".") #'xref-find-definitions)
    map)
  "Repeat map for xref navigation.")

(put 'my-go-back 'repeat-map 'xref-repeat-map)
(put 'xref-find-definitions 'repeat-map 'xref-repeat-map)

(defvar page-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'scroll-up-command)
    (define-key map (kbd "V") #'scroll-down-command)
    map)
  "Repeat map for page scrolling.")

(put 'scroll-up-command 'repeat-map 'page-repeat-map)
(put 'scroll-down-command 'repeat-map 'page-repeat-map)

(setq repeat-exit-timeout 3)

(use-package expreg
  :ensure nil
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract))
  :config
  (defvar expreg-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "=") #'expreg-expand)
      (define-key map (kbd "-") #'expreg-contract)
      map)
    "Repeat map for expreg.")
  (put 'expreg-expand 'repeat-map 'expreg-repeat-map)
  (put 'expreg-contract 'repeat-map 'expreg-repeat-map))

(use-package avy
  :ensure nil
  :bind
  ("C-;" . avy-goto-char-2)
  ("C-c C-j" . avy-resume))

(use-package pdf-tools
  :ensure nil
  :mode ("\\.pdf\\'" . pdf-view-mode))

(use-package envrc
  :ensure nil
  :diminish envrc-mode
  :hook (after-init . envrc-global-mode))

(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-default-method "ssh"
        remote-file-name-inhibit-locks t
        tramp-verbose 1))

(use-package helpful
  :ensure nil
  :bind
  ([remap describe-command]  . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-symbol]   . helpful-symbol)
  ([remap describe-variable] . helpful-variable))

(use-package editorconfig
  :ensure nil
  :diminish
  :config (editorconfig-mode 1))

(use-package diminish
  :ensure nil
  :config (diminish 'line-number-mode))

(use-package xclip
  :ensure nil
  :defer t
  :hook (after-init . xclip-mode))

(use-package popper
  :ensure nil
  :bind (("M-o"   . popper-toggle)
         ("M-O"   . popper-cycle)
         ("C-M-o" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*Warnings\\*"
     "\\*Compile-Log\\*"
     "\\*compilation\\*"
     "\\*eldoc.*\\*"
     "\\*Flymake diagnostics\\*"
     "\\*xref\\*"
     "\\*Backtrace\\*"
     "\\*e?shell\\*"
     "\\*eat\\*"
     "\\*term\\*"
     "\\*vterm\\*"
     "\\*SQL.*\\*"
     help-mode
     helpful-mode
     compilation-mode
     flymake-diagnostics-buffer-mode
     messages-buffer-mode))
  (popper-display-control t)
  (popper-window-height 0.33)
  (popper-group-function #'popper-group-by-project)
  :init
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package eshell
  :ensure nil
  :init
  (defun my/setup-eshell ()
    (keymap-set eshell-mode-map "C-r" 'consult-history)
    (setq eshell-hist-ignoredups t
          eshell-scroll-to-bottom-on-input t)
    (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
    (setq eshell-prompt-function
          (lambda ()
            (concat (abbreviate-file-name (eshell/pwd))
                    (if (= (user-uid) 0) " # " " $ ")))))
  :hook
  ((eshell-mode . my/setup-eshell)
   (eshell-mode . (lambda () (setenv "TERM" "xterm-256color")))))

(use-package eat
  :ensure nil
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)
  :hook
  ((eshell-mode . eat-eshell-mode)
   (eshell-mode . eat-eshell-visual-command-mode)))

(use-package elfeed
  :ensure nil
  :custom
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)))

(use-package elfeed-org
  :ensure nil
  :config (elfeed-org)
  :custom
  (rmh-elfeed-org-files
   (list (expand-file-name "elfeed.org" user-emacs-directory))))

(use-package elfeed-tube
  :ensure nil
  :after elfeed
  :config (elfeed-tube-setup))

(defvar my-anthropic-api-key nil
  "Cached Anthropic API key for current session.")

(defun my-get-anthropic-key ()
  "Get Anthropic API key, prompting if not cached."
  (unless my-anthropic-api-key
    (setq my-anthropic-api-key (read-passwd "Enter Anthropic API key: ")))
  my-anthropic-api-key)

(use-package gptel
  :ensure nil
  :config
  (setq gptel-default-mode 'org-mode)

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(qwen3:4b))

  (setq gptel-model "claude-sonnet-4-20250514")
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key #'my-get-anthropic-key
          :models '(claude-sonnet-4-20250514
                    claude-opus-4-20250514
                    claude-haiku-4-20250119)))

  ;; Context / RAG
  (setq gptel-track-media t)

  :bind
  (("C-c g g" . gptel)
   ("C-c g s" . gptel-send)
   ("C-c g m" . gptel-menu)
   ("C-c g a" . gptel-add)
   ("C-c g f" . gptel-add-file)))

(use-package tabspaces
  :ensure nil
  :hook (after-init . tabspaces-mode)
  :config
  (with-eval-after-load 'consult
	(plist-put consult-source-buffer :hidden t)
	(plist-put consult-source-buffer :default nil)
	(defvar consult--source-workspace
      (list :name     "Workspace Buffers"
			:narrow   ?w
			:history  'buffer-name-history
			:category 'buffer
			:state    #'consult--buffer-state
			:default  t
			:items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name))))
	(add-to-list 'consult-buffer-sources 'consult--source-workspace))
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  (tabspaces-initialize-project-with-todo nil)
  :bind
  (("C-c TAB TAB" . tabspaces-switch-or-create-workspace)
   ("C-c TAB b"   . tabspaces-switch-to-buffer)
   ("C-c TAB d"   . tabspaces-close-workspace)
   ("C-c TAB k"   . tabspaces-kill-buffers-close-workspace)))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.
Deactivates the mark, closes completion, aborts minibuffer, or quits."
  (interactive)
  (cond
   ((region-active-p)
    (deactivate-mark))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(defun my-project-find-file ()
  "Find file in current project, fallback to consult-find."
  (interactive)
  (if (and (fboundp 'project-current) (project-current))
      (call-interactively #'project-find-file)
    (if (fboundp 'consult-find)
        (consult-find default-directory)
      (call-interactively #'find-file))))

(defun my-project-find-buffer ()
  "Project-aware buffer switch, fallback to consult-buffer."
  (interactive)
  (if (and (fboundp 'project-current) (project-current))
      (consult-project-buffer)
    (call-interactively #'consult-buffer)))

(defvar my-file-keymap (make-sparse-keymap) "Keymap for file operations.")
(defvar my-window-keymap (make-sparse-keymap) "Keymap for window operations.")
(defvar my-code-keymap (make-sparse-keymap) "Keymap for code operations.")

;; Bind prefix keys under C-c
(global-set-key (kbd "C-c f") my-file-keymap)
(global-set-key (kbd "C-c w") my-window-keymap)
(global-set-key (kbd "C-c s") search-map)
(global-set-key (kbd "C-c p") project-prefix-map)
(global-set-key (kbd "C-c l") my-code-keymap)

  ;;; Files — C-c f ...
(define-key my-file-keymap (kbd "a") #'my-project-find-file)
(define-key my-file-keymap (kbd "b") #'my-project-find-buffer)
(define-key my-file-keymap (kbd "r") #'consult-recent-file)
(define-key my-file-keymap (kbd "s") #'save-buffer)

  ;;; Windows — C-c w ...
(define-key my-window-keymap (kbd "v") #'split-window-right)
(define-key my-window-keymap (kbd "s") #'split-window-below)
(define-key my-window-keymap (kbd "h") #'windmove-left)
(define-key my-window-keymap (kbd "j") #'windmove-down)
(define-key my-window-keymap (kbd "k") #'windmove-up)
(define-key my-window-keymap (kbd "l") #'windmove-right)
(define-key my-window-keymap (kbd "q") #'delete-window)
(define-key my-window-keymap (kbd "o") #'delete-other-windows)
(define-key my-window-keymap (kbd "m") #'delete-other-windows)
(define-key my-window-keymap (kbd "u") #'winner-undo)
(define-key my-window-keymap (kbd "r") #'winner-redo)

  ;;; Search — C-c s ... (uses built-in search-map)
(define-key search-map (kbd "l") #'consult-line)
(define-key search-map (kbd "G") #'consult-ripgrep)
(define-key search-map (kbd "g") #'consult-grep)
(define-key search-map (kbd "o") #'consult-outline)
(define-key search-map (kbd "i") #'consult-imenu)
(define-key search-map (kbd "m") #'consult-mark)
(define-key search-map (kbd "M") #'consult-global-mark)

  ;;; Code — C-c l ...
(define-key my-code-keymap (kbd "d") #'xref-find-definitions)
(define-key my-code-keymap (kbd "r") #'xref-find-references)
(define-key my-code-keymap (kbd "a") #'eglot-code-actions)
(define-key my-code-keymap (kbd "h") #'display-local-help)
(define-key my-code-keymap (kbd "o") #'eglot-code-action-organize-imports)
(define-key my-code-keymap (kbd "R") #'eglot-rename)

  ;;; Project shells & compile — C-c t ...
(defvar my-term-keymap (make-sparse-keymap) "Keymap for terminal/compile.")
(global-set-key (kbd "C-c t") my-term-keymap)

(defun my-project-eat ()
  "Open eat in current project root."
  (interactive)
  (let ((default-directory (or (and (project-current) (project-root (project-current)))
                               default-directory)))
    (eat)))

(defun my-project-compile ()
  "Compile in current project root."
  (interactive)
  (let ((default-directory (or (and (project-current) (project-root (project-current)))
                               default-directory)))
    (call-interactively #'compile)))

(defun my-project-sql ()
  "Open SQL interactive in current project."
  (interactive)
  (let ((default-directory (or (and (project-current) (project-root (project-current)))
                               default-directory)))
    (call-interactively #'sql-connect)))

(define-key my-term-keymap (kbd "t") #'my-project-eat)
(define-key my-term-keymap (kbd "c") #'my-project-compile)
(define-key my-term-keymap (kbd "r") #'my-project-compile)  ;; recompile alias
(define-key my-term-keymap (kbd "s") #'my-project-sql)
(define-key my-term-keymap (kbd "e") #'project-eshell)

;; Better quit
(global-set-key (kbd "C-g") #'prot/keyboard-quit-dwim)

;; GPTel bindings under C-c g
(global-set-key (kbd "C-c g g") #'gptel)
(global-set-key (kbd "C-c g s") #'gptel-send)
(global-set-key (kbd "C-c g m") #'gptel-menu)
(global-set-key (kbd "C-c g a") #'gptel-add)
(global-set-key (kbd "C-c g f") #'gptel-add-file)

;; Org capture
(global-set-key (kbd "C-c c") #'org-capture)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c f" "files"
    "C-c w" "windows"
    "C-c s" "search"
    "C-c p" "project"
    "C-c l" "code"
    "C-c g" "gptel/llm"
    "C-c TAB" "workspaces"))

(provide 'init)
;;; init.el ends here
