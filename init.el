;;; init.el --- Main configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Tangled from README.org — do not edit by hand.
;; Based on Emacs-Kick by Rahul Martim Juliato, adapted for NixOS + IGC + Sway.
;;; Code:

(setq package-archives nil)
(setq package-enable-at-startup nil)

(require 'package)
;; Activates Nix-provided packages and loads their autoloads.
;; Required: package-enable-at-startup is nil, so nothing else does this.
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
  (auto-revert-use-notify nil)
  (help-window-select t)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (inhibit-startup-message t)
  (make-backup-files nil)
  (ring-bell-function 'ignore)
  (split-width-threshold 170)
  (tab-width 4)
  (treesit-font-lock-level 4)
  (use-short-answers t)
  ;; Keep `ispell-completion-at-point' out of text/org buffers: it loads a
  ;; dictionary and can block completion.
  (text-mode-ispell-word-completion nil)
  (redisplay-skip-fontification-on-input t)
  (bidi-paragraph-direction 'left-to-right)
  (bidi-inhibit-bpa t)
  (cursor-in-non-selected-windows nil)
  (save-interprogram-paste-before-kill t)
  (highlight-nonselected-windows nil)
  (kill-do-not-save-duplicates t)
  (tab-always-indent 'complete)
  (savehist-additional-variables
   '(search-ring regexp-search-ring kill-ring))
  (window-combination-resize t)
  (set-mark-command-repeat-pop t)
  :hook
  (prog-mode . display-line-numbers-mode)

  :config
  (let ((mono-font "FiraCode Nerd Font")
        (sans-font "DejaVu Sans"))
    (set-face-attribute 'default nil :family mono-font :weight 'medium :height 160)
    (set-face-attribute 'fixed-pitch nil :family mono-font :weight 'medium :height 160)
    (set-face-attribute 'variable-pitch nil :family sans-font :height 160))

  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
  (modify-coding-system-alist 'file "" 'utf-8)

  (add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p)

  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when scroll-bar-mode (scroll-bar-mode -1))
  (column-number-mode 1)
  (delete-selection-mode 1)
  (pixel-scroll-precision-mode 1)
  (blink-cursor-mode -1)
  ;; (global-hl-line-mode 1)
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode)
  (global-auto-revert-mode 1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode 1)
  (repeat-mode 1)
  (file-name-shadow-mode 1)

  (setq compilation-scroll-output t)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

  (add-hook 'after-init-hook
            (lambda ()
              (with-current-buffer (get-buffer-create "*scratch*")
                (insert (format ";;    Welcome to Emacs!\n;;\n;;    Loading time : %s\n"
                                (emacs-init-time)))))))

(use-package gnus
  :ensure nil
  :defer t
  :config
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nnimap "gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl)
                  (nnir-search-engine imap)
                  (nnimap-authinfo-file "~/.authinfo.gpg"))))
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls)
  (setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
        gnus-use-cache t
        gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject))

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
  :config
  (defvar flymake-repeat-map
	(let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'flymake-goto-next-error)
      (define-key map (kbd "p") #'flymake-goto-prev-error)
      map)
	"Repeat map for Flymake error navigation.")
  (put 'flymake-goto-next-error 'repeat-map 'flymake-repeat-map)
  (put 'flymake-goto-prev-error 'repeat-map 'flymake-repeat-map)
  (defun consult-flymake-project ()
    "Jump to Flymake diagnostic in project."
    (interactive)
    (consult-flymake t))
  :bind (:map flymake-mode-map
              ("C-c e e" . consult-flymake)
              ("C-c e l" . consult-flymake-project)
              ("C-c e n" . flymake-goto-next-error)
              ("C-c e p" . flymake-goto-prev-error)))

(defvar my/literate-config-file
  (expand-file-name "README.org" "~/.emacs.d/")
  "The org file this configuration is tangled from.")

(defun my/literate-config-buffer-p ()
  "Return non-nil if the current buffer visits `my/literate-config-file'."
  (and buffer-file-name
       (file-equal-p buffer-file-name my/literate-config-file)))

(defun my/org-babel-tangle-config ()
  "Tangle the literate config when it is saved."
  (when (my/literate-config-buffer-p)
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)
      (message "Tangled %s" (file-name-nondirectory my/literate-config-file)))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'my/org-babel-tangle-config nil t)))

(use-package org
  :ensure nil
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((sqlite . t)
			 (sql . t))))
  :custom
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-return-follows-link t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-directory "~/Sync/personal/")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-confirm-babel-evaluate
   (lambda (lang _body)
     (not (and (equal lang "emacs-lisp")
               (my/literate-config-buffer-p)))))
  :config
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4
							  org-level-5 org-level-6 org-level-7 org-level-8))
	(set-face-attribute face nil :weight 'bold))
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
(completion-category-overrides '((file (styles partial-completion))))
;; Emacs 31+: make partial-completion behave like substring for paths.
(completion-pcm-leading-wildcard t)
:config
(with-eval-after-load 'eglot
  (setq completion-category-overrides
    '((file (styles partial-completion))
      (eglot (styles orderless))
      (eglot-capf (styles orderless))))))

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
  :after (embark consult))

(use-package corfu
  :ensure nil
  :custom
  (corfu-auto nil)
  ;; (corfu-auto-delay 0.1)
  ;; (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-on-exact-match nil)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  :bind (:map corfu-map
              ("M-q" . corfu-quick-complete)
              ("C-q" . corfu-quick-insert))
  :init
  (global-corfu-mode)
  :config
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package cape
:ensure nil
:init
(add-hook 'emacs-lisp-mode-hook (lambda ()
  (add-to-list 'completion-at-point-functions #'cape-elisp-block 'append)))
(add-hook 'org-mode-hook (lambda ()
  (add-to-list 'completion-at-point-functions #'cape-elisp-block 'append)))
:config
;; A plain list, not `cape-capf-super'. Supering merges every candidate into
;; one set sorted by Corfu, which discards the `sortText' relevance ranking
;; the language server sends. In a list, Capfs are tried in order and the
;; first non-nil one answers, so Eglot keeps its own ordering.
;;
;; Eglot goes first and `tempel-expand' second, never `tempel-complete':
;; `tempel--prefix-bounds' falls back to `bounds-of-thing-at-point' when the
;; text at point matches no template, so `tempel-complete' returns every
;; template at nearly any symbol and buries the LSP candidates.
;; `tempel-expand' requires an exact template name, so it stays quiet.
;; Use `M-+' to browse templates on demand.
;;
;; Eglot is marked non-exclusive so completion still falls through to
;; `tempel-expand' and `cape-file' when the server returns no match.
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-nonexclusive #'eglot-completion-at-point)
                              #'tempel-expand
                              #'cape-file))))
(add-to-list 'completion-at-point-functions #'cape-file 'append))

(use-package catppuccin-theme
  :ensure nil
  :custom
  (catppuccin-flavor 'frappe)
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package ligature
  :ensure nil
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
						  '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
							;; =:= =!=
							("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
							;; ;; ;;;
							(";" (rx (+ ";")))
							;; && &&&
							("&" (rx (+ "&")))
							;; !! !!! !. !: !!. != !== !~
							("!" (rx (+ (or "=" "!" "\." ":" "~"))))
							;; ?? ??? ?:  ?=  ?.
							("?" (rx (or ":" "=" "\." (+ "?"))))
							;; %% %%%
							("%" (rx (+ "%")))
							;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
							;; |->>-||-<<-| |- |== ||=||
							;; |==>>==<<==<=>==//==/=!==:===>
							("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
											"-" "=" ))))
							;; \\ \\\ \/
							("\\" (rx (or "/" (+ "\\"))))
							;; ++ +++ ++++ +>
							("+" (rx (or ">" (+ "+"))))
							;; :: ::: :::: :> :< := :// ::=
							(":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
							;; // /// //// /\ /* /> /===:===!=//===>>==>==/
							("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
											"="))))
							;; .. ... .... .= .- .? ..= ..<
							("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
							;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
							("-" (rx (+ (or ">" "<" "|" "~" "-"))))
							;; *> */ *)  ** *** ****
							("*" (rx (or ">" "/" ")" (+ "*"))))
							;; www wwww
							("w" (rx (+ "w")))
							;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
							;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
							;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
							;; << <<< <<<<
							("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
											"-"  "/" "|" "="))))
							;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
							;; >> >>> >>>>
							(">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
							;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
							("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
										 (+ "#"))))
							;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
							("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
							;; __ ___ ____ _|_ __|____|_
							("_" (rx (+ (or "_" "|"))))
							;; Fira code: 0xFF 0x12
							("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
							;; Fira code:
							"Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
							;; The few not covered by the regexps.
							"{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

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

(defun my/ml-flymake ()
  "Flymake error/warning counts for the mode-line."
  (when (bound-and-true-p flymake-mode)
    (let* ((diags (flymake-diagnostics))
           (errors   (cl-count :error   diags :key #'flymake-diagnostic-type))
           (warnings (cl-count :warning diags :key #'flymake-diagnostic-type)))
      (concat
       (when (> errors 0)
         (propertize (format " !%d" errors) 'face '(:foreground "#e57373")))
       (when (> warnings 0)
         (propertize (format " ⚠%d" warnings) 'face '(:foreground "#ffb74d")))
       (when (= (+ errors warnings) 0)
         (propertize " ✓" 'face '(:foreground "#81c784")))))))

(defvar-local my/ml-flymake-cache "")

(defun my/ml-flymake-update ()
  "Cache flymake status string."
  (setq my/ml-flymake-cache
        (if (bound-and-true-p flymake-mode)
            (let* ((diags (flymake-diagnostics))
                   (errors (cl-count :error diags :key #'flymake-diagnostic-type))
                   (warnings (cl-count :warning diags :key #'flymake-diagnostic-type)))
              (cond
               ((> errors 0) (propertize (format " !%d" errors) 'face '(:foreground "#e57373")))
               ((> warnings 0) (propertize (format " ⚠%d" warnings) 'face '(:foreground "#ffb74d")))
               (t (propertize " ✓" 'face '(:foreground "#81c784")))))
          "")))

(add-hook 'flymake-after-diagnostics-hook #'my/ml-flymake-update)

(setq-default mode-line-format
  '(" "
    (:eval (if (buffer-modified-p)
               (propertize "●" 'face '(:foreground "#ffb74d"))
             " "))
    " %b "
    "%l:%C  "
    (:eval (my/ml-flymake))
    "  "
    mode-line-end-spaces))

(add-hook 'flymake-after-diagnostics-hook #'force-mode-line-update)

(setq treesit-font-lock-level 4)
(add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
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
  :defer t
  :init
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
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
  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript"))
                 "deno" "lsp"
                 :initializationOptions (:enable t :lint t :unstable t)))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd"))))

(use-package consult-eglot
  :ensure nil
  :after eglot
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)))

(use-package flycheck
  :ensure nil
  :defer t
  :commands (flycheck-mode global-flycheck-mode)
  :config
  ;; Error Lens-style tint across the whole line, not just the message.
  ;; (setq flycheck-annotate-background t)
  (global-flycheck-annotate-mode 1)
  ;; Eglot keeps doing the LSP work; Flycheck renders the diagnostics.
  (global-flycheck-eglot-mode 1))

(defun my/use-flycheck ()
  "Hand diagnostics over to Flycheck, disabling Flymake."
  (interactive)
  (remove-hook 'prog-mode-hook #'flymake-mode)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p flymake-mode)
        (flymake-mode -1))))
  (global-flycheck-mode 1)
  (message "Diagnostics: Flycheck"))

(defun my/use-flymake ()
  "Hand diagnostics back to Flymake, disabling Flycheck."
  (interactive)
  (when (bound-and-true-p global-flycheck-mode)
    (global-flycheck-mode -1))
  (add-hook 'prog-mode-hook #'flymake-mode)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'prog-mode)
        (flymake-mode 1))))
  (message "Diagnostics: Flymake"))

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

(use-package tempel-collection
  :ensure nil
  :after tempel)

(use-package eglot-tempel
  :ensure nil
  :after tempel
  :init (eglot-tempel-mode t))

(use-package apheleia
  :ensure nil
  :diminish
  :config
  (apheleia-global-mode 1)
  (add-to-list 'apheleia-formatters
               '(templ-format "go" "tool" "templ" "fmt" filepath))
  (add-to-list 'apheleia-formatters
               '(deno-format "deno" "fmt" "--ext" "ts" "-"))
  (setf (alist-get 'nixfmt-rfc-style apheleia-formatters)
        '("nixfmt"))
  (setf (alist-get 'nix-mode apheleia-mode-alist)
        'nixfmt-rfc-style)
  (setf (alist-get 'golangci-fmt apheleia-formatters)
		'("golangci-lint" "fmt" "--stdin" filepath))
  (setf (alist-get 'go-mode apheleia-mode-alist) 'golangci-fmt)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'golangci-fmt)
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
  (indent-bars-highlight-current-depth nil)
  ;; Throttle tree-sitter context queries while navigating
  (indent-bars-treesit-update-delay 0.1))

(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
   (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
   (after-init . dape-breakpoint-load)

  :custom
  ;; Info buffers to the right
  ;; (dape-buffer-window-arrangement 'right)
  ;; Info buffers like gud (gdb-mi)
   (dape-buffer-window-arrangement 'gud)
   (dape-info-hide-mode-line nil)

  ;; Projectile users
  ;; (dape-cwd-function #'projectile-project-root)

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
   (dape-breakpoint-global-mode 1)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  ;; Save buffers on startup, useful for interpreted languages
   (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
   (add-hook 'dape-compile-hook #'kill-buffer)
  )

;; For a more ergonomic Emacs and `dape' experience
(use-package repeat
  :init
  (repeat-mode 1))

;; Left and right side windows occupy full frame height
(use-package emacs
  :custom
  (window-sides-vertical t))

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
  :config (setq geiser-default-implementation 'guile))

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
  ((after-init             . global-diff-hl-mode)
   (magit-post-refresh     . diff-hl-magit-post-refresh)
   (after-save             . diff-hl-update)
   (dired-mode             . diff-hl-dired-mode))
  :config
  (diff-hl-margin-mode)
  :custom
  (diff-hl-side 'left)
  (diff-hl-disable-on-remote t))

(use-package undo-fu
  :ensure nil
  :commands (undo-fu-only-undo undo-fu-only-redo)
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session
  :ensure nil
  :config
  (setq undo-fu-session-directory
        (expand-file-name "undo-fu-session" user-emacs-directory))
  (unless (file-exists-p undo-fu-session-directory)
    (make-directory undo-fu-session-directory t))
  (setq undo-fu-session-incompatible-files
        '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

(use-package vundo
  :ensure nil
  :bind ("C-x u" . vundo))

(defun my-smarter-move-beginning-of-line (arg)
  "Move to first non-whitespace char, or column 0 if already there."
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

(defun my-indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-a")       #'my-smarter-move-beginning-of-line)
(global-set-key (kbd "C-c d")     #'my-duplicate-line)
(global-set-key (kbd "M-<up>")    #'my-move-line-up)
(global-set-key (kbd "M-<down>")  #'my-move-line-down)
(global-set-key (kbd "C-c i")     #'my-indent-buffer)

(defun my-comment-or-uncomment ()
  "Toggle comment on current line or region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(global-set-key (kbd "C-c ;") #'my-comment-or-uncomment)

(defvar my-auto-center-commands
  '(xref-find-definitions
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
(advice-add 'save-place-find-file-hook :after
			(lambda (&rest _)
              (when buffer-file-name (ignore-errors (recenter)))))

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
    (define-key map (kbd ".") #'xref-find-definitions)
    (define-key map (kbd ",") #'xref-go-back)
    map)
  "Repeat map for xref navigation.")

(put 'xref-find-definitions 'repeat-map 'xref-repeat-map)
(put 'xref-go-back 'repeat-map 'xref-repeat-map)

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

(use-package surround
  :ensure nil
  :bind-keymap ("M-'" . surround-keymap))

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
   '("\\*compilation\\*"
     "\\*eat\\*"
     compilation-mode))
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

(use-package ghostel
  :ensure nil
  :bind (("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-s" . consult-line)
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))

(use-package ghostel-eshell
  :ensure nil
  :after ghostel
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

(use-package ghostel-compile
  :ensure nil
  :after ghostel
  :hook (after-init . ghostel-compile-global-mode))

(use-package elfeed
  :ensure nil
  :defer t
  :custom
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)))

(use-package elfeed-org
  :ensure nil
  :after elfeed
  :config (elfeed-org)
  :custom
  (rmh-elfeed-org-files
   (list (expand-file-name "elfeed.org" user-emacs-directory))))

(use-package elfeed-tube
  :ensure nil
  :after elfeed
  :config (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
			  ("F" . elfeed-tube-fetch)
			  ([remap save-buffer] . elfeed-tube-save)
			  :map elfeed-search-mode-map
			  ("F" . elfeed-tube-fetch)
			  ([remap save-buffer] . elfeed-tube-save)))

(require 'auth-source)

(defvar my-gptel--key-cache nil
  "Alist of API keys entered by hand this session, keyed by host.")

(defun my-gptel-key (host env-var)
  "Return the API key for HOST.
Search `auth-sources' for HOST first, then the ENV-VAR environment
variable, and only then prompt — caching the answer for the session."
  (or (auth-source-pick-first-password :host host :user "apikey")
      (getenv env-var)
      (alist-get host my-gptel--key-cache nil nil #'equal)
      (setf (alist-get host my-gptel--key-cache nil nil #'equal)
            (read-passwd (format "%s API key: " host)))))

(defun my-gptel-openrouter-key ()
  "Return the OpenRouter API key."
  (my-gptel-key "openrouter.ai" "OPENROUTER_API_KEY"))

(defun my-gptel-litellm-key ()
  "Return the master key of the local LiteLLM proxy."
  (my-gptel-key "litellm" "LITELLM_MASTER_KEY"))

(defvar my-gptel-openrouter-models
  '(anthropic/claude-sonnet-4.5
    anthropic/claude-opus-4.1
    anthropic/claude-haiku-4.5
    openai/gpt-5.1
    openai/gpt-5-mini
    google/gemini-2.5-pro
    google/gemini-2.5-flash
    deepseek/deepseek-chat-v3.1
    qwen/qwen3-coder
    moonshotai/kimi-k2)
  "Shortlist of models offered by the OpenRouter backend.
OpenRouter carries hundreds of models and renames them often, so this
is a hand-picked subset that will drift.  Run
`my-gptel-openrouter-refresh-models' to replace it with the live
catalogue.")

(defvar my-gptel-litellm-models
  '(deepseek-chat deepseek-reasoner sonnet)
  "Model aliases served by the local LiteLLM proxy.
These are the `model_name' values from the proxy's own config.yaml,
not upstream model names — LiteLLM maps one to the other.  Run
`my-gptel-litellm-refresh-models' to sync this list with the running
proxy.")

(defvar my-gptel-openrouter nil
  "The OpenRouter gptel backend.")

(defvar my-gptel-llama-cpp nil
  "The local llama.cpp gptel backend.")

(defvar my-gptel-litellm nil
  "The gptel backend for a local LiteLLM proxy.")

(use-package gptel
  :ensure nil
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-track-media t)

  ;; Local model: no key, no network, no cost.  Reachable from the
  ;; gptel menu or with the `local' preset.
  (setq my-gptel-llama-cpp
        (gptel-make-openai "llama.cpp"
          :host "localhost:8999"
          :protocol "http"
          :endpoint "/v1/chat/completions"
          :stream t
          :key "dummy"
          :models '(qwen3.5-9b)))

  ;; OpenRouter speaks the OpenAI API, so `gptel-make-openai' is the
  ;; right constructor — one key for every hosted model.
  (setq my-gptel-openrouter
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key #'my-gptel-openrouter-key
          :models my-gptel-openrouter-models))

  ;; Optional LiteLLM proxy: one endpoint in front of DeepSeek and
  ;; anything else worth routing directly, with its own key store and
  ;; spend caps.  Registering it is free when the proxy is not running
  ;; — nothing connects until the backend is selected.
  (setq my-gptel-litellm
        (gptel-make-openai "LiteLLM"
          :host "localhost:4000"
          :protocol "http"
          :endpoint "/v1/chat/completions"
          :stream t
          :key #'my-gptel-litellm-key
          :models my-gptel-litellm-models))

  (setq gptel-backend my-gptel-openrouter)
  (setq gptel-model 'anthropic/claude-sonnet-4.5)

  :bind
  (("C-c g g" . gptel)
   ("C-c g s" . gptel-send)
   ("C-c g m" . gptel-menu)
   ("C-c g a" . gptel-add)
   ("C-c g f" . gptel-add-file)
   ("C-c g r" . gptel-rewrite)
   ("C-c g k" . gptel-abort)))

(defun my-gptel--fetch-models (url &optional key)
  "Return the model ids advertised at URL as a list of symbols.
URL must be an OpenAI-style /models endpoint.  KEY, when non-nil, is
sent as a bearer token."
  (let* ((url-request-extra-headers
          (and key (list (cons "Authorization" (concat "Bearer " key)))))
         (buf (url-retrieve-synchronously url t nil 30)))
    (unless buf
      (user-error "No response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (unless (search-forward "\n\n" nil t)
            (user-error "Malformed response from %s" url))
          (let ((payload (json-parse-buffer :object-type 'plist
                                            :array-type 'list)))
            (or (mapcar (lambda (m) (intern (plist-get m :id)))
                        (plist-get payload :data))
                (user-error "No models listed at %s" url))))
      (kill-buffer buf))))

(defun my-gptel-openrouter-refresh-models ()
  "Replace the OpenRouter backend's model list with the live catalogue."
  (interactive)
  (let ((models (my-gptel--fetch-models
                 "https://openrouter.ai/api/v1/models")))
    (setq my-gptel-openrouter-models models)
    (setf (gptel-backend-models my-gptel-openrouter) models)
    (message "OpenRouter: %d models available" (length models))))

(defun my-gptel-litellm-refresh-models ()
  "Sync the LiteLLM backend's model list with the running proxy.
Doubles as a health check: an error here means the proxy is down, on
another port, or refusing the master key."
  (interactive)
  (let ((models (my-gptel--fetch-models "http://localhost:4000/v1/models"
                                        (my-gptel-litellm-key))))
    (setq my-gptel-litellm-models models)
    (setf (gptel-backend-models my-gptel-litellm) models)
    (message "LiteLLM: %s" (mapconcat #'symbol-name models ", "))))

(defvar my-gptel-code-directive
  "You are a programming assistant working inside Emacs on the user's project.
Read the project with your tools instead of guessing: list files, search,
and read what you need before answering.  Keep answers short, show real
code, and say plainly when you are unsure."
  "Base system message for project chats and the `code' preset.")

(defvar my-gptel-project-instruction-files
  '("AGENTS.md" ".gptel.md" "CLAUDE.md" "CONVENTIONS.md")
  "File names looked for at a project root, in order of preference.
The first one that exists is appended to `my-gptel-code-directive' to
form the system message of that project's chat buffer.")

(defun my-gptel-project-root (&optional dir)
  "Return the project root for DIR, or nil when there is no project."
  (let ((default-directory (or dir default-directory)))
    (when-let ((pr (project-current)))
      (project-root pr))))

(defun my-gptel-project-instructions (root)
  "Return (FILE . TEXT) for ROOT's instruction file, or nil if it has none."
  (when root
    (when-let ((file (seq-some (lambda (name)
                                 (let ((f (expand-file-name name root)))
                                   (and (file-readable-p f) f)))
                               my-gptel-project-instruction-files)))
      (with-temp-buffer
        (insert-file-contents file)
        (cons file (buffer-string))))))

(defun my-gptel-project-system-message (root)
  "Build the system message for the project at ROOT."
  (let ((instructions (my-gptel-project-instructions root)))
    (concat my-gptel-code-directive
            (format "\n\nProject root: %s"
                    (abbreviate-file-name (or root default-directory)))
            (when instructions
              (format "\n\nProject instructions from %s:\n\n%s"
                      (file-name-nondirectory (car instructions))
                      (cdr instructions))))))

(defun my-gptel-set-system-message (text)
  "Set TEXT as the system message of the current buffer.
`gptel-system-prompt' was called `gptel--system-message' before gptel
0.9.9.6, so honour both — the Nix pin decides which one exists."
  (if (boundp 'gptel-system-prompt)
      (setq-local gptel-system-prompt text)
    (setq-local gptel--system-message text)))

(defun my-gptel-project-reload ()
  "Re-read the project instruction file into this buffer's system message."
  (interactive)
  (let* ((root (my-gptel-project-root))
         (instructions (my-gptel-project-instructions root)))
    (my-gptel-set-system-message (my-gptel-project-system-message root))
    (message "gptel: context reloaded for %s%s"
             (abbreviate-file-name (or root default-directory))
             (if instructions
                 (format " (%s)" (file-name-nondirectory (car instructions)))
               " (no instruction file)"))))

(defun my-gptel-project ()
  "Open a gptel chat buffer dedicated to the current project.
The buffer's `default-directory' is the project root — so tools run
there — and its system message carries the project's instruction file
if it has one.  Reuses the buffer on subsequent calls."
  (interactive)
  (let* ((pr (project-current))
         (root (if pr (project-root pr) default-directory))
         (name (if pr (project-name pr) "default"))
         (buf-name (format "*gptel: %s*" name))
         (fresh (not (get-buffer buf-name)))
         (buf (gptel buf-name)))
    (with-current-buffer buf
      (setq default-directory root)
      (when fresh (my-gptel-project-reload)))
    (pop-to-buffer buf)))

(defun my-gptel-add-project-file (file)
  "Add FILE, completed from the current project's files, to gptel's context."
  (interactive
   (let ((pr (or (project-current)
                 (user-error "Not inside a project"))))
     (list (completing-read "Add to gptel context: "
                            (project-files pr) nil t))))
  (gptel-add-file file))

(with-eval-after-load 'gptel

  (setq gptel-use-tools t)

  ;; Read buffer contents
  (gptel-make-tool
   :name "read_buffer"
   :function (lambda (buffer)
               (unless (buffer-live-p (get-buffer buffer))
				 (error "Error: buffer %s is not live." buffer))
               (with-current-buffer buffer
				 (buffer-substring-no-properties (point-min) (point-max))))
   :description "Return the contents of an Emacs buffer"
   :args (list '(:name "buffer"
					   :type string
					   :description "The name of the buffer whose contents are to be retrieved"))
   :category "emacs")

  ;; Read file contents
  (gptel-make-tool
   :name "read_file"
   :function (lambda (filepath)
               (with-temp-buffer
				 (insert-file-contents (expand-file-name filepath))
				 (buffer-string)))
   :description "Read and return the contents of a file"
   :args (list '(:name "filepath"
					   :type string
					   :description "Path to the file to read. Supports relative paths and ~."))
   :category "filesystem")

  ;; List directory contents
  (gptel-make-tool
   :name "list_directory"
   :function (lambda (directory)
               (mapconcat #'identity
                          (directory-files (expand-file-name directory))
                          "\n"))
   :description "List the contents of a directory"
   :args (list '(:name "directory"
					   :type string
					   :description "Path to the directory to list"))
   :category "filesystem")

  ;; List project files
  (gptel-make-tool
   :name "list_project_files"
   :function (lambda ()
               (if-let ((pr (project-current)))
                   (mapconcat #'identity (project-files pr) "\n")
				 "No project found for current buffer"))
   :description "List all files in the current project"
   :args (list)
   :category "filesystem")

  ;; Read Emacs documentation
  (gptel-make-tool
   :name "read_documentation"
   :function (lambda (symbol)
               (let ((sym (intern symbol)))
				 (cond
                  ((fboundp sym) (documentation sym))
                  ((boundp sym) (documentation-property sym 'variable-documentation))
                  (t (format "No documentation found for %s" symbol)))))
   :description "Read the documentation for a given Emacs function or variable"
   :args (list '(:name "symbol"
					   :type string
					   :description "The name of the function or variable to look up"))
   :category "emacs")

  ;; Read URL contents
  (gptel-make-tool
   :name "read_url"
   :function (lambda (url)
               (with-current-buffer (url-retrieve-synchronously url)
				 (goto-char (point-min))
				 (forward-paragraph)
				 (let ((dom (libxml-parse-html-region (point) (point-max))))
                   (run-at-time 0 nil #'kill-buffer (current-buffer))
                   (with-temp-buffer
					 (shr-insert-document dom)
					 (buffer-substring-no-properties (point-min) (point-max))))))
   :description "Fetch and read the contents of a URL"
   :args (list '(:name "url"
					   :type string
					   :description "The URL to fetch"))
   :category "web")

  ;; Run shell command (requires confirmation)
  (gptel-make-tool
   :name "run_command"
   :function (lambda (command &optional working_dir)
               (let ((default-directory (if (and working_dir
												 (not (string= working_dir "")))
											(expand-file-name working_dir)
                                          default-directory)))
				 (shell-command-to-string command)))
   :description "Run a shell command and return its output"
   :args (list '(:name "command"
					   :type string
					   :description "The shell command to run")
               '(:name "working_dir"
					   :type string
					   :description "Optional directory to run the command in"))
   :category "command"
   :confirm t
   :include t)

  ;; Append text to buffer
  (gptel-make-tool
   :name "append_to_buffer"
   :function (lambda (buffer text)
               (with-current-buffer (get-buffer-create buffer)
				 (save-excursion
                   (goto-char (point-max))
                   (insert text)))
               (format "Appended text to buffer %s" buffer))
   :description "Append text to an Emacs buffer, creating it if needed"
   :args (list '(:name "buffer"
					   :type string
					   :description "The name of the buffer to append to")
               '(:name "text"
					   :type string
					   :description "The text to append"))
   :category "emacs")

  ;; Read PDF contents
  (gptel-make-tool
   :name "read_pdf"
   :function (lambda (filepath)
               (let ((file (expand-file-name filepath)))
				 (unless (file-readable-p file)
                   (error "Error: file %s is not readable." file))
				 (pdf-info-gettext file
                                   (cons 1 (pdf-info-number-of-pages file)))))
   :description "Extract text content from a PDF file"
   :args (list '(:name "filepath"
					   :type string
					   :description "Path to the PDF file. Supports relative paths and ~."))
   :category "filesystem")

  ;; Project root, name and branch
  (gptel-make-tool
   :name "project_root"
   :function (lambda ()
               (if-let ((pr (project-current)))
                   (let* ((root (project-root pr))
                          (default-directory root)
                          (branch (ignore-errors
                                    (car (process-lines-ignore-status
                                          "git" "rev-parse" "--abbrev-ref" "HEAD")))))
                     (format "root: %s\nname: %s\nbranch: %s"
                             root (project-name pr) (or branch "unknown")))
				 "No project found for current buffer"))
   :description "Return the root directory, name and VC branch of the current project"
   :args (list)
   :category "filesystem")

  ;; Search the project with ripgrep
  (gptel-make-tool
   :name "search_project"
   :function (lambda (pattern &optional glob)
               (let* ((pr (project-current))
                      (default-directory (if pr (project-root pr) default-directory))
                      (args (append '("--line-number" "--no-heading" "--color" "never"
                                      "--max-count" "20" "--max-columns" "200")
                                    (when (and glob (not (string-empty-p glob)))
                                      (list "--glob" glob))
                                    (list "--regexp" pattern)))
                      (output (with-temp-buffer
								(let ((status (apply #'call-process "rg" nil t nil args)))
                                  (pcase status
                                    (0 (buffer-string))
                                    (1 "No matches")
                                    (_ (format "ripgrep failed: %s" (buffer-string))))))))
				 (if (> (length output) 8000)
                     (concat (substring output 0 8000)
                             "\n… output truncated, narrow the search")
                   output)))
   :description "Search the files of the current project with ripgrep, returning matching lines with their file and line number"
   :args (list '(:name "pattern"
					   :type string
					   :description "Regular expression to search for")
               '(:name "glob"
					   :type string
					   :optional t
					   :description "Optional file glob restricting the search, e.g. *.el"))
   :category "project"))

(with-eval-after-load 'gptel
  ;; Presets landed in gptel 0.9.8; skip them on an older pin rather
  ;; than breaking init.
  (when (fboundp 'gptel-make-preset)

    (gptel-make-preset 'local
      :description "Local llama.cpp model — offline and free."
      :backend "llama.cpp"
      :model 'qwen3.5-9b
      :tools nil)

    (gptel-make-preset 'quick
      :description "Cheap fast hosted model for one-off questions."
      :backend "OpenRouter"
      :model 'google/gemini-2.5-flash
      :tools nil)

    (gptel-make-preset 'cheap
      :description "DeepSeek through the local LiteLLM proxy."
      :backend "LiteLLM"
      :model 'deepseek-chat
      :system my-gptel-code-directive
      :tools '("project_root" "list_project_files" "search_project"
               "read_file" "read_buffer"))

    (gptel-make-preset 'code
      :description "Strong hosted model with project tools."
      :backend "OpenRouter"
      :model 'anthropic/claude-sonnet-4.5
      :system my-gptel-code-directive
      :tools '("project_root" "list_project_files" "search_project"
               "read_file" "read_buffer"))

    (gptel-make-preset 'reason
      :description "Slowest, strongest model for hard problems."
      :backend "OpenRouter"
      :model 'anthropic/claude-opus-4.1
      :system my-gptel-code-directive
      :tools '("project_root" "list_project_files" "search_project"
               "read_file" "read_buffer"))

    (gptel-make-preset 'explain
      :description "Explain code to someone who has not seen it."
      :system "Explain what this code does, plainly and concretely.
Name the moving parts, then walk through the flow.  No filler.")))

(use-package tabspaces
  :ensure nil
  :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-keymap-prefix "C-c TAB")
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  (tabspaces-initialize-project-with-todo nil)
  :config
  (with-eval-after-load 'consult
    (defvar consult-source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :category 'buffer
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda ()
                        (consult--buffer-query
                         :predicate #'tabspaces--local-buffer-p
                         :sort 'visibility
                         :as #'consult--buffer-pair)))
      "Tabspaces-local buffer source for `consult-buffer'.")
    (add-to-list 'consult-buffer-sources 'consult-source-workspace)
    ;; Full buffer list is still reachable via its `b' narrow key.
    (plist-put consult-source-buffer :hidden t)
    (plist-put consult-source-buffer :default nil)))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for `keyboard-quit'."
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

(defvar my-file-keymap   (make-sparse-keymap) "Keymap for file operations.")
(defvar my-window-keymap (make-sparse-keymap) "Keymap for window operations.")
(defvar my-code-keymap   (make-sparse-keymap) "Keymap for code operations.")
(defvar my-term-keymap   (make-sparse-keymap) "Keymap for terminal/compile.")

(global-set-key (kbd "C-c f") my-file-keymap)
(global-set-key (kbd "C-c w") my-window-keymap)

(global-set-key (kbd "C-c p") project-prefix-map)
(global-set-key (kbd "C-c l") my-code-keymap)
(global-set-key (kbd "C-c t") my-term-keymap)
;; C-c TAB is bound by `tabspaces-mode-map', see the Tabspaces section.

;;; Files — C-c f …
(define-key my-file-keymap (kbd "a") #'my-project-find-file)
(define-key my-file-keymap (kbd "b") #'my-project-find-buffer)
(define-key my-file-keymap (kbd "r") #'consult-recent-file)
(define-key my-file-keymap (kbd "s") #'save-buffer)

;;; Windows — C-c w …
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

;;; Search — C-c s … (uses built-in search-map)
(global-set-key (kbd "C-c s") search-map)
(define-key search-map (kbd "l") #'consult-line)
(define-key search-map (kbd "G") #'consult-ripgrep)
(define-key search-map (kbd "g") #'consult-grep)
(define-key search-map (kbd "o") #'consult-outline)
(define-key search-map (kbd "i") #'consult-imenu)
(define-key search-map (kbd "m") #'consult-mark)
(define-key search-map (kbd "M") #'consult-global-mark)

;;; Code — C-c l …
(define-key my-code-keymap (kbd "d") #'xref-find-definitions)
(define-key my-code-keymap (kbd "r") #'xref-find-references)
(define-key my-code-keymap (kbd "a") #'eglot-code-actions)
(define-key my-code-keymap (kbd "h") #'display-local-help)
(define-key my-code-keymap (kbd "o") #'eglot-code-action-organize-imports)
(define-key my-code-keymap (kbd "R") #'eglot-rename)

(defun my-project-eat ()
  "Open eat in current project root with a project-named buffer."
  (interactive)
  (let* ((pr (project-current))
		 (default-directory (if pr (project-root pr) default-directory))
		 (name (if pr (project-name pr) "default"))
		 (buf-name (format "*eat-%s*" name)))
	(if (get-buffer buf-name)
		(pop-to-buffer buf-name)
      (eat)
      (rename-buffer buf-name))))

(defun my-project-compile ()
  "Compile in current project root with a project-named buffer."
  (interactive)
  (let* ((pr (project-current))
		 (default-directory (if pr (project-root pr) default-directory))
		 (name (if pr (project-name pr) "default"))
		 (compilation-buffer-name-function
          (lambda (_mode) (format "*compilation-%s*" name))))
	(call-interactively #'compile)))

(defun my-project-sql ()
  "Open SQL interactive in current project."
  (interactive)
  (let ((default-directory (or (and (project-current) (project-root (project-current)))
                               default-directory)))
    (call-interactively #'sql-connect)))

(define-key my-term-keymap (kbd "t") #'my-project-eat)
(define-key my-term-keymap (kbd "c") #'my-project-compile)
(define-key my-term-keymap (kbd "s") #'my-project-sql)

(global-set-key (kbd "C-g") #'prot/keyboard-quit-dwim)

(global-set-key (kbd "C-c g g") #'gptel)
(global-set-key (kbd "C-c g s") #'gptel-send)
(global-set-key (kbd "C-c g m") #'gptel-menu)
(global-set-key (kbd "C-c g a") #'gptel-add)
(global-set-key (kbd "C-c g f") #'gptel-add-file)
(global-set-key (kbd "C-c g r") #'gptel-rewrite)
(global-set-key (kbd "C-c g k") #'gptel-abort)
(global-set-key (kbd "C-c g p") #'my-gptel-project)
(global-set-key (kbd "C-c g P") #'my-gptel-add-project-file)
(global-set-key (kbd "C-c g R") #'my-gptel-project-reload)

(global-set-key (kbd "C-c c") #'org-capture)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c f"   "files"
    "C-c w"   "windows"
    "C-c s"   "search"
    "C-c p"   "project"
    "C-c l"   "code/lsp"
    "C-c g"   "gptel/llm"
    "C-c t"   "terminal"
    "C-c e"   "errors/flymake"
    "C-c !"   "flycheck"
    "C-c TAB" "workspaces"
    "M-'"     "surround"))

(provide 'init)
;;; init.el ends here
