;;; init.el --- Init -*- lexical-binding: t; -*-


(setq package-archives nil)
(setq package-enable-at-startup nil)

;; Initialize package system
(require 'package)
(package-initialize)


(require 'use-package)
(setq use-package-always-ensure nil)  ; Auto-install packages


;;; Load custom-file
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/"))
(when (file-exists-p custom-file)
  (load custom-file :noerror :nomessage))

;;; Set up load paths
;; Note: user-emacs-directory is set to var/ in early-init.el
;; So we need absolute paths relative to the actual .emacs.d
(let ((emacs-root (file-name-directory
                   (directory-file-name user-emacs-directory))))
  (add-to-list 'load-path (expand-file-name "lisp" emacs-root))
  (add-to-list 'load-path (expand-file-name "modules" emacs-root))
  (add-to-list 'load-path (expand-file-name "site-lisp" emacs-root))
  
  ;; Add all subdirectories
  (let ((default-directory (expand-file-name "modules" emacs-root)))
    (when (file-directory-p default-directory)
      (normal-top-level-add-subdirs-to-load-path)))
  
  (let ((default-directory (expand-file-name "site-lisp" emacs-root)))
    (when (file-directory-p default-directory)
      (normal-top-level-add-subdirs-to-load-path))))

(setq auto-save-list-file-prefix (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory (expand-file-name "tramp-autosave/" user-emacs-directory))
(setq backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq savehist-file (expand-file-name "history" user-emacs-directory))
(setq project-list-file (expand-file-name "projects.eld" user-emacs-directory))
(setq transient-history-file (expand-file-name "transient/history.el" user-emacs-directory))
(setq transient-levels-file (expand-file-name "transient/levels.el" user-emacs-directory))
(setq transient-values-file (expand-file-name "transient/values.el" user-emacs-directory))


(setq native-comp-async-query-on-exit t)

;; Allow for shorter responses: "y" for yes and "n" for no.
(setq read-answer-short t)
(if (boundp 'use-short-answers)
	(setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;;; Minibuffer

;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
	  '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; User interface

;; By default, Emacs "updates" its ui more often than it needs to
(setq which-func-update-delay 1.0)


(defalias #'view-hello-file #'ignore)  ; Never show the hello file

;; No beeping or blinking
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;;; Show-paren

(setq show-paren-delay 0.1
	  show-paren-highlight-openparen t
	  show-paren-when-point-inside-paren t
	  show-paren-when-point-in-periphery t)

;;; Misc

(setq custom-buffer-done-kill t)

(setq whitespace-line-column nil)  ; Use the value of `fill-column'.

;; Can be activated with `display-line-numbers-mode'
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

(setq truncate-string-ellipsis "…")

;; Disable truncation of printed s-expressions in the message buffer
(setq eval-expression-print-length nil
	  eval-expression-print-level nil)

;; Position underlines at the descent line instead of the baseline.
(setq x-underline-at-descent-line t)

(setq tramp-verbose 1)
(setq tramp-completion-reread-directory-timeout 50)
(setq remote-file-name-inhibit-cache 50)

;; Automatically rescan the buffer for Imenu entries when `imenu' is invoked
;; This ensures the index reflects recent edits.
(setq imenu-auto-rescan t)

;; Prevent truncation of long function names in `imenu' listings
(setq imenu-max-item-length 160)

;; Disable auto-adding a new line at the bottom when scrolling.
(setq next-line-add-newlines nil)

;;; Files

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive))
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Ignoring this is acceptable since it will redirect to the buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resolve symlinks so that operations are conducted from the file's directory
(setq find-file-visit-truename t
	  vc-follow-symlinks t)

;; Prefer vertical splits over horizontal ones
(setq split-width-threshold 170
	  split-height-threshold nil)

;;; Buffers

(setq uniquify-buffer-name-style 'forward)

;;; comint (general command interpreter in a window)

(setq ansi-color-for-comint-mode t
	  comint-prompt-read-only t
	  comint-buffer-maximum-size 4096)

;;; Compilation

(setq compilation-ask-about-save nil
	  compilation-always-kill t
	  compilation-scroll-output 'first-error)

;; Skip confirmation prompts when creating a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)

;;; Backup files

;; Avoid backups or lockfiles to prevent creating world-readable copies of files
(setq create-lockfiles nil)
(setq make-backup-files nil)

(setq backup-directory-alist
	  `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq backup-by-copying-when-linked t)
(setq backup-by-copying t)  ; Backup by copying rather renaming
(setq delete-old-versions t)  ; Delete excess backup versions silently
(setq version-control t)  ; Use version numbers for backup files
(setq kept-new-versions 5)
(setq kept-old-versions 5)

;;; VC

(setq vc-git-print-log-follow t)
(setq vc-make-backup-files nil)  ; Do not backup version controlled files
(setq vc-git-diff-switches '("--histogram"))  ; Faster algorithm for diffing.

;;; Auto save




(setq auto-save-include-big-deletions t)

(setq auto-save-list-file-prefix
	  (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
	  (expand-file-name "tramp-autosave/" user-emacs-directory))

;; Auto save options
(setq kill-buffer-delete-auto-save-files t)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;;; Auto revert
;; Auto-revert in Emacs is a feature that automatically updates the contents of
;; a buffer to reflect changes made to the underlying file.
(setq revert-without-query (list ".")  ; Do not prompt
	  auto-revert-stop-on-user-input nil
	  auto-revert-verbose t)

;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t)
(setq global-auto-revert-ignore-modes '(Buffer-menu-mode))  ; Resolve issue #29

;;; recentf

;; `recentf' is an that maintains a list of recently accessed files.
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup 'mode)

;; Update recentf-exclude
(setq recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))

;;; saveplace

;; Enables Emacs to remember the last location within a file upon reopening.
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)

;;; savehist

;; `savehist-mode' is an Emacs feature that preserves the minibuffer history
;; between sessions.
(setq history-length 300)
(setq savehist-save-minibuffer-history t)  ;; Default
(setq savehist-additional-variables
	  '(kill-ring                        ; clipboard
		register-alist                   ; macros
		mark-ring global-mark-ring       ; marks
		search-ring regexp-search-ring)) ; searches

;;; Frames and windows

;; However, do not resize windows pixelwise, as this can cause crashes in some
;; cases when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

(setq resize-mini-windows 'grow-only)

;; The native border "uses" a pixel of the fringe on the rightmost
;; splits, whereas `window-divider-mode' does not.
(setq window-divider-default-bottom-width 1
	  window-divider-default-places t
	  window-divider-default-right-width 1)

;;; Fontification

;; Disable fontification during user input to reduce lag in large buffers.
;; Also helps marginally with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;;; Scrolling

;; Enables faster scrolling. This may result in brief periods of inaccurate
;; syntax highlighting, which should quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Move point to top/bottom of buffer before signaling a scrolling error.
(setq scroll-error-top-bottom t)

;; Keep screen position if scroll command moved it vertically out of the window.
(setq scroll-preserve-screen-position t)

;; If `scroll-conservatively' is set above 100, the window is never
;; automatically recentered, which decreases the time spend recentering.
(setq scroll-conservatively 101)

;; 1. Preventing automatic adjustments to `window-vscroll' for long lines.
;; 2. Resolving the issue of random half-screen jumps during scrolling.
(setq auto-window-vscroll nil)

;; Number of lines of margin at the top and bottom of a window.
(setq scroll-margin 0)

;; Number of lines of continuity when scrolling by screenfuls.
(setq next-screen-context-lines 0)

;; Horizontal scrolling
(setq hscroll-margin 2
	  hscroll-step 1)

;;; Mouse

(setq mouse-yank-at-point nil)

;;; Cursor

;; The blinking cursor is distracting and interferes with cursor settings in
;; some minor modes that try to change it buffer-locally (e.g., Treemacs).
(when (bound-and-true-p blink-cursor-mode)
  (blink-cursor-mode -1))

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Do not extend the cursor to fit wide characters
(setq x-stretch-cursor nil)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;;; Text editing, indent, font, and formatting

;; Avoid automatic frame resizing when adjusting settings.
(setq global-text-scale-adjust-resizes-frames nil)

;; A longer delay can be annoying as it causes a noticeable pause after each
;; deletion, disrupting the flow of editing.
(setq delete-pair-blink-delay 0.03)

(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

;; Disable visual indicators in the fringe for buffer boundaries and empty lines
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)

;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
(setq truncate-partial-width-windows nil)

;; Configure automatic indentation to be triggered exclusively by newline and
;; DEL (backspace) characters.
(setq-default electric-indent-chars '(?\n ?\^?))

;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
(setq-default indent-tabs-mode nil
			  tab-width 4)

;; Enable indentation and completion using the TAB key
(setq tab-always-indent 'complete)
(setq tab-first-completion 'word-or-paren-or-punct)

;; Perf: Reduce command completion overhead.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines.
(setq comment-multi-line t)

;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance.
(setq comment-empty-lines t)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)

;; Disable the obsolete practice of end-of-line spacing from the typewriter era.
(setq sentence-end-double-space nil)

;; According to the POSIX, a line is defined as "a sequence of zero or more
;; non-newline characters followed by a terminating newline".
(setq require-final-newline t)

;; Eliminate delay before highlighting search matches
(setq lazy-highlight-initial-delay 0)

;;; Modeline

;; Makes Emacs omit the load average information from the mode line.
(setq display-time-default-load-average nil)

;;; Filetype

;; Do not notify the user each time Python tries to guess the indentation offset
(setq python-indent-guess-indent-offset-verbose nil)

(setq sh-indent-after-continuation 'always)

;;; Dired and ls-lisp

(setq dired-free-space nil
	  dired-dwim-target t  ; Propose a target for intelligent moving/copying
	  dired-deletion-confirmer 'y-or-n-p
	  dired-filter-verbose nil
	  dired-recursive-deletes 'top
	  dired-recursive-copies 'always
	  dired-create-destination-dirs 'ask)

;; This is a higher-level predicate that wraps `dired-directory-changed-p'
;; with additional logic. This `dired-buffer-stale-p' predicate handles remote
;; files, wdired, unreadable dirs, and delegates to dired-directory-changed-p
;; for modification checks.
(setq auto-revert-remote-files nil)
(setq dired-auto-revert-buffer 'dired-buffer-stale-p)

(setq dired-vc-rename-file t)

;; Disable the prompt about killing the Dired buffer for a deleted directory.
(setq dired-clean-confirm-killing-deleted-buffers nil)

;; dired-omit-mode
(setq dired-omit-verbose nil)
(setq dired-omit-files (concat "\\`[.]\\'"))

(setq ls-lisp-verbosity nil)
(setq ls-lisp-dirs-first t)

;;; Ediff

;; Configure Ediff to use a single frame and split windows horizontally
(setq ediff-window-setup-function 'ediff-setup-windows-plain
	  ediff-split-window-function 'split-window-horizontally)

;;; Help

;; Enhance `apropos' and related functions to perform more extensive searches
(setq apropos-do-all t)

;; Fixes #11: Prevents help command completion from triggering autoload.
;; Loading additional files for completion can slow down help commands and may
;; unintentionally execute initialization code from some libraries.
(setq help-enable-completion-autoload nil)
(setq help-enable-autoload nil)
(setq help-enable-symbol-autoload nil)
(setq help-window-select t)  ;; Focus new help windows when opened

;;; ispell
;; In Emacs 30 and newer, disable Ispell completion to avoid annotation errors
;; when no `ispell' dictionary is set.
(setq text-mode-ispell-word-completion nil)

(setq ispell-silently-savep t)

;;; ibuffer

(setq ibuffer-formats
	  '((mark modified read-only locked
			  " " (name 55 55 :left :elide)
			  " " (size 8 -1 :right)
			  " " (mode 18 18 :left :elide) " " filename-and-process)
		(mark " " (name 16 -1) " " filename)))

;;; xref

;; Enable completion in the minibuffer instead of the definitions buffer
(setq xref-show-definitions-function 'xref-show-definitions-completing-read
	  xref-show-xrefs-function 'xref-show-definitions-completing-read)

;;; abbrev

;; Ensure `abbrev_defs` is stored in the correct location when
;; `user-emacs-directory` is modified, as it defaults to ~/.emacs.d/abbrev_defs
;; regardless of the change.
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

(setq save-abbrevs 'silently)

;;; dabbrev

(setq dabbrev-upcase-means-case-search t)

(setq dabbrev-ignored-buffer-modes
	  '(archive-mode image-mode docview-mode tags-table-mode pdf-view-mode))

(setq dabbrev-ignored-buffer-regexps
	  '(;; - Buffers starting with a space (internal or temporary buffers)
		"\\` "
		;; Tags files such as ETAGS, GTAGS, RTAGS, TAGS, e?tags, and GPATH,
		;; including versions with numeric extensions like <123>
		"\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?"))

;;; Remove warnings from narrow-to-region, upcase-region...

(dolist (cmd '(list-timers narrow-to-region upcase-region downcase-region
						   list-threads erase-buffer scroll-left
						   dired-find-alternate-file))
  (put cmd 'disabled nil))
(let ((mono-font "FiraCode Nerd Font")
      (sans-font "DejaVu Sans"))
  ;; Semi-bold is usually a good middle ground
  (set-face-attribute 'default nil :family mono-font :weight 'semi-bold :height 180)
  (set-face-attribute 'fixed-pitch nil :family mono-font :weight 'semi-bold :height 180)
  (set-face-attribute 'variable-pitch nil :family sans-font :height 180))
;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 5)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose t))

;; Recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)

  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600))

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

(setq auto-save-default t
      auto-save-interval 300
      auto-save-timeout 60)
;; The built-in outline-minor-mode provides structured code folding in modes
;; such as Emacs Lisp and Python, allowing users to collapse and expand sections
;; based on headings or indentation levels. This feature enhances navigation and
;; improves the management of large files with hierarchical structures.
(use-package outline
  :diminish
  :ensure nil
  :commands outline-minor-mode
  :hook
  ((emacs-lisp-mode . outline-minor-mode)
   ;; Use " ▼" instead of the default ellipsis "..." for folded text to make
   ;; folds more visually distinctive and readable.
   (outline-minor-mode
    .
    (lambda()
      (let* ((display-table (or buffer-display-table (make-display-table)))
             (face-offset (* (face-id 'shadow) (ash 1 22)))
             (value (vconcat (mapcar (lambda (c) (+ face-offset c)) " ▼"))))
        (set-display-table-slot display-table 'selective-display value)
        (setq buffer-display-table display-table))))))

;; The outline-indent Emacs package provides a minor mode that enables code
;; folding based on indentation levels.
;;
;; In addition to code folding, *outline-indent* allows:
;; - Moving indented blocks up and down
;; - Indenting/unindenting to adjust indentation levels
;; - Inserting a new line with the same indentation level as the current line
;; - Move backward/forward to the indentation level of the current line
;; - and other features.
(use-package outline-indent
  :ensure nil
  :commands outline-indent-minor-mode

  :custom
  (outline-indent-ellipsis " ▼")

  :init
  ;; The minor mode can also be automatically activated for a certain modes.
  (add-hook 'python-mode-hook #'outline-indent-minor-mode)
  (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
  (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode))


(winner-mode 1)
(repeat-mode 1)
;; CHANGE: Remove :ensure nil from packages you want to install via package.el

(use-package diff-hl
  :config
  (diff-hl-mode)  ; Update on-the-fly
  (diff-hl-margin-mode)  ; Use margin instead of fringe
  :hook
  ((after-init . global-diff-hl-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package undo-fu
  :ensure nil

  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  ;; Store undo history files in your Emacs directory
  (setq undo-fu-session-directory (expand-file-name "undo-fu-session" user-emacs-directory))

  ;; Create the directory if it doesn't exist
  (unless (file-exists-p undo-fu-session-directory)
	(make-directory undo-fu-session-directory t))
  (setq undo-limit 800000)           ; Default is 160000
  (setq undo-strong-limit 1200000)   ; Default is 240000
  (setq undo-outer-limit 12000000)   ; Default is 24000000
  ;; Configure session settings
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-global-mode t)

  ;; Enable globally
  (global-undo-fu-session-mode))

;;; testing stop
(use-package vundo
  ;; :ensure nil  ; Remove this line to install from MELPA
  :config
  (global-set-key (kbd "C-x u") 'vundo))


(use-package editorconfig
  :diminish ""
  ;; :ensure nil  ; Remove this line to install from MELPA
  :config
  (editorconfig-mode 1))


(use-package diminish
  ;; :ensure nil  ; Remove this line to install from MELPA
  :config
  (diminish 'line-number-mode))




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
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode +1)

  ;; Info buffers to the right
  ;; (dape-buffer-window-arrangement 'right)
  ;; Info buffers like gud (gdb-mi)
  (dape-buffer-window-arrangement 'gud)
  (dape-info-hide-mode-line nil)

  ;; Projectile users
  ;; (dape-cwd-function #'projectile-project-root)

  :config
  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook #'kill-buffer)
  )


;; Personal info
(setq user-mail-address "leonpierre.dufour@gmail.com"
      user-full-name "Leon-Pierre Dufour")

;; IMAP settings for reading mail
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnir-search-engine imap)
               (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
               (nnmail-expiry-wait 90)))

;; SMTP settings for sending mail
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls)

;; Store passwords securely
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

;; Make Gnus NOT ignore [Gmail] mailboxes
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; Store sent mail on Gmail's servers
(setq gnus-message-archive-group "nnimap+gmail:[Gmail]/Sent Mail")

;; Don't keep message buffers around
(setq message-kill-buffer-on-exit t)


(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(delete-selection-mode 1)
(setq compilation-scroll-output t)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq compilation-auto-jump-to-first-error t)

(global-so-long-mode 1)
(setq history-length t)                    ; No limit on history length
(setq history-delete-duplicates nil)       ; Keep duplicates for recency-based sorting
(setq savehist-save-minibuffer-history 1)  ; Save minibuffer history
;; Core infrastructure
(require 'my-completion)  ; Needed by many modules
(require 'my-themes)      ; UI setup

;; Utilities (no dependencies)
(require 'my-utils)
(require 'my-which-key)
(require 'my-avy)

;; Major modes and languages
(require 'my-treesit)
(require 'my-prog)        ; Depends on completion
(require 'my-lang)        ; Depends on prog

;; Development tools
(require 'my-flymake)     ; Depends on prog
(require 'my-apheleia)    ; Depends on prog
(require 'my-magit)

;; Applications
(require 'my-dired)
(require 'my-shell)
(require 'my-org)
(require 'my-elfeed)

;; Modal editing (affects everything)
(require 'my-meow)

;; Windows and UI (should be late)
(require 'my-windows)

;; Keybindings last (overrides everything)
(require 'my-keybinds)

;; Optional/experimental
(require 'my-llm)



(provide 'init)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; init.el ends here
