;;; post-init.el --- Simplified Emacs Configuration -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;; Streamlined core Emacs configuration

;;; Code:
(let ((mono-font "FiraCode Nerd Font")
	  (sans-font "DejaVu Sans"))
  ;; Significantly larger font size for 4K screens
  ;; Values between 180-240 are often good for 4K displays
  (set-face-attribute 'default nil :family mono-font :weight 'medium :height 200)
  (set-face-attribute 'fixed-pitch nil :family mono-font :height 200)
  (set-face-attribute 'variable-pitch nil :family sans-font :height 200))
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

;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)

(setq auto-save-interval 300)
(setq auto-save-timeout 60)

;; When auto-save-visited-mode is enabled, Emacs will auto-save file-visiting
;; buffers after a certain amount of idle time if the user forgets to save it
;; with save-buffer or C-x s for example.
;;
;; This is different from auto-save-mode: auto-save-mode periodically saves
;; all modified buffers, creating backup files, including those not associated
;; with a file, while auto-save-visited-mode only saves file-visiting buffers
;; after a period of idle time, directly saving to the file itself without
;; creating backup files.
(setq auto-save-visited-interval 30)   ; Save after 5 seconds if inactivity
(auto-save-visited-mode 1)

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
  :ensure t
  :commands outline-indent-minor-mode

  :custom
  (outline-indent-ellipsis " ▼")

  :init
  ;; The minor mode can also be automatically activated for a certain modes.
  (add-hook 'python-mode-hook #'outline-indent-minor-mode)
  (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
  (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode))

;; The stripspace Emacs package provides stripspace-local-mode, a minor mode
;; that automatically removes trailing whitespace and blank lines at the end of
;; the buffer when saving.
                                        ; (use-package stripspace
;;   :diminish
;;   :ensure nil
;;   :commands stripspace-local-mode
;;
;;   ;; Enable for prog-mode-hook, text-mode-hook, conf-mode-hook
;;   :hook ((prog-mode . stripspace-local-mode)
;;          (text-mode . stripspace-local-mode)
;;          (conf-mode . stripspace-local-mode))
;;
;;   :custom
;;   ;; The `stripspace-only-if-initially-clean' option:
;;   ;; - nil to always delete trailing whitespace.
;;   ;; - Non-nil to only delete whitespace when the buffer is clean initially.
;;   ;; (The initial cleanliness check is performed when `stripspace-local-mode'
;;   ;; is enabled.)
;;   (stripspace-only-if-initially-clean nil)
;;
;;   ;; Enabling `stripspace-restore-column' preserves the cursor's column position
;;   ;; even after stripping spaces. This is useful in scenarios where you add
;;   ;; extra spaces and then save the file. Although the spaces are removed in the
;;   ;; saved file, the cursor remains in the same position, ensuring a consistent
;;   ;; editing experience without affecting cursor placement.
;;   (stripspace-restore-column t))
;;
;; ;; ;; This automates the process of updating installed packages
;; ;; (use-package auto-package-update
;; ;;   :ensure t
;; ;;   :custom
;; ;;   ;; Set the number of days between automatic updates.
;; ;;   ;; Here, packages will only be updated if at least 7 days have passed
;; ;;   ;; since the last successful update.
;; ;;   (auto-package-update-interval 7)
;; ;;
;; ;;   ;; Suppress display of the *auto-package-update results* buffer after updates.
;; ;;   ;; This keeps the user interface clean and avoids unnecessary interruptions.
;; ;;   (auto-package-update-hide-results t)
;; ;;
;; ;;   ;; Automatically delete old package versions after updates to reduce disk
;; ;;   ;; usage and keep the package directory clean. This prevents the accumulation
;; ;;   ;; of outdated files in Emacs’s package directory, which consume
;; ;;   ;; unnecessary disk space over time.
;; ;;   (auto-package-update-delete-old-versions t)
;; ;;
;; ;;   ;; Uncomment the following line to enable a confirmation prompt
;; ;;   ;; before applying updates. This can be useful if you want manual control.
;; ;;   ;; (auto-package-update-prompt-before-update t)
;; ;;
;; ;;   :config
;; ;;   ;; Run package updates automatically at startup, but only if the configured
;; ;;   ;; interval has elapsed.
;; ;;   (auto-package-update-maybe)
;; ;;
;; ;;   ;; Schedule a background update attempt daily at 10:00 AM.
;; ;;   ;; This uses Emacs' internal timer system. If Emacs is running at that time,
;; ;;   ;; the update will be triggered. Otherwise, the update is skipped for that
;; ;;   ;; day. Note that this scheduled update is independent of
;; ;;   ;; `auto-package-update-maybe` and can be used as a complementary or
;; ;;   ;; alternative mechanism.
;; ;;   (auto-package-update-at-time "10:00"))
;; ;; Helpful is an alternative to the built-in Emacs help that provides much more
;; ;; contextual information.
(use-package helpful
  :ensure t
  :commands (helpful-callable

             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

;; (use-package buffer-terminator
;;   :ensure t
;;   :custom
;;   ;; Enable/Disable verbose mode to log buffer cleanup events
;;   (buffer-terminator-verbose nil)
;;
;;   ;; Set the inactivity timeout (in seconds) after which buffers are considered
;;   ;; inactive (default is 30 minutes):
;;   (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes
;;
;;   ;; Define how frequently the cleanup process should run (default is every 10
;;   ;; minutes):
;;   (buffer-terminator-interval (* 10 60)) ; 10 minutes
;;
;;   :config
;;   (buffer-terminator-mode 1))

(winner-mode 1)
(repeat-mode 1)
;; CHANGE: Remove :ensure nil from packages you want to install via package.el
                                        ; (use-package diff-hl
                                        ;   :hook
                                        ;   ((after-init . global-diff-hl-mode)
                                        ;    (magit-post-refresh . diff-hl-magit-post-refresh)))
(use-package undo-fu
  :ensure t

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
  (setq undo-fu-session-directory (expand-file-name "undo-fu-session" minimal-emacs-user-directory))

  ;; Create the directory if it doesn't exist
  (unless (file-exists-p undo-fu-session-directory)
	(make-directory undo-fu-session-directory t))

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

(use-package window
  :ensure nil       ;; This is built-in, no need to fetch it.
  :custom
  (display-buffer-alist
   '(
     ("\\*.*e?shell\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -1))

     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     )))


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
;; (setq treesit-font-lock-level 4) ;; Use advanced font locking for Treesit mode.
(global-so-long-mode 1)
(setq history-length t)                    ; No limit on history length
(setq history-delete-duplicates nil)       ; Keep duplicates for recency-based sorting
(setq savehist-save-minibuffer-history 1)  ; Save minibuffer history
(require 'my-test)
;; Load completion system first - other modules depend on it
(require 'my-completion)
;; Load core utilities and themes early
(require 'my-themes)
(require 'my-which-key)
(require 'my-utils)

;; Load editing enhancements
(require 'my-editing-utils)
(require 'my-treesit)

(require 'my-meow)
;; Load programming support (depends on completion)
(require 'my-prog)
(require 'my-elisp)
(require 'my-lang)

;; Load version control (can be heavy)
(require 'my-magit)

;; Load applications
(require 'my-dired)
(require 'my-shell)
(require 'my-org)

;; Load modal editing last (affects all modes)

;; Load custom keybindings last
(require 'my-keybinds)

;; Commented out modules
(require 'my-llm)
;; (require 'my-spelling)
;; (require 'my-evil)
;; (require 'my-lsp)

(provide 'post-init)
;;; post-init.el ends here
