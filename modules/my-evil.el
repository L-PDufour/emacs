;;; my-evil.el --- Evil Mode Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Hybrid Evil configuration that preserves Emacs keybindings while adding
;; vim-style modal editing. This setup is designed for users who:
;;
;; - Love Emacs keybindings and want to keep them
;; - Want vim's powerful modal editing (hjkl, text objects, operators)
;; - Don't like Kakoune/Meow style but appreciate good integrations
;; - Want chord-free access to Emacs commands via evil-keypad
;;
;; PHILOSOPHY:
;; - Normal state: Vim motions + evil-keypad for Emacs commands
;; - Insert state: Pure Emacs (all your C-c, C-x bindings work)
;; - Preserve all existing C-c keymaps (files, windows, search, code)
;; - Integrate seamlessly with Vertico, Consult, Which-key, Avy
;;
;;; Code:

(require 'evil)
(require 'evil-collection)

;;; ============================================================================
;;; BASIC EVIL CONFIGURATION
;;; ============================================================================

;; Start evil-mode
(evil-mode 1)

;; Undo system - use undo-fu if available (you have it!)
(when (require 'undo-fu nil t)
  (setq evil-undo-system 'undo-fu))

;; Respect visual line mode
(setq evil-respect-visual-line-mode t)

;; Search behavior
(setq evil-search-module 'evil-search)
(setq evil-ex-search-case 'sensitive)
(setq evil-ex-substitute-global t) ; Default to global substitute :%s/foo/bar/g

;; Split behavior
(setq evil-split-window-below t)
(setq evil-vsplit-window-right t)

;; Move beyond end of line in normal state
(setq evil-move-beyond-eol t)

;; Use fine-grained undo
(setq evil-want-fine-undo t)

;; Cursor appearance per state
(setq evil-insert-state-cursor '(bar "white")
      evil-normal-state-cursor '(box "orange")
      evil-visual-state-cursor '(box "yellow")
      evil-emacs-state-cursor  '(box "red"))

;;; ============================================================================
;;; EVIL-KEYPAD: Chord-free Emacs Commands
;;; ============================================================================
;;
;; Evil-keypad lets you access Emacs commands without chording!
;; In normal state, press SPC then simple keys:
;;
;; Examples:
;;   SPC x f     → C-x C-f (find-file)
;;   SPC c f a   → C-c f a (my-project-find-file)
;;   SPC c w v   → C-c w v (split-window-right)
;;   SPC c s l   → C-c s l (consult-line)
;;
;; Special first keys:
;;   x → C-x prefix   (SPC x f = C-x C-f)
;;   c → C-c prefix   (SPC c f a = C-c f a)
;;   h → C-h prefix   (SPC h f = C-h f)
;;   m → M- prefix    (SPC m x = M-x)
;;   g → C-M- prefix
;;   u → C-u (universal argument)
;;

(use-package evil-keypad
  :ensure nil
  :after evil
  :demand t
  :config
  ;; Bind SPC in normal and visual states to evil-keypad
  (evil-define-key '(normal visual) 'global (kbd "SPC") 'evil-keypad)

  ;; Enable which-key integration for live hints
  (when (require 'which-key nil t)
    (setq evil-keypad-show-which-key t))

  ;; Configure keypad settings
  (setq evil-keypad-timeout 0.5) ; Time to wait for next key

  ;; Your existing C-c keymaps will automatically work!
  ;; SPC c f a → C-c f a (my-project-find-file)
  ;; SPC c f b → C-c f b (my-project-find-buffer)
  ;; SPC c f r → C-c f r (consult-recent-file)
  ;; SPC c w v → C-c w v (split-window-right)
  ;; SPC c s l → C-c s l (consult-line)
  ;; SPC c l d → C-c l d (xref-find-definitions)
  )

;;; ============================================================================
;;; STATES: Keep Emacs Bindings Where They Matter
;;; ============================================================================

;; Modes that should start in Emacs state (not evil)
(setq evil-emacs-state-modes
      '(archive-mode
        bbdb-mode
        bookmark-bmenu-mode
        bookmark-edit-annotation-mode
        browse-kill-ring-mode
        bzr-annotate-mode
        calc-mode
        cfw:calendar-mode
        completion-list-mode
        Custom-mode
        debugger-mode
        delicious-search-mode
        desktop-menu-mode
        doc-view-mode
        dvc-bookmarks-mode
        dvc-diff-mode
        dvc-info-buffer-mode
        dvc-log-buffer-mode
        dvc-revlist-mode
        dvc-revlog-mode
        dvc-status-mode
        dvc-tips-mode
        ediff-mode
        ediff-meta-mode
        ert-results-mode
        eww-mode
        ffap-mode
        finder-mode
        gdb-breakpoints-mode
        gdb-disassembly-mode
        gdb-frames-mode
        gdb-locals-mode
        gdb-memory-mode
        gdb-registers-mode
        gdb-threads-mode
        gist-list-mode
        gnus-article-mode
        gnus-browse-mode
        gnus-group-mode
        gnus-server-mode
        gnus-summary-mode
        google-maps-static-mode
        ibuffer-mode
        jde-javadoc-checker-report-mode
        magit-popup-mode
        mh-folder-mode
        monky-mode
        notmuch-hello-mode
        notmuch-search-mode
        notmuch-show-mode
        occur-mode
        package-menu-mode
        pdf-outline-buffer-mode
        pdf-view-mode
        proced-mode
        rcirc-mode
        recentf-dialog-mode
        reftex-select-bib-mode
        reftex-select-label-mode
        reftex-toc-mode
        sldb-mode
        slime-inspector-mode
        slime-thread-control-mode
        slime-xref-mode
        term-mode
        tetris-mode
        tla-annotate-mode
        tla-archive-list-mode
        tla-bconfig-mode
        tla-bookmarks-mode
        tla-branch-list-mode
        tla-browse-mode
        tla-category-list-mode
        tla-changelog-mode
        tla-follow-symlinks-mode
        tla-inventory-file-mode
        tla-inventory-mode
        tla-lint-mode
        tla-logs-mode
        tla-revision-list-mode
        tla-revlog-mode
        tla-tree-lint-mode
        tla-version-list-mode
        vc-annotate-mode
        vc-dir-mode
        vc-git-log-view-mode
        vc-svn-log-view-mode
        vm-mode
        vm-summary-mode
        w3m-mode
        wab-compilation-mode
        xgit-annotate-mode
        xgit-changelog-mode
        xgit-diff-mode
        xgit-revlog-mode
        xhg-annotate-mode
        xhg-log-mode
        xhg-mode
        xhg-mq-mode
        xhg-mq-sub-mode
        xhg-status-extra-mode))

;; Modes that should start in insert state
(setq evil-insert-state-modes
      '(git-commit-mode
        magit-popup-mode))

;; Modes that should start in motion state (read-only, vim motions work)
(setq evil-motion-state-modes
      '(help-mode
        Man-mode
        woman-mode
        info-mode
        apropos-mode
        compilation-mode
        grep-mode
        occur-mode))

;;; ============================================================================
;;; KEYBINDINGS: Hybrid Vim + Emacs
;;; ============================================================================

;; Keep C-g as keyboard-quit (your custom prot/keyboard-quit-dwim)
(define-key evil-insert-state-map (kbd "C-g") 'prot/keyboard-quit-dwim)
(define-key evil-normal-state-map (kbd "C-g") 'prot/keyboard-quit-dwim)
(define-key evil-visual-state-map (kbd "C-g") 'prot/keyboard-quit-dwim)

;; Keep C-z as undo-fu (you have this configured)
(when (fboundp 'undo-fu-only-undo)
  (define-key evil-insert-state-map (kbd "C-z") 'undo-fu-only-undo)
  (define-key evil-insert-state-map (kbd "C-S-z") 'undo-fu-only-redo))

;; Map undo/redo in normal state to undo-fu
(when (fboundp 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo))

;; Keep window navigation in insert state
(define-key evil-insert-state-map (kbd "C-w") evil-window-map)

;; Escape should exit to normal state (standard vim behavior)
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

;; Use visual line motions for wrapped lines
(evil-define-key 'normal 'global
  (kbd "j") 'evil-next-visual-line
  (kbd "k") 'evil-previous-visual-line)

;; Window management from normal state (vim-style)
(evil-define-key 'normal 'global
  (kbd "C-w h") 'windmove-left
  (kbd "C-w j") 'windmove-down
  (kbd "C-w k") 'windmove-up
  (kbd "C-w l") 'windmove-right
  (kbd "C-w v") 'split-window-right
  (kbd "C-w s") 'split-window-below
  (kbd "C-w q") 'delete-window
  (kbd "C-w o") 'delete-other-windows)

;; Leader-style bindings (alternative to evil-keypad)
;; These provide quick access to common commands
(evil-define-key 'normal 'global
  (kbd ",") nil) ; Free up comma as potential leader

;; Quick access to Avy (you have my-avy configured)
(when (fboundp 'avy-goto-char-timer)
  (evil-define-key 'normal 'global
    (kbd "s") 'avy-goto-char-timer    ; 's' for search/jump
    (kbd "S") 'avy-goto-line))

;; Keep 'f' and 't' as vim's find-char
(evil-define-key 'normal 'global
  (kbd "f") 'evil-find-char
  (kbd "F") 'evil-find-char-backward
  (kbd "t") 'evil-find-char-to
  (kbd "T") 'evil-find-char-to-backward
  (kbd ";") 'evil-repeat-find-char
  (kbd ",") 'evil-repeat-find-char-backward)

;; Use 'g' prefix for goto operations (vim-style)
(evil-define-key 'normal 'global
  (kbd "gd") 'xref-find-definitions
  (kbd "gr") 'xref-find-references
  (kbd "gb") 'xref-go-back
  (kbd "gf") 'find-file-at-point)

;; Use ']' and '[' for navigation (like vim-unimpaired)
(evil-define-key 'normal 'global
  (kbd "] b") 'next-buffer
  (kbd "[ b") 'previous-buffer
  (kbd "] e") 'flymake-goto-next-error
  (kbd "[ e") 'flymake-goto-prev-error
  (kbd "] q") 'next-error
  (kbd "[ q") 'previous-error)

;; Comments (using built-in comment-dwim or similar)
(evil-define-key 'normal 'global
  (kbd "gc") 'comment-dwim)
(evil-define-key 'visual 'global
  (kbd "gc") 'comment-dwim)

;;; ============================================================================
;;; EVIL-COLLECTION: Package Integrations
;;; ============================================================================
;;
;; evil-collection provides evil bindings for many Emacs packages
;; (similar to how meow-tree-sitter extends meow)
;;

;; Configure which packages evil-collection should NOT integrate with
;; (in case you want to keep some packages in pure Emacs mode)
(setq evil-collection-mode-list
      '(;; Core
        (buff-menu "buff-menu")
        calendar
        compile
        custom
        debug
        help
        ibuffer
        info
        replace
        simple

        ;; Version control
        magit
        diff-mode
        ediff

        ;; Completion/search (careful here - you want to preserve your setup)
        vertico
        consult

        ;; Dired
        dired
        wdired

        ;; Org mode
        org
        org-agenda

        ;; Development
        xref
        flymake

        ;; Others
        elfeed
        man
        woman))

;; Setup evil-collection
(evil-collection-init)

;; Special handling for minibuffer - keep it Emacs-like
(evil-set-initial-state 'minibuffer-inactive-mode 'emacs)

;;; ============================================================================
;;; INTEGRATIONS: Meow-style Smooth Operator Feeling
;;; ============================================================================

;; Integration with which-key (you have my-which-key)
(when (require 'which-key nil t)
  ;; Add descriptions for evil states
  (which-key-add-key-based-replacements
    "SPC x" "C-x prefix"
    "SPC c" "C-c prefix"
    "SPC h" "help"
    "SPC m" "M-x"
    "SPC c f" "files"
    "SPC c w" "windows"
    "SPC c s" "search"
    "SPC c p" "project"
    "SPC c l" "code"))

;; Integration with consult (your completion setup)
;; Make evil play nice with consult's navigation
(when (require 'consult nil t)
  (evil-define-key 'normal 'global
    (kbd "M-y") 'consult-yank-pop)) ; Better yank history

;; Visual mode enhancements for region operations
(evil-define-key 'visual 'global
  (kbd "v") 'er/expand-region) ; If you have expand-region

;;; ============================================================================
;;; TEXT OBJECTS: Enhanced Vim Text Objects
;;; ============================================================================

;; Add extra text objects for better editing
;; You can expand this with evil-surround, evil-textobj-tree-sitter, etc.

;; Example: 'e' for entire buffer
(evil-define-text-object evil-entire-buffer (count &optional beg end type)
  "Select entire buffer"
  (evil-range (point-min) (point-max)))

(define-key evil-outer-text-objects-map "e" 'evil-entire-buffer)

;;; ============================================================================
;;; OPTIONAL ENHANCEMENTS (Uncomment to enable)
;;; ============================================================================

;; evil-surround: cs"' to change surrounding quotes, ys to add surround
;; (use-package evil-surround
;;   :ensure nil
;;   :config
;;   (global-evil-surround-mode 1))

;; evil-commentary: gcc to comment line, gc{motion} to comment
;; (use-package evil-commentary
;;   :ensure nil
;;   :config
;;   (evil-commentary-mode))

;; evil-indent-plus: ii/ai text objects for indentation
;; (use-package evil-indent-plus
;;   :ensure nil
;;   :config
;;   (evil-indent-plus-default-bindings))

;; evil-exchange: gx{motion} to mark, gx again to exchange
;; (use-package evil-exchange
;;   :ensure nil
;;   :config
;;   (evil-exchange-install))

;; evil-textobj-tree-sitter: Tree-sitter powered text objects
;; (use-package evil-textobj-tree-sitter
;;   :ensure nil
;;   :after (evil tree-sitter))

;;; ============================================================================
;;; MODE-SPECIFIC CONFIGURATIONS
;;; ============================================================================

;; Don't enable evil in certain modes (like terminal)
(add-hook 'term-mode-hook 'evil-emacs-state)
(add-hook 'eshell-mode-hook 'evil-emacs-state)

;; Start in normal state for prog-mode (where meow was enabled)
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (eq evil-state 'insert)
              (evil-normal-state))))

;; Org-mode specific: don't hijack TAB
(evil-define-key 'normal org-mode-map
  (kbd "TAB") 'org-cycle
  (kbd "<tab>") 'org-cycle)

;;; ============================================================================
;;; HELPER FUNCTIONS
;;; ============================================================================

(defun my-evil-shift-left-visual ()
  "Shift left and keep visual selection."
  (interactive)
  (call-interactively 'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(defun my-evil-shift-right-visual ()
  "Shift right and keep visual selection."
  (interactive)
  (call-interactively 'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

(evil-define-key 'visual 'global
  (kbd "<") 'my-evil-shift-left-visual
  (kbd ">") 'my-evil-shift-right-visual)

;;; ============================================================================
;;; USAGE GUIDE
;;; ============================================================================

;; QUICK REFERENCE:
;;
;; NORMAL STATE (vim motions):
;;   hjkl        - Move cursor
;;   w/b         - Word forward/backward
;;   0/$         - Line start/end
;;   gg/G        - Buffer start/end
;;   f{char}     - Find character
;;   s{chars}    - Jump with avy
;;   d{motion}   - Delete
;;   c{motion}   - Change
;;   y{motion}   - Yank (copy)
;;   p           - Paste
;;   u           - Undo
;;   C-r         - Redo
;;   v           - Visual mode
;;   V           - Visual line mode
;;   C-v         - Visual block mode
;;   gd          - Go to definition
;;   gr          - Find references
;;   ]b/[b       - Next/prev buffer
;;   ]e/[e       - Next/prev error
;;
;; EVIL-KEYPAD (SPC prefix in normal state):
;;   SPC x f     - C-x C-f (find-file)
;;   SPC c f a   - C-c f a (my-project-find-file)
;;   SPC c f b   - C-c f b (my-project-find-buffer)
;;   SPC c f r   - C-c f r (consult-recent-file)
;;   SPC c w v   - C-c w v (split-window-right)
;;   SPC c w s   - C-c w s (split-window-below)
;;   SPC c w h   - C-c w h (windmove-left)
;;   SPC c s l   - C-c s l (consult-line)
;;   SPC c s g   - C-c s g (consult-grep)
;;   SPC c l d   - C-c l d (xref-find-definitions)
;;   SPC c l r   - C-c l r (xref-find-references)
;;   SPC m x     - M-x (execute-extended-command)
;;   SPC h f     - C-h f (describe-function)
;;   SPC h v     - C-h v (describe-variable)
;;
;; INSERT STATE (pure Emacs):
;;   C-c ...     - All your C-c keymaps work
;;   C-x ...     - All C-x commands work
;;   C-g         - prot/keyboard-quit-dwim
;;   C-z         - undo-fu-only-undo
;;   ESC         - Back to normal state
;;
;; WINDOW MANAGEMENT (both insert and normal):
;;   C-w h/j/k/l - Navigate windows
;;   C-w v       - Split vertically
;;   C-w s       - Split horizontally
;;   C-w q       - Delete window
;;   C-w o       - Delete other windows
;;
;; TEXT OBJECTS:
;;   i{char}     - Inner object (iw=inner word, i"=inside quotes)
;;   a{char}     - Around object (aw=a word, a"=around quotes)
;;   Common: w (word), s (sentence), p (paragraph), ", ', (, [, {, <, t (tag)

(provide 'my-evil)
;;; my-evil.el ends here
