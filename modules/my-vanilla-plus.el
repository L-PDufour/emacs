;;; my-vanilla-plus.el --- Enhanced Vanilla Emacs Keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Enhanced vanilla Emacs experience with modern conveniences:
;; - S-exp navigation with repeat maps (no constant chording!)
;; - Vim-style C-o/C-i for jump navigation
;; - Repeat maps for common operations (windows, buffers, etc.)
;; - Better defaults while staying close to vanilla Emacs
;;
;; PHILOSOPHY:
;; - Vanilla Emacs keybindings as foundation
;; - Add repeat maps to reduce RSI from constant modifier holding
;; - Vim-style jump navigation (C-o/C-i) for better workflow
;; - Enhance, don't replace - keep Emacs muscle memory
;;
;;; Code:

;;; ============================================================================
;;; ENSURE REPEAT-MODE IS ENABLED
;;; ============================================================================

;; repeat-mode is built-in since Emacs 28
;; It allows commands to be repeated without re-entering the prefix key
(repeat-mode 1)

;; Show repeat indicator in echo area
(setq repeat-echo-message-function #'repeat-echo-mode-line-message)

;; Timeout for repeat mode (nil = no timeout, stay in repeat mode until non-repeat key)
(setq repeat-exit-timeout 3) ; Exit after 3 seconds of inactivity

;;; ============================================================================
;;; S-EXP NAVIGATION REPEAT MAP
;;; ============================================================================
;;
;; Problem: Navigating s-expressions requires constant C-M-f, C-M-b, etc.
;; Solution: Press the chord once, then just tap f, b, u, d, etc.
;;
;; Example workflow:
;;   C-M-f     → forward-sexp (enters repeat mode)
;;   f f f     → keep going forward (just tap 'f')
;;   b b       → go back twice (just tap 'b')
;;   u         → go up a level
;;   <any other key> → exit repeat mode
;;

(defvar sexp-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    ;; Forward/backward sexp
    (define-key map (kbd "f") #'forward-sexp)
    (define-key map (kbd "b") #'backward-sexp)

    ;; Up/down list
    (define-key map (kbd "u") #'backward-up-list)
    (define-key map (kbd "d") #'down-list)

    ;; Forward/backward list
    (define-key map (kbd "n") #'forward-list)
    (define-key map (kbd "p") #'backward-list)

    ;; Beginning/end of defun
    (define-key map (kbd "a") #'beginning-of-defun)
    (define-key map (kbd "e") #'end-of-defun)

    ;; Mark sexp
    (define-key map (kbd "SPC") #'mark-sexp)

    ;; Transpose
    (define-key map (kbd "t") #'transpose-sexps)

    ;; Kill/copy
    (define-key map (kbd "k") #'kill-sexp)
    (define-key map (kbd "w") #'copy-sexp-as-kill)

    map)
  "Repeat map for s-expression navigation commands.")

;; Enable repeat mode for all sexp navigation commands
(dolist (cmd '(forward-sexp
               backward-sexp
               backward-up-list
               down-list
               forward-list
               backward-list
               beginning-of-defun
               end-of-defun
               mark-sexp
               transpose-sexps
               kill-sexp
               copy-sexp-as-kill))
  (put cmd 'repeat-map 'sexp-navigation-repeat-map))

;; Add helpful message when entering sexp repeat mode
(defun sexp-repeat-mode-message ()
  "Show helpful message for sexp repeat mode."
  (message "S-exp: [f]wd [b]ack [u]p [d]own [n]ext [p]rev [a]start [e]nd [SPC]mark [k]ill [w]copy [t]ranspose"))

;; Optional: Show message when entering repeat mode
;; (advice-add 'forward-sexp :after (lambda (&rest _) (sexp-repeat-mode-message)))

;;; ============================================================================
;;; VIM-STYLE JUMP NAVIGATION (C-o / C-i)
;;; ============================================================================
;;
;; Problem: M-. goes to definition, but jumping back requires M-,
;;          No forward jump like vim's C-i
;; Solution: C-o jumps back, C-i jumps forward (vim-style)
;;
;; This integrates with xref marker stack (M-. creates markers)
;;

;; C-o: Jump back (like vim)
(global-set-key (kbd "C-o") #'xref-go-back)

;; C-i: Jump forward (like vim)
;; Note: In terminal, C-i = TAB, so this only works in GUI Emacs
;; For terminal users, we'll also bind M-i
(global-set-key (kbd "C-i") #'xref-go-forward)
(global-set-key (kbd "M-i") #'xref-go-forward) ; Alternative for terminal

;; Keep M-, as alternative (some users prefer it)
;; M-. is already bound to xref-find-definitions
;; M-, is already bound to xref-go-back

;; Create repeat map for jump navigation
(defvar jump-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'xref-go-back)
    (define-key map (kbd "i") #'xref-go-forward)
    map)
  "Repeat map for jump navigation.")

(put 'xref-go-back 'repeat-map 'jump-repeat-map)
(put 'xref-go-forward 'repeat-map 'jump-repeat-map)

;;; ============================================================================
;;; WINDOW NAVIGATION REPEAT MAP
;;; ============================================================================
;;
;; Problem: C-x o to switch windows repeatedly is tedious
;; Solution: C-x o once, then just tap o o o
;;

(defvar window-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    ;; Switch windows
    (define-key map (kbd "o") #'other-window)

    ;; Direction-based (if windmove is available)
    (define-key map (kbd "h") #'windmove-left)
    (define-key map (kbd "j") #'windmove-down)
    (define-key map (kbd "k") #'windmove-up)
    (define-key map (kbd "l") #'windmove-right)

    ;; Resize
    (define-key map (kbd "+") #'enlarge-window)
    (define-key map (kbd "-") #'shrink-window)
    (define-key map (kbd ">") #'enlarge-window-horizontally)
    (define-key map (kbd "<") #'shrink-window-horizontally)
    (define-key map (kbd "=") #'balance-windows)

    ;; Split
    (define-key map (kbd "2") #'split-window-below)
    (define-key map (kbd "3") #'split-window-right)

    ;; Delete
    (define-key map (kbd "0") #'delete-window)
    (define-key map (kbd "1") #'delete-other-windows)

    map)
  "Repeat map for window operations.")

(put 'other-window 'repeat-map 'window-navigation-repeat-map)
(put 'windmove-left 'repeat-map 'window-navigation-repeat-map)
(put 'windmove-right 'repeat-map 'window-navigation-repeat-map)
(put 'windmove-up 'repeat-map 'window-navigation-repeat-map)
(put 'windmove-down 'repeat-map 'window-navigation-repeat-map)
(put 'enlarge-window 'repeat-map 'window-navigation-repeat-map)
(put 'shrink-window 'repeat-map 'window-navigation-repeat-map)
(put 'enlarge-window-horizontally 'repeat-map 'window-navigation-repeat-map)
(put 'shrink-window-horizontally 'repeat-map 'window-navigation-repeat-map)
(put 'balance-windows 'repeat-map 'window-navigation-repeat-map)

;;; ============================================================================
;;; BUFFER NAVIGATION REPEAT MAP
;;; ============================================================================
;;
;; Problem: Cycling through buffers with C-x C-<left/right> is cumbersome
;; Solution: Bind to easier keys and add repeat map
;;

;; Better buffer cycling keybindings
(global-set-key (kbd "C-x <left>") #'previous-buffer)
(global-set-key (kbd "C-x <right>") #'next-buffer)
(global-set-key (kbd "C-x p") #'previous-buffer)
(global-set-key (kbd "C-x n") #'next-buffer)

(defvar buffer-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") #'previous-buffer)
    (define-key map (kbd "<right>") #'next-buffer)
    (define-key map (kbd "p") #'previous-buffer)
    (define-key map (kbd "n") #'next-buffer)
    map)
  "Repeat map for buffer navigation.")

(put 'previous-buffer 'repeat-map 'buffer-navigation-repeat-map)
(put 'next-buffer 'repeat-map 'buffer-navigation-repeat-map)

;;; ============================================================================
;;; PAGE NAVIGATION REPEAT MAP
;;; ============================================================================
;;
;; Problem: Scrolling with C-v and M-v repeatedly
;; Solution: Press once, then just tap v
;;

(defvar page-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'scroll-up-command)
    (define-key map (kbd "V") #'scroll-down-command)
    (define-key map (kbd "l") #'recenter-top-bottom)
    map)
  "Repeat map for page navigation.")

(put 'scroll-up-command 'repeat-map 'page-navigation-repeat-map)
(put 'scroll-down-command 'repeat-map 'page-navigation-repeat-map)
(put 'recenter-top-bottom 'repeat-map 'page-navigation-repeat-map)

;;; ============================================================================
;;; LINE NAVIGATION REPEAT MAP
;;; ============================================================================
;;
;; Problem: Moving lines up/down with C-n/C-p is fine, but could be easier
;; Solution: Add repeat map for C-n/C-p
;;

(defvar line-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "f") #'forward-char)
    (define-key map (kbd "b") #'backward-char)
    (define-key map (kbd "a") #'move-beginning-of-line)
    (define-key map (kbd "e") #'move-end-of-line)
    map)
  "Repeat map for basic line navigation.")

(put 'next-line 'repeat-map 'line-navigation-repeat-map)
(put 'previous-line 'repeat-map 'line-navigation-repeat-map)

;;; ============================================================================
;;; WORD NAVIGATION REPEAT MAP
;;; ============================================================================

(defvar word-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'forward-word)
    (define-key map (kbd "b") #'backward-word)
    (define-key map (kbd "k") #'kill-word)
    (define-key map (kbd "d") #'backward-kill-word)
    map)
  "Repeat map for word navigation.")

(put 'forward-word 'repeat-map 'word-navigation-repeat-map)
(put 'backward-word 'repeat-map 'word-navigation-repeat-map)

;;; ============================================================================
;;; PARAGRAPH NAVIGATION REPEAT MAP
;;; ============================================================================

(defvar paragraph-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'forward-paragraph)
    (define-key map (kbd "p") #'backward-paragraph)
    map)
  "Repeat map for paragraph navigation.")

(put 'forward-paragraph 'repeat-map 'paragraph-navigation-repeat-map)
(put 'backward-paragraph 'repeat-map 'paragraph-navigation-repeat-map)

;;; ============================================================================
;;; UNDO/REDO REPEAT MAP
;;; ============================================================================

;; If using undo-fu
(when (require 'undo-fu nil t)
  (defvar undo-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "z") #'undo-fu-only-undo)
      (define-key map (kbd "Z") #'undo-fu-only-redo)
      map)
    "Repeat map for undo operations.")

  (put 'undo-fu-only-undo 'repeat-map 'undo-repeat-map)
  (put 'undo-fu-only-redo 'repeat-map 'undo-repeat-map))

;;; ============================================================================
;;; BETTER MARK AND EXCHANGE
;;; ============================================================================
;;
;; Make C-x C-x more useful: exchange point and mark without activating mark
;;

(defun exchange-point-and-mark-no-activate ()
  "Exchange point and mark without activating the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(global-set-key (kbd "C-x C-x") #'exchange-point-and-mark-no-activate)

;; Keep regular version available
(global-set-key (kbd "C-x x") #'exchange-point-and-mark)

;;; ============================================================================
;;; BETTER BEGINNING OF LINE
;;; ============================================================================
;;
;; C-a moves to first non-whitespace character, press again to go to column 0
;;

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning of line.
Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key (kbd "C-a") #'smart-beginning-of-line)

;;; ============================================================================
;;; DUPLICATE LINE/REGION
;;; ============================================================================
;;
;; Useful command that many other editors have
;;

(defun duplicate-line-or-region ()
  "Duplicate current line or region."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (goto-char (region-end))
        (insert text))
    (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (end-of-line)
      (newline)
      (insert line))))

(global-set-key (kbd "C-c d") #'duplicate-line-or-region)

;;; ============================================================================
;;; MOVE LINE UP/DOWN
;;; ============================================================================
;;
;; Like Alt+Up/Down in many editors
;;

(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<up>") #'move-line-up)
(global-set-key (kbd "M-<down>") #'move-line-down)

;; Also bind to C-S-up/down for consistency with other editors
(global-set-key (kbd "C-S-<up>") #'move-line-up)
(global-set-key (kbd "C-S-<down>") #'move-line-down)

;;; ============================================================================
;;; BETTER COMMENT DWIM
;;; ============================================================================
;;
;; Comment/uncomment region or current line
;;

(defun comment-or-uncomment-line-or-region ()
  "Comment or uncomment current line or region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "C-;") #'comment-or-uncomment-line-or-region)

;;; ============================================================================
;;; BETTER INDENTATION
;;; ============================================================================
;;
;; Indent region or current line
;;

(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(global-set-key (kbd "C-c i") #'indent-buffer)

;;; ============================================================================
;;; RECENTF INTEGRATION
;;; ============================================================================
;;
;; Quick access to recent files
;;

(global-set-key (kbd "C-c r") #'recentf-open-files)

;;; ============================================================================
;;; BETTER KILL RING ACCESS
;;; ============================================================================
;;
;; Bind yank-pop to something more accessible (if not using consult)
;; M-y after C-y cycles through kill ring
;; This is already the default, but ensure it's repeatable

(defvar yank-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "y") #'yank-pop)
    map)
  "Repeat map for yank operations.")

(put 'yank-pop 'repeat-map 'yank-repeat-map)

;;; ============================================================================
;;; MARK WHOLE LINE
;;; ============================================================================
;;
;; Like C-a C-SPC C-e but in one command
;;

(defun mark-whole-line ()
  "Mark the whole line including newline."
  (interactive)
  (beginning-of-line)
  (push-mark (line-end-position) nil t))

(global-set-key (kbd "C-c l") #'mark-whole-line)

;;; ============================================================================
;;; OCCUR REPEAT MAP
;;; ============================================================================
;;
;; Make occur navigation repeatable
;;

(with-eval-after-load 'replace
  (defvar occur-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'occur-next)
      (define-key map (kbd "p") #'occur-prev)
      map)
    "Repeat map for occur navigation.")

  (put 'occur-next 'repeat-map 'occur-repeat-map)
  (put 'occur-prev 'repeat-map 'occur-repeat-map))

;;; ============================================================================
;;; COMPILATION REPEAT MAP
;;; ============================================================================
;;
;; Make compilation error navigation repeatable
;;

(defvar compilation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-error)
    (define-key map (kbd "p") #'previous-error)
    map)
  "Repeat map for compilation errors.")

(put 'next-error 'repeat-map 'compilation-repeat-map)
(put 'previous-error 'repeat-map 'compilation-repeat-map)

;;; ============================================================================
;;; FLYMAKE REPEAT MAP (if available)
;;; ============================================================================

(with-eval-after-load 'flymake
  (defvar flymake-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'flymake-goto-next-error)
      (define-key map (kbd "p") #'flymake-goto-prev-error)
      map)
    "Repeat map for flymake navigation.")

  (put 'flymake-goto-next-error 'repeat-map 'flymake-repeat-map)
  (put 'flymake-goto-prev-error 'repeat-map 'flymake-repeat-map))

;;; ============================================================================
;;; CONFIGURATION SUMMARY
;;; ============================================================================

(defun vanilla-plus-show-config ()
  "Show summary of vanilla-plus enhancements."
  (interactive)
  (with-current-buffer (get-buffer-create "*Vanilla Plus Config*")
    (erase-buffer)
    (insert "=== Vanilla Plus Enhancements ===\n\n")
    (insert "S-EXP NAVIGATION (enter with C-M-f, C-M-b, etc.):\n")
    (insert "  f - forward-sexp\n")
    (insert "  b - backward-sexp\n")
    (insert "  u - backward-up-list\n")
    (insert "  d - down-list\n")
    (insert "  n/p - forward/backward-list\n")
    (insert "  a/e - beginning/end-of-defun\n")
    (insert "  SPC - mark-sexp\n")
    (insert "  k - kill-sexp, w - copy-sexp\n\n")

    (insert "JUMP NAVIGATION (vim-style):\n")
    (insert "  C-o - xref-go-back (jump back)\n")
    (insert "  C-i / M-i - xref-go-forward (jump forward)\n")
    (insert "  Works with M-. (xref-find-definitions)\n\n")

    (insert "WINDOW NAVIGATION (enter with C-x o):\n")
    (insert "  o - other-window\n")
    (insert "  h/j/k/l - windmove directions\n")
    (insert "  +/- - enlarge/shrink vertically\n")
    (insert "  >/</ - enlarge/shrink horizontally\n")
    (insert "  = - balance-windows\n\n")

    (insert "BUFFER NAVIGATION:\n")
    (insert "  C-x <left>/p - previous-buffer (repeatable)\n")
    (insert "  C-x <right>/n - next-buffer (repeatable)\n\n")

    (insert "ENHANCED COMMANDS:\n")
    (insert "  C-a - smart beginning of line (indent-aware)\n")
    (insert "  C-; - comment/uncomment line or region\n")
    (insert "  C-c d - duplicate line or region\n")
    (insert "  C-c i - indent entire buffer\n")
    (insert "  C-c l - mark whole line\n")
    (insert "  M-<up/down> - move line up/down\n\n")

    (insert "REPEAT MODE:\n")
    (insert "  All navigation commands enter repeat mode\n")
    (insert "  Timeout: 3 seconds (customize with repeat-exit-timeout)\n")
    (insert "  Press any non-repeat key to exit\n")
    (display-buffer (current-buffer))))

(global-set-key (kbd "C-c ?") #'vanilla-plus-show-config)

;;; ============================================================================
;;; USAGE GUIDE
;;; ============================================================================

;; QUICK START:
;;
;; 1. S-EXP NAVIGATION:
;;    Press C-M-f once, then just tap:
;;    - f f f (forward 3 times)
;;    - b b (back 2 times)
;;    - u (up one level)
;;    No more C-M-f C-M-f C-M-f!
;;
;; 2. JUMP NAVIGATION:
;;    M-. to go to definition
;;    C-o to jump back (like vim!)
;;    C-i to jump forward
;;    Can repeat: C-o o o o
;;
;; 3. WINDOW SWITCHING:
;;    C-x o once, then: o o o
;;    Or use h/j/k/l for directions
;;
;; 4. SMART C-a:
;;    First C-a: go to first non-whitespace
;;    Second C-a: go to column 0
;;
;; 5. DUPLICATE LINES:
;;    C-c d duplicates current line or region
;;
;; 6. MOVE LINES:
;;    M-<up> moves line up
;;    M-<down> moves line down

(provide 'my-vanilla-plus)
;;; my-vanilla-plus.el ends here
