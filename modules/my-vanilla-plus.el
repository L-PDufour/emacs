;;; my-vanilla-plus.el --- Minimal S-exp and Xref Enhancements -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Minimal enhancements focused on two pain points:
;; 1. S-exp navigation with repeat maps (no more constant C-M-f C-M-f C-M-f)
;; 2. Better xref workflow (keep vanilla M-. and M-,)
;;
;; PHILOSOPHY:
;; - Vanilla Emacs keybindings - nothing gets rebound
;; - Just add repeat maps to reduce modifier-key RSI
;; - Simple, focused, minimal
;;
;;; Code:

;;; ============================================================================
;;; ENSURE REPEAT-MODE IS ENABLED
;;; ============================================================================

;; repeat-mode is built-in since Emacs 28
(repeat-mode 1)

;; Timeout for repeat mode (nil = no timeout)
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

;;; ============================================================================
;;; XREF NAVIGATION REPEAT MAP
;;; ============================================================================
;;
;; Make xref-go-back and xref-go-forward repeatable
;; Keep vanilla bindings: M-. and M-,
;; Just make M-, repeatable so you can tap: M-, , , ,
;;

(defvar xref-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ",") #'xref-go-back)
    (define-key map (kbd ".") #'xref-find-definitions)
    map)
  "Repeat map for xref navigation.")

(put 'xref-go-back 'repeat-map 'xref-repeat-map)
(put 'xref-find-definitions 'repeat-map 'xref-repeat-map)

;; Also make xref-go-forward repeatable if you use it
(when (fboundp 'xref-go-forward)
  (define-key xref-repeat-map (kbd "n") #'xref-go-forward)
  (put 'xref-go-forward 'repeat-map 'xref-repeat-map))

;;; ============================================================================
;;; USAGE GUIDE
;;; ============================================================================

;; S-EXP NAVIGATION:
;;   C-M-f once, then tap: f f f (forward 3 times)
;;   C-M-b once, then tap: b b (back 2 times)
;;   C-M-u once, then tap: u (up), d (down), e (end), etc.
;;
;; Available keys in repeat mode:
;;   f - forward-sexp
;;   b - backward-sexp
;;   u - backward-up-list
;;   d - down-list
;;   n - forward-list (next sibling)
;;   p - backward-list (previous sibling)
;;   a - beginning-of-defun
;;   e - end-of-defun
;;   SPC - mark-sexp
;;   k - kill-sexp
;;   w - copy-sexp
;;   t - transpose-sexps
;;
;; XREF NAVIGATION:
;;   M-. to jump to definition (vanilla)
;;   M-, to jump back, then tap: , , , (back 3 more times)
;;   Or tap: . to jump to definition again
;;
;; TIMEOUT:
;;   Repeat mode exits after 3 seconds of inactivity
;;   Or press any other key to exit immediately

(provide 'my-vanilla-plus)
;;; my-vanilla-plus.el ends here

