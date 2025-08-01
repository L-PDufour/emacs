;;; my-meow.el --- Meow configuration  -*- lexical-binding: t; -*-
;; Copyright (C) 2025  desktop

;;; Commentary:
;; Configuration for the Meow modal editing package.

;;; Code:

(use-package meow
  :config
  (defun meow-setup ()
	(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
	(setq meow-use-clipboard t)
	(setq meow-selection-command-fallback
		  '((meow-change . meow-change-char)
			;; (meow-kill . meow-c-k)
			(meow-kill . meow-delete)
			;; (meow-cancel-selection . keyboard-quit)
			(meow-cancel-selection . ignore)
			(meow-pop-selection . meow-pop-grab)
			(meow-beacon-change . meow-beacon-change-char)))
	;; (meow-motion-define-key
	;;  '("j" . meow-next)
	;;  '("k" . meow-prev)
	;;  '("<escape>" . ignore))
	(meow-leader-define-key
	 ;; Use SPC (0-9) for digit arguments.
	 '("1" . meow-digit-argument)
	 '("2" . meow-digit-argument)
	 '("3" . meow-digit-argument)
	 '("4" . meow-digit-argument)
	 '("5" . meow-digit-argument)
	 '("6" . meow-digit-argument)
	 '("7" . meow-digit-argument)
	 '("8" . meow-digit-argument)
	 '("9" . meow-digit-argument)
	 '("0" . meow-digit-argument)
     '("SPC" . consult-buffer)
	 '("/" . meow-keypad-describe-key)
	 '("?" . meow-cheatsheet))

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-kill)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . keyboard-escape-quit)))
  (meow-setup)
  (meow-setup-indicator)
  (meow-global-mode 1))

(use-package meow-tree-sitter
  :ensure t
  :after meow
  :config
  (meow-tree-sitter-register-defaults))

(provide 'my-meow)
;;; my-meow.el ends here
