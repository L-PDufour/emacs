;;; vim-tab-bar-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from vim-tab-bar.el

(defvar vim-tab-bar-mode nil "\
Non-nil if Vim-Tab-Bar mode is enabled.
See the `vim-tab-bar-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vim-tab-bar-mode'.")
(custom-autoload 'vim-tab-bar-mode "vim-tab-bar" nil)
(autoload 'vim-tab-bar-mode "vim-tab-bar" "\
Toggle `vim-tab-bar-mode'.

This is a global minor mode.  If called interactively, toggle the
`Vim-Tab-Bar mode' mode.  If the prefix argument is positive, enable the
mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vim-tab-bar-mode)'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "vim-tab-bar" '("vim-tab-bar-"))

;;; End of scraped data

(provide 'vim-tab-bar-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; vim-tab-bar-autoloads.el ends here