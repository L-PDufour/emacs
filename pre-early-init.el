;;; pre-early-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;; (setq debug-on-error t)
(setq minimal-emacs-ui-features '(context-menu menu-bar dialogs tooltips))
(setq user-emacs-directory (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
;; NOTE: This must be placed in 'pre-early-init.el'.
(when (featurep 'native-compile)
  ;; Ensure Emacs consistently uses the specified ELN cache directory for native
  ;; compilation, preventing it from creating or loading .eln files in the
  ;; default system or user-standard paths. This guarantees that both loading
  ;; and writing of native-compiled files happen exclusively in the user-defined
  ;; cache directory, avoiding permission issues and path inconsistencies.
  (let ((eln-cache-dir (convert-standard-filename
                        (expand-file-name "eln-cache" user-emacs-directory))))
    (when (boundp 'native-comp-eln-load-path)
      (setcar native-comp-eln-load-path eln-cache-dir))
    (setq native-compile-target-directory eln-cache-dir)
    (when (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache eln-cache-dir))))
(provide 'pre-early-init)


;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; pre-early-init.el ends here
