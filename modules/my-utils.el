;;; my-utils.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package pdf-tools
  :ensure nil
  :mode ("\\.pdf\\'" . pdf-view-mode))

(use-package tramp
  :ensure nil
  :config
  ;; Add remote PATH
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Performance: Disable file locks on remote files
  (setq remote-file-name-inhibit-locks t)

  ;; Performance: Exclude remote files from version control checks
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  ;; Performance: Use persistent SSH connections (ControlMaster)
  (setq tramp-use-ssh-controlmaster-options t)

  ;; Performance: Disable auto-save on remote files
  (setq tramp-auto-save-directory (expand-file-name "tramp-autosave/" user-emacs-directory))

  ;; Performance: Copy files in chunks
  (setq tramp-copy-size-limit nil)
  (setq tramp-inline-compress-start-size 1000000)

  ;; Performance: Optimize connection sharing
  (setq tramp-connection-properties
        '((nil "remote-shell" "/bin/sh")))

  ;; Performance: Disable projectile on remote files
  (defadvice projectile-project-root (around ignore-remote first activate)
    "Ignore remote files for projectile."
    (unless (file-remote-p default-directory)
      ad-do-it)))

(use-package envrc
  :diminish envrc-mode
  :hook (after-init . envrc-global-mode)
  :config
  ;; Enable envrc on remote files (for nix-shell support)
  (setq envrc-remote t))

(provide 'my-utils)
;;; my-utils.el ends here
