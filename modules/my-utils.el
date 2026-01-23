;;; my-utils.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package pdf-tools
  :ensure nil
  :mode ("\\.pdf\\'" . pdf-view-mode))

(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  
  (setq tramp-default-method "ssh")
  (setq remote-file-name-inhibit-locks t)
  (setq tramp-verbose 1)
  
  ;; SSH ControlMaster
  (setq tramp-use-ssh-controlmaster-options t)
  (setq tramp-ssh-controlmaster-options
        (concat "-o ControlMaster=auto "
                "-o ControlPath=~/.ssh/sockets/%%r@%%h:%%p "
                "-o ControlPersist=600"))
  
  (setq tramp-auto-save-directory (expand-file-name "tramp-autosave/" user-emacs-directory)))

(use-package envrc
  :diminish envrc-mode
  :hook (after-init . envrc-global-mode)
  :config
  ;; Enable envrc on remote files (for nix-shell support)
  (setq envrc-remote t))

(provide 'my-utils)
;;; my-utils.el ends here
