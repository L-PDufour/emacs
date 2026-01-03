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
  
  ;; Use faster connection method (sshx is often faster than scp)
  (setq tramp-default-method "ssh")
  
  ;; Disable expensive operations
  (setq remote-file-name-inhibit-locks t)
  (setq remote-file-name-inhibit-cache nil)  ; enable caching
  (setq tramp-verbose 1)  ; reduce logging overhead (0-10)
  
  ;; VC optimizations
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq vc-handled-backends '(Git))  ; limit to just Git if that's all you use
  
  ;; SSH ControlMaster (major speedup)
  (setq tramp-use-ssh-controlmaster-options t)
  (setq tramp-ssh-controlmaster-options
        (concat "-o ControlMaster=auto "
                "-o ControlPath=~/.ssh/sockets/%%r@%%h:%%p "
                "-o ControlPersist=600"))
  
  ;; File operations
  (setq tramp-auto-save-directory (expand-file-name "tramp-autosave/" user-emacs-directory))
  (setq tramp-copy-size-limit nil)
  (setq tramp-inline-compress-start-size 1000000)
  
  ;; Emacs 31+: use pipe for better performance
  (setq tramp-use-connection-share t)
  
  ;; Cache remote file properties longer
  (setq tramp-completion-reread-directory-timeout nil)
  
  ;; Disable projectile on remote
  (with-eval-after-load 'projectile
    (defadvice projectile-project-root (around ignore-remote first activate)
      (unless (file-remote-p default-directory)
        ad-do-it))))

(use-package envrc
  :diminish envrc-mode
  :hook (after-init . envrc-global-mode)
  :config
  ;; Enable envrc on remote files (for nix-shell support)
  (setq envrc-remote t))

(provide 'my-utils)
;;; my-utils.el ends here
