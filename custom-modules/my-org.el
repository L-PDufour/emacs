;;; my-org.el ---                                    -*- lexical-binding: t; -*-
;;; Commentary: org
;;; Code:
(use-package org
  :ensure nil
  :config
    (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-directory "~/Sync/org")
  (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
  
  (setq org-agenda-files
        (append
         (directory-files-recursively org-directory "\\.org$")  ;; Recursively find all `.org` files
         (list org-default-notes-file)))  ;; Optionally add your default notes file

  
  ;; Archive location and basic settings
  (setq org-archive-location (concat org-directory "/archive/%s_archive::"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
  ;; Enhanced TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)"  "|" "DONE(d)" "CANCELLED(c)")))

 
      ;; Enhanced capture templates
  (setq org-capture-templates
        '(("i" "Inbox" entry
           (file org-default-notes-file)
           "* TODO %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n\n%i\n\n%a")))
    
  :hook (org-mode . auto-save-mode)
  :bind ("C-c c" . org-capture))

(provide 'my-org)
;;; my-org.el ends here
;; Copyright (C) 2025  desktop

;; Author: desktop <desktop@nixos>
