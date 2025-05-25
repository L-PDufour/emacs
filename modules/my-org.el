;;; my-org.el ---                                    -*- lexical-binding: t; -*-
;;; Commentary: org
;;; Code:
(use-package org-appear
  :ensure nil
  :hook(org-mode . org-appear-mode))


(use-package org
  :ensure nil
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-return-follows-link t)
  (setq org-mouse-1-follows-link t)
  (setq org-link-descriptive t)
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
           (file+headline org-default-notes-file "Inbox")
           "* TODO %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n\n%i\n\n%a")))

  (defun crafted-org-enhance-electric-pair-inhibit-predicate ()
    "Disable auto-pairing of \"<\" in `org-mode' when using `electric-pair-mode'."
    (when (and electric-pair-mode (eql major-mode #'org-mode))
      (setq-local electric-pair-inhibit-predicate
                  `(lambda (c)
                     (if (char-equal c ?<)
                         t
                       (,electric-pair-inhibit-predicate c))))))
  :hook ((org-mode . auto-save-mode)
         (org-mode . org-indent-mode)
         (org-mode . org-appear-mode)
         (electric-pair-mode . crafted-org-enhance-electric-pair-inhibit-predicate)
         (org-mode . crafted-org-enhance-electric-pair-inhibit-predicate))


  :bind ("C-c c" . org-capture))

(provide 'my-org)
;;; my-org.el ends here
;; Copyright (C) 2025  desktop

;; Author: desktop <desktop@nixos>
