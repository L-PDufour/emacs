;;; my-org.el ---                                    -*- lexical-binding: t; -*-
;;; Commentary: org
;;; Code:
(use-package org
  :ensure nil
  :custom
  (org-directory "~/Sync/org")
  
  :config
  (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
  
  (setq org-agenda-files
        (append
         (directory-files-recursively org-directory "\\.org$")  ;; Recursively find all `.org` files
         (list org-default-notes-file)))  ;; Optionally add your default notes file

  
  ;; Archive location and basic settings
  (setq org-archive-location (concat org-directory "/archive/%s_archive::"))
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-startup-folded t)
  (setq org-startup-indented t)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  ;; Enhanced TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "IN-PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Enhanced tags
  (setq org-tag-alist
        '((:startgroup . nil)
          ("@home" . ?h)
          ("@shopping" . ?s)
          ("@school" . ?e)
          ("@campus" . ?c)
          ("@health" . ?m)
          (:endgroup . nil)
          ("urgent" . ?u)
          ("shared" . ?r)
          ("recipe" . ?p)
          ("assignment" . ?a)
          ("exam" . ?x)
          ("reading" . ?d)))

  ;; Enhanced capture templates
  (setq org-capture-templates
        '(("i" "Inbox" entry
           (file org-default-notes-file)
           "* TODO %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n\n%i\n\n%a")
          
          ("s" "School")
          ("st" "School Task" entry
           (file+olp ,(expand-file-name "school/tasks.org" org-directory) "Classes" "Current Semester")
           "* TODO %^{Task}\n:PROPERTIES:\n:COURSE: %^{Course}\n:DEADLINE: %^{Deadline}t\n:CATEGORY: School\n:END:\n%?")
          ("sn" "School Note" entry
           (file+olp ,(expand-file-name "school/tasks.org" org-directory) "Classes" "Current Semester")
           "* %^{Topic}\n:PROPERTIES:\n:COURSE: %^{Course}\n:DATE: %U\n:CATEGORY: School\n:END:\n%?")
          
          ("f" "Family Task" entry 
           (file+olp ,(expand-file-name "family/tasks.org" org-directory) "Family Tasks")
           "* TODO %?\n  %i\n  SCHEDULED: %^t\n  :PROPERTIES:\n  :CATEGORY: Family\n  :END:")
          
          ("r" "Recipe" entry
           (file ,(expand-file-name "recipes/recipes.org" org-directory))
           "* Recipe Name  :recipe:%^{Type|breakfast|meal|dessert|snack|beverage}:\n:PROPERTIES:\n:SERVINGS:\n:PREP_TIME:\n:COOK_TIME:\n:CATEGORY: Recipe\n:END:\nSource: [[%^{Recipe URL}][Original Recipe]]\n** Ingredients\n%?\n** Instructions\n** Notes\n")))

  ;; Create additional standard org subdirectories
  (let ((org-subdirs '("archive" "projects" "journal" "references")))
    (dolist (dir org-subdirs)
      (let ((subdir-path (expand-file-name dir org-directory)))
        (unless (file-exists-p subdir-path)
          (make-directory subdir-path)))))

  ;; Enhanced agenda views
  (setq org-agenda-custom-commands
        '(("h" "Home & Family"
           ((tags-todo "@home")
            (tags-todo "@shopping")
            (tags-todo "urgent")))
          ("s" "School Overview"
           ((tags-todo "assignment")
            (tags-todo "exam")
            (tags-todo "@school|@campus")))
          ("p" "Meal Planning"
           ((tags "recipe"
                  ((org-agenda-files (list (expand-file-name "recipes/recipes.org" org-directory)))))))))
  :hook (org-mode . auto-save-mode)
  :bind ("C-c c" . org-capture))

(provide 'my-org)
;;; my-org.el ends here
;; Copyright (C) 2025  desktop

;; Author: desktop <desktop@nixos>
