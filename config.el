;; config.el

;; ... (your existing configuration)

;; Org mode configuration
(use-package org
  :config
  (setq org-startup-indented t)  ; Enable org-indent-mode by default
  (setq org-startup-folded nil)  ; Start with all sections expanded
  (setq org-hide-emphasis-markers t)  ; Hide markup symbols like *bold* /italic/
  (setq org-pretty-entities t)  ; Display entities like \alpha as UTF-8 characters
  (setq org-agenda-files '("~/org/"))  ; Set default directory for agenda files

  ;; Basic key bindings
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)

  ;; Custom TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

  ;; Org-capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))

  ;; Enable org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))

;; You can add more configuration hereation here

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha) ;; or 'latte, 'frappe, 'macchiato, or 'mocha
  (load-theme 'catppuccin t)
  (setq catppuccin-highlight-matches t)
  (setq catppuccin-italic-comments t)
  (setq catppuccin-italic-keywords t))
