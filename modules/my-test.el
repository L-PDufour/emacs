;;; my-test.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package tab-bar
  :ensure nil
  :defer t
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-auto-width nil)
  (tab-bar-separator " ")
  (tab-bar-format '(tab-bar-format-tabs-groups
					Tab-bar-format-tabs tab-bar-separator
					tab-bar-format-add-tab))
  :init
  ;;; --- OPTIONAL INTERNAL FN OVERRIDES TO DECORATE NAMES
  (defun tab-bar-tab-name-format-hints (name _tab i)
	(if tab-bar-tab-hints (concat (format "»%d«" i) "") name))

  (defun tab-bar-tab-group-format-default (tab _i &optional current-p)
	(propertize
	 (concat (funcall tab-bar-tab-group-function tab))
	 'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))


  ;;; --- UTILITIES FUNCTIONS
  (defun emacs-solo/tab-group-from-project ()
	"Call `tab-group` with the current project name as the group."
	(interactive)
	(when-let* ((proj (project-current))
				(name (file-name-nondirectory
					   (directory-file-name (project-root proj)))))
	  (tab-group (format "[%s]" name))))

  (defun emacs-solo/tab-switch-to-group ()
    "Prompt for a tab group and switch to its first tab.
Uses position instead of index field."
    (interactive)
    (let* ((tabs (funcall tab-bar-tabs-function)))
	  (let* ((groups (delete-dups (mapcar (lambda (tab)
										    (funcall tab-bar-tab-group-function tab))
										  tabs)))
		     (group (completing-read "Switch to group: " groups nil t)))
	    (let ((i 1) (found nil))
		  (dolist (tab tabs)
		    (let ((tab-group (funcall tab-bar-tab-group-function tab)))
			  (when (and (not found)
					     (string= tab-group group))
			    (setq found t)
			    (tab-bar-select-tab i)))
		    (setq i (1+ i)))))))

  ;;; --- EXTRA KEYBINDINGS
  (global-set-key (kbd "C-x t P") #'emacs-solo/tab-group-from-project)
  (global-set-key (kbd "C-x t g") #'emacs-solo/tab-switch-to-group)

  ;;; --- TURNS ON BY DEFAULT
  (tab-bar-mode 1))

(use-package devdocs)

;; Easy insertion of weblinks
(use-package org-web-tools)
(use-package wgrep)
(use-package breadcrumb)



(use-package project
  :ensure nil
  :bind (:map project-prefix-map
              ("t" . eat-project))
  :custom
  (project-switch-use-entire-map t))

(provide 'my-test)
;;; my-test.el ends here
