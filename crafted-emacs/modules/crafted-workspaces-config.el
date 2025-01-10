;;; crafted-workspaces-config.el --- Workspaces configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community
;; Keywords: project, workspace

;;; Commentary:

;; Use tabspaces to manage workspaces

;;; Code:

(use-package tabspaces
  :hook (after-init . tabspaces-mode) ;; enable tabspaces at startup
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-session t)
  (tabspaces-session-auto-restore t))

;; If you want to integrate with project management
(with-eval-after-load 'project
  (setq project-switch-commands #'tabspaces-project-switch-project-open-file))
;; Make sure project is initialized
(project--ensure-read-project-list)

(provide 'crafted-workspaces-config)
;;; crafted-workspaces-config.el ends here
