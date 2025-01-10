;;; early-init.el --- Crafted Emacs Base Example -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Base example early-init.el
;; Modified from the info file version to be runnable.

;;; Code:


;; Adjust the path (e.g. to an absolute one)
;; depending where you cloned Crafted Emacs.

(setq gc-cons-threshold most-positive-fixnum)
(load "~/.emacs.d/crafted-emacs/modules/crafted-early-init-config")
(setq visible-bell t)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;; _
(provide 'early-init)
;;; early-init.el ends here
