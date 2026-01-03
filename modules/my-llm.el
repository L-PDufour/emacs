;;; my-llm.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar my-anthropic-api-key nil
  "Cached Anthropic API key for current session.")

(defun my-get-anthropic-key ()
  "Get Anthropic API key, prompting if not cached."
  (unless my-anthropic-api-key
    (setq my-anthropic-api-key
          (read-passwd "Enter Anthropic API key: ")))
  my-anthropic-api-key)

(use-package gptel
  :config
  (setq gptel-default-mode 'org-mode)
  
  ;; Set the model BEFORE creating the backend
  (setq gptel-model "claude-sonnet-4-20250514")
  
  (setq gptel-backend 
        (gptel-make-anthropic "Claude"
          :stream t
          :key #'my-get-anthropic-key
          :models '(claude-sonnet-4-20250514      ; Default: best balance
                    claude-haiku-4-20250119))))   ; Fast and cheap

;; Load gptel tools and RAG configuration
(require 'my-gptel-tools)

(provide 'my-llm)
;;; my-llm.el ends here
