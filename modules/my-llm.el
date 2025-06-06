;;; my-llm.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; Variable to cache the API key during session
(defvar my-anthropic-api-key nil
  "Cached Anthropic API key for current session.")

;; Function to get API key (prompt once, then cache)
(defun my-get-anthropic-key ()
  "Get Anthropic API key, prompting if not cached."
  (unless my-anthropic-api-key
	(setq my-anthropic-api-key
		  (read-passwd "Enter Anthropic API key: ")))
  my-anthropic-api-key)
(use-package gptel
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-backend (gptel-make-anthropic "Claude"
						:stream t
						:key #'my-get-anthropic-key)))

(provide 'my-llm)
;;; my-llm.el ends here
