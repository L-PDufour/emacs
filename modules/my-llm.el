;;; my-llm.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package gptel
  :ensure nil
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'test
		gptel-backend(gptel-make-anthropic "Claude"
										   :stream t
										   :key (lambda () (getenv "ANTHROPIC_API_KEY")))))


(provide 'my-llm)
;;; my-llm.el ends here
