;;; my-llm.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package gptel
  :straight t
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'test
        gptel-backend(gptel-make-openai "llama-cpp"
                       :stream t
                       :protocol "http"
                       :host "localhost:8080"
                       :models '(test))))

(provide 'my-llm)
;;; my-llm.el ends here
