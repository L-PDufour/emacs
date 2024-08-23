;; init.el
(require 'use-package)
(load-file (expand-file-name "config.el" (file-name-directory load-file-name)))

;; config.el
(use-package evil
  :config
  (evil-mode 1))

(use-package which-key
  :config
  (which-key-mode))

;; Add more configuration for your packages here
