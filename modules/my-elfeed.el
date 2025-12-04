;;; my-elfeed.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package elfeed
  :custom
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)))

(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files
   (list (expand-file-name "elfeed.org" user-emacs-directory)))
  :hook
  (after-save . (lambda ()
                  (when (and (buffer-file-name)
                             (string-match-p "elfeed\\.org$" (buffer-file-name)))
                    (elfeed-org)
                    (message "Elfeed feeds reloaded!")))))

(use-package elfeed-tube
  :ensure nil
  :after elfeed
  :config
  (elfeed-tube-setup))

(provide 'my-elfeed)
;;; my-elfeed.el ends here
