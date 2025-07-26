;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'aggressive t)
 '(bookmark-save-flag 1)
 '(completion-cycle-threshold 3)
 '(completions-detailed t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain t)
 '(eglot-autoshutdown t)
 '(eshell-scroll-to-bottom-on-input 'this)
 '(fancy-splash-image
   "/home/desktop/.emacs.d/crafted-emacs/system-crafters-logo.png")
 '(fast-but-imprecise-scrolling t)
 '(global-auto-revert-non-file-buffers t)
 '(ibuffer-movement-cycle nil)
 '(ibuffer-old-time 24)
 '(kill-do-not-save-duplicates t)
 '(load-prefer-newer t)
 '(marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
 '(org-agenda-files
   '("/home/desktop/Sync/org/family/recipes/recipes.org"
	 "/home/desktop/Sync/org/family/shoppingList.org"
	 "/home/desktop/Sync/org/family/tasks.org"
	 "/home/desktop/Sync/org/school/IFT-1003.org"
	 "/home/desktop/Sync/org/school/IFT-1004.org"
	 "/home/desktop/Sync/org/school/tasks.org"
	 "/home/desktop/Sync/org/inbox.org"))
 '(org-hide-emphasis-markers t)
 '(org-link-descriptive t)
 '(org-mouse-1-follows-link t)
 '(org-return-follows-link t)
 '(package-archive-priorities
   '(("gnu" . 99) ("nongnu" . 80) ("stable" . 70) ("melpa" . 0)))
 '(package-selected-packages
   '(aggressive-indent apheleia avy cape catppuccin-theme combobulate
					   corfu dape diff-hl diminish dired-subtree
					   dumber-jump eat eglot-booster
					   eglot-signature-eldoc-talkative eglot-tempel
					   embark-consult envrc flymake-eslint go-mode
					   gptel ibuffer-project lsp-mode lsp-ui lua-mode
					   magit marginalia markdown-mode meow-tree-sitter
					   nerd-icons-completion nerd-icons-corfu
					   nerd-icons-dired nix-mode orderless org-appear
					   org-pdftools rainbow-delimiters
					   tempel-collection templ-ts-mode trashed
					   treesit-auto undo-fu-session vertico
					   visual-fill-column vundo))
 '(package-vc-selected-packages
   '((eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
	 (combobulate :url "https://github.com/mickeynp/combobulate")
	 (catppuccin-theme :url "https://github.com/catppuccin/emacs"
					   :branch "master")))
 '(safe-local-variable-values
   '((smie-indent-basic . 2)
	 (eval setq-local dape-configs
		   '((js-debug-node :type "pwa-node" :request "attach" :port
							9229 :hostname "localhost" :sourceMaps t
							:cwd default-directory :program
							"web/assets/js/main.js")
			 (js-debug-browser :type "pwa-chrome" :request "launch"
							   :url "http://localhost:3000" :webRoot
							   default-directory :sourceMaps t)))
	 (dape-configs
	  (js-debug-node :type "pwa-node" :request "attach" :port 9229
					 :hostname "localhost" :sourceMaps t :cwd
					 default-directory :program
					 "web/assets/js/main.js")
	  (js-debug-browser :type "pwa-chrome" :request "launch" :url
						"http://localhost:3000" :webRoot
						default-directory :sourceMaps t))
	 (dape-configs
	  (js-debug-browser :type "pwa-chrome" :request "launch" :url
						"http://localhost:3000" :webRoot
						default-directory :sourceMaps t))))
 '(scheme-program-name "guile")
 '(scroll-conservatively 101)
 '(scroll-margin 0)
 '(scroll-preserve-screen-position t)
 '(switch-to-buffer-in-dedicated-window 'pop)
 '(switch-to-buffer-obey-display-actions t)
 '(tabspaces-include-buffers '("*scratch*") nil nil "Customized with use-package tabspaces")
 '(tabspaces-mode t)
 '(tabspaces-remove-to-default t nil nil "Customized with use-package tabspaces")
 '(tabspaces-use-filtered-buffers-as-default t nil nil "Customized with use-package tabspaces")
 '(treesit-auto-langs '(go java javascript lua latex markdown python typescript)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:background "#292c3c" :foreground "#626880"))))
 '(line-number-current-line ((t (:background "#292c3c" :foreground "#babbf1")))))
