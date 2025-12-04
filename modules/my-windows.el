;;; my-windows.el --- Window management with Popper -*- lexical-binding: t -*-
;;; Commentary:
;; Modern popup window management using popper package

;;; Code:

;; ============================================
;; POPPER - Modern Popup Management
;; ============================================

(use-package popper
  :bind (("M-o"   . popper-toggle)       ;; Toggle last popup
         ("M-O"   . popper-cycle)         ;; Cycle through popups
         ("C-M-o" . popper-toggle-type))  ;; Convert between popup/normal

  :custom
  ;; Buffers that should be treated as popups
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*Warnings\\*"
     "\\*Compile-Log\\*"
     "\\*compilation\\*"
     "\\*eldoc.*\\*"
     "\\*Flymake diagnostics\\*"
     "\\*xref\\*"
     "\\*lsp-help\\*"
     "\\*Backtrace\\*"
     ;; Shells
     "\\*e?shell\\*"
     "\\*term\\*"
     "\\*eat\\*"
     "\\*vterm\\*"
     ;; Help modes
     help-mode
     helpful-mode
     apropos-mode
     ;; Other useful modes
     compilation-mode
     flymake-diagnostics-buffer-mode
     messages-buffer-mode))

  ;; Where to display popups (bottom by default)
  (popper-display-control t)

  ;; Popup window height as fraction of frame
  (popper-window-height 0.33)

  ;; Group popups by project
  (popper-group-function #'popper-group-by-project)

  :init
  ;; Enable popper mode globally
  (popper-mode +1)
  ;; Show popup names in mode line
  (popper-echo-mode +1)

  :config
  ;; Optional: Customize display function
  (setq popper-display-function #'popper-select-popup-at-bottom))

;; ============================================
;; ELDOC CONFIGURATION
;; ============================================

(use-package eldoc
  :ensure nil
  :diminish
  :custom
  ;; Show in echo area by default (minibuffer)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-idle-delay 0.5)

  ;; If you want eldoc in a popup buffer instead:
  ;; (eldoc-display-functions '(eldoc-display-in-buffer))

  :hook (prog-mode . eldoc-mode))

;; ============================================
;; WINDOW DISPLAY RULES (Fallback)
;; ============================================
(use-package window
  :ensure nil
  :custom
  ;; Better window splitting
  (split-width-threshold 170)
  (split-height-threshold nil)

  ;; Display rules for buffers
  ;; These work WITH popper - popper overrides for its managed buffers
  (display-buffer-alist
   '(
     ;; Vertico/Consult completions should take over current window
     ;; or reuse existing window - NOT create popups
     ("\\*Completions\\*"
      (display-buffer-reuse-window display-buffer-same-window)
      (reusable-frames . visible))

     ;; Embark collect should be in main window
     ("\\*Embark Collect"
      (display-buffer-reuse-window display-buffer-same-window))
     ("\\*eldoc\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     ;; Magit in full window
     ("\\*magit.*\\*"
      (display-buffer-same-window)
      (inhibit-same-window . nil))

     ;; Dape debugger on the right
     ("\\*dape-.*\\*"
      (display-buffer-in-side-window)
      (window-width . 0.35)
      (side . right)
      (slot . 0))

     ;; Org capture in full window
     ("\\*Org Select\\*"
      (display-buffer-same-window))
     
     ;; Consult buffers should use same window
     ("\\*consult-.*\\*"
      (display-buffer-same-window)))))

;; ============================================
;; HELPFUL CONFIGURATION
;; ============================================
(use-package helpful
  :commands (helpful-callable

             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 5)

  :config
  ;; Make 'q' close helpful buffers
  (define-key helpful-mode-map (kbd "q") #'quit-window)
  (define-key helpful-mode-map (kbd "C-g") #'quit-window))

;; ============================================
;; WINNER MODE
;; ============================================

(use-package winner
  :ensure nil
  :config
  (winner-mode 1)
  :bind
  (("C-c w u" . winner-undo)
   ("C-c w r" . winner-redo)))

;; ============================================
;; WINDOW NAVIGATION
;; ============================================



;; Or keep your existing C-c w h/j/k/l bindings
;; (Already defined in my-keybinds.el)

;; ============================================
;; ADDITIONAL HELPER FUNCTIONS
;; ============================================

(defun my/close-all-popups ()
  "Close all popper popup windows."
  (interactive)
  (popper-close-latest))

(defun my/maximize-window ()
  "Maximize current window (delete others)."
  (interactive)
  (delete-other-windows))

(global-set-key (kbd "C-c w m") #'my/maximize-window)
(global-set-key (kbd "C-c w c") #'my/close-all-popups)
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1) ; affects `scroll-step'
(setq-default scroll-margin 0)
(provide 'my-windows)
;;; my-windows.el ends here
