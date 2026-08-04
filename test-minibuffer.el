;;; test-minibuffer.el --- Default *Completions* playground -*- lexical-binding: t; -*-

;;; Commentary:

;; The built-in minibuffer/in-buffer completion UI with nothing else
;; around it.  Run it against a stock Emacs, without touching the real
;; config:
;;
;;     emacs -Q --load test-minibuffer.el
;;
;; Emacs 31 or newer: `completion-eager-display', `completion-eager-update'
;; and `minibuffer-visible-completions-up-down-map' are all new there.
;;
;; Adapted from Rahul M. Juliato, "The Completions Buffer Is Now Enough"
;; <https://www.rahuljuliato.com/posts/completions-buffer-is-now-enough>,
;; with a few settings from Protesilaos Stavrou's "Emacs: default
;; minibuffer completions overview (Emacs 31)"
;; <https://protesilaos.com/codelog/2026-07-29-emacs-default-minibuffer-completion-overview/>.
;;
;; Things to try once it is loaded:
;;
;; - `M-x' and type a few letters, then C-n / C-p through the list.
;;   Nothing was pressed to summon it: that is `completion-eager-display'.
;; - `C-x C-f' and walk a directory tree with the same keys.
;; - In *scratch*, type (window-lay and hit TAB twice.
;; - Focus any prompt, `M-:', and evaluate (my/completion-category) to
;;   learn which key to write a `completion-category-overrides' entry for.
;;
;; Note the difference from the real config: this file uses the stock
;; `flex' style, whose `try-completion' merges the surviving candidates
;; and inserts the result.  With `tab-always-indent' set to `complete'
;; that means TAB types a candidate at you — often a far, wrong one —
;; before you ever see the list.  `flex-noinsert' below keeps flex's
;; filtering and scoring and drops the merge.  The real config runs
;; Orderless, which never merges, so it does not need this.

;;; Code:

(defun my/minibuffer-truncate-lines ()
  "Keep minibuffer lines unwrapped."
  (setq truncate-lines t))

(defun my/completion-category ()
  "Return the completion category of the active minibuffer prompt."
  (when-let* ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (completion-metadata-get
       (completion-metadata (buffer-substring-no-properties
                             (minibuffer-prompt-end)
                             (max (minibuffer-prompt-end) (point)))
                            minibuffer-completion-table
                            minibuffer-completion-predicate)
       'category))))

(defun my/flex-noinsert-try-completion (string table pred point)
  "Flex `try-completion' that never auto-extends the input on TAB.

Keeps flex's filtering and scoring but suppresses the merge:

  - no candidates           -> nil   (no match)
  - exactly one candidate   -> complete it fully
  - two or more candidates  -> return STRING unchanged, so TAB only
                               pops the *Completions* list.

STRING, TABLE, PRED and POINT are the usual `try-completion' args."
  (let ((all (completion-flex-all-completions string table pred point)))
    (cond
     ((null all) nil)
     ((= (safe-length all) 1)
      (let ((sole (car all)))
        (if (string= sole string) t (cons sole (length sole)))))
     (t (cons string point)))))

(use-package minibuffer
  :ensure nil
  :demand t
  :hook ((minibuffer-setup . cursor-intangible-mode)
         (minibuffer-setup . my/minibuffer-truncate-lines))
  :custom
  (tab-always-indent 'complete)
  (completion-auto-help t)
  (completion-auto-select t)
  (completion-show-help nil)
  (completion-show-inline-help nil)
  (completion-ignore-case t)
  (completion-styles '(partial-completion flex initials))
  (completion-category-overrides '((eglot-capf (styles flex-noinsert))))
  (completions-format 'one-column)
  (completions-max-height 10)
  (completions-sort 'historical)
  (completions-detailed t)
  (enable-recursive-minibuffers t)
  (read-minibuffer-restore-windows nil)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  :config
  (savehist-mode 1)                     ; so `completions-sort' persists
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  ;; Register the `flex-noinsert' style: same filtering and sorting as
  ;; `flex', but with the wrapper above as its try function.
  (add-to-list 'completion-styles-alist
               '(flex-noinsert
                 my/flex-noinsert-try-completion
                 completion-flex-all-completions
                 "Flex matching that never extends input on TAB."))
  ;; Reuse flex's metadata tweak so *Completions* sorts by flex score.
  (put 'flex-noinsert 'completion--adjust-metadata
       'completion--flex-adjust-metadata)

  ;; The two settings the whole thing rests on: show *Completions*
  ;; without being asked, and refresh it as you type.  Emacs 31.
  (when (boundp 'completion-eager-display)
    (setq completion-eager-display t
          completion-eager-update t))

  ;; Point stays in the minibuffer while C-n / C-p walk the visible
  ;; list; `up-down' rather than t leaves left/right moving in the input.
  (if (boundp 'minibuffer-visible-completions-up-down-map)
      (progn
        (setq minibuffer-visible-completions 'up-down)
        (keymap-set minibuffer-visible-completions-up-down-map
                    "C-n" #'minibuffer-next-completion)
        (keymap-set minibuffer-visible-completions-up-down-map
                    "C-p" #'minibuffer-previous-completion))
    (setq minibuffer-visible-completions t)))

(provide 'test-minibuffer)
;;; test-minibuffer.el ends here
