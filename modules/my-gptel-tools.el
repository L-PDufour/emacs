;;; my-gptel-tools.el --- GPtel Tools and RAG Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This file provides example tools and RAG capabilities for gptel.
;;
;; USAGE:
;; 1. Tools: Access via `M-x gptel-menu` (or `C-u` before sending)
;; 2. Context/RAG: Use `gptel-add` or `gptel-add-file` to include documents
;; 3. Multi-modal: Enable `gptel-track-media` for images/documents
;;
;; EXAMPLES:
;; - Ask: "What's in my init.el?" (with read_buffer tool enabled)
;; - Ask: "Search for TODO comments" (with search_code tool enabled)
;; - Add file context: M-x gptel-add-file, select file, then ask questions
;; - In chat buffer: Use links [[file:path/to/file.org]] to include content
;;
;;; Code:

(require 'gptel)

;;; ============================================================================
;;; TOOL DEFINITIONS
;;; ============================================================================

;; Tool 1: Read Emacs buffer contents
(gptel-make-tool
 :name "read_buffer"
 :function (lambda (buffer)
             "Read the entire contents of an Emacs buffer."
             (unless (buffer-live-p (get-buffer buffer))
               (error "Buffer %s does not exist or is not live" buffer))
             (with-current-buffer buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :description "Read and return the complete contents of an Emacs buffer by name"
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the buffer to read, e.g. 'init.el' or '*scratch*'"))
 :category "emacs")

;; Tool 2: List open buffers
(gptel-make-tool
 :name "list_buffers"
 :function (lambda ()
             "List all open buffers with their modes."
             (mapconcat
              (lambda (buf)
                (format "%s (%s)"
                        (buffer-name buf)
                        (with-current-buffer buf
                          (symbol-name major-mode))))
              (buffer-list)
              "\n"))
 :description "List all currently open Emacs buffers with their major modes"
 :args nil
 :category "emacs")

;; Tool 3: Search code in project
(gptel-make-tool
 :name "search_code"
 :function (lambda (pattern)
             "Search for a pattern in the current project."
             (let ((default-directory (or (project-root (project-current))
                                          default-directory))
                   (results '()))
               (dolist (file (directory-files-recursively default-directory "\\.\\(el\\|org\\|py\\|js\\|ts\\)$"))
                 (with-temp-buffer
                   (insert-file-contents file)
                   (goto-char (point-min))
                   (while (re-search-forward pattern nil t)
                     (push (format "%s:%d: %s"
                                   (file-relative-name file default-directory)
                                   (line-number-at-pos)
                                   (string-trim (thing-at-point 'line t)))
                           results))))
               (if results
                   (mapconcat #'identity (nreverse results) "\n")
                 (format "No matches found for: %s" pattern))))
 :description "Search for a regex pattern in code files (.el, .org, .py, .js, .ts) within the current project"
 :args (list '(:name "pattern"
                     :type string
                     :description "The regex pattern to search for in project files"))
 :category "project")

;; Tool 4: Execute shell command
(gptel-make-tool
 :name "shell_command"
 :function (lambda (command)
             "Execute a shell command and return its output."
             (string-trim (shell-command-to-string command)))
 :description "Execute a shell command and return its output. Use for system operations like 'ls', 'git status', 'find', etc."
 :args (list '(:name "command"
                     :type string
                     :description "The shell command to execute"))
 :category "system")

;; Tool 5: Create or write file
(gptel-make-tool
 :name "create_file"
 :function (lambda (filepath content)
             "Create or overwrite a file with content."
             (let ((full-path (expand-file-name filepath)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Successfully created/updated: %s (%d bytes)"
                       filepath
                       (length content))))
 :description "Create a new file or overwrite an existing file with the specified content"
 :args (list '(:name "filepath"
                     :type string
                     :description "The path where to create the file (relative or absolute)")
             '(:name "content"
                     :type string
                     :description "The complete content to write to the file"))
 :category "filesystem")

;; Tool 6: Get current project info
(gptel-make-tool
 :name "get_project_info"
 :function (lambda ()
             "Get information about the current project."
             (if-let ((proj (project-current)))
                 (format "Project root: %s\nProject type: %s"
                         (project-root proj)
                         (type-of proj))
               "No project detected in current directory"))
 :description "Get information about the current Emacs project (root directory, type)"
 :args nil
 :category "project")

;; Tool 7: Describe function/variable
(gptel-make-tool
 :name "describe_symbol"
 :function (lambda (symbol-name)
             "Get documentation for an Emacs symbol (function or variable)."
             (let ((sym (intern-soft symbol-name)))
               (if (not sym)
                   (format "Symbol '%s' not found" symbol-name)
                 (with-temp-buffer
                   (if (fboundp sym)
                       (describe-function sym (current-buffer))
                     (describe-variable sym (current-buffer)))
                   (buffer-string)))))
 :description "Get documentation for an Emacs Lisp function or variable"
 :args (list '(:name "symbol_name"
                     :type string
                     :description "The name of the Emacs symbol (function or variable) to describe"))
 :category "emacs")

;;; ============================================================================
;;; RAG / CONTEXT CONFIGURATION
;;; ============================================================================

;; Enable tracking of media (images, documents) in chat buffers
(setq gptel-track-media t)

;; Configure context behavior
(setq gptel-context-max-tokens 100000) ; Adjust based on your model's limits

;; Helper function to quickly add current buffer as context
(defun my-gptel-add-current-buffer ()
  "Add the current buffer to gptel context."
  (interactive)
  (gptel-add (current-buffer))
  (message "Added %s to gptel context" (buffer-name)))

;; Helper function to add region to context
(defun my-gptel-add-region ()
  "Add the current region to gptel context."
  (interactive)
  (if (use-region-p)
      (progn
        (gptel-add (region-beginning) (region-end))
        (message "Added region to gptel context"))
    (user-error "No active region")))

;; Helper function to clear all context
(defun my-gptel-clear-context ()
  "Clear all gptel context."
  (interactive)
  (setq gptel-context--alist nil)
  (message "Cleared all gptel context"))

;; Helper function to show current context
(defun my-gptel-show-context ()
  "Show current gptel context in a temporary buffer."
  (interactive)
  (let ((context gptel-context--alist))
    (if (not context)
        (message "No context currently added")
      (with-current-buffer (get-buffer-create "*gptel-context*")
        (erase-buffer)
        (insert "=== Current GPtel Context ===\n\n")
        (dolist (item context)
          (insert (format "- %s\n" (car item))))
        (display-buffer (current-buffer))))))

;;; ============================================================================
;;; CONVENIENCE FUNCTIONS
;;; ============================================================================

(defun my-gptel-chat-with-context ()
  "Start a gptel chat session and prompt to add context files."
  (interactive)
  (gptel)
  (when (y-or-n-p "Add files/buffers as context? ")
    (call-interactively #'gptel-add)))

(defun my-gptel-ask-about-code ()
  "Ask a question about code in the current buffer using gptel."
  (interactive)
  (gptel-add (current-buffer))
  (gptel)
  (insert "Analyze the code I've provided and answer: ")
  (message "Current buffer added as context. Type your question."))

;;; ============================================================================
;;; KEY BINDINGS (Optional - uncomment to use)
;;; ============================================================================

(global-set-key (kbd "C-c g g") #'gptel)
(global-set-key (kbd "C-c g s") #'gptel-send)
(global-set-key (kbd "C-c g m") #'gptel-menu)
(global-set-key (kbd "C-c g a") #'gptel-add)
(global-set-key (kbd "C-c g f") #'gptel-add-file)
(global-set-key (kbd "C-c g b") #'my-gptel-add-current-buffer)
(global-set-key (kbd "C-c g r") #'my-gptel-add-region)
(global-set-key (kbd "C-c g c") #'my-gptel-clear-context)
(global-set-key (kbd "C-c g x") #'my-gptel-show-context)

;;; ============================================================================
;;; USAGE GUIDE
;;; ============================================================================

;; 1. USING TOOLS:
;;    - Open a gptel chat: M-x gptel
;;    - Before sending: C-u M-x gptel-send (or M-x gptel-menu)
;;    - Select which tools to enable
;;    - Ask questions that require tool use, e.g.:
;;      * "What buffers are currently open?" → uses list_buffers
;;      * "Show me my init.el file" → uses read_buffer
;;      * "Search for all TODO comments" → uses search_code
;;
;; 2. USING CONTEXT/RAG:
;;    Method A - Add files before asking:
;;      M-x gptel-add-file → select file(s)
;;      M-x gptel → ask questions about those files
;;
;;    Method B - Add current buffer:
;;      M-x my-gptel-add-current-buffer
;;      M-x gptel → ask about the buffer
;;
;;    Method C - Use links in chat (Markdown):
;;      [](file:///home/user/emacs/init.el)
;;      or
;;      [[file:/home/user/emacs/init.el]]
;;
;; 3. VIEWING CONTEXT:
;;    M-x my-gptel-show-context → see what's currently in context
;;    M-x my-gptel-clear-context → remove all context
;;
;; 4. EXAMPLE WORKFLOWS:
;;    Workflow A - Code Analysis:
;;      1. Open your code file
;;      2. M-x my-gptel-add-current-buffer
;;      3. M-x gptel
;;      4. Ask: "Explain this code and suggest improvements"
;;
;;    Workflow B - Multi-file RAG:
;;      1. M-x gptel-add-file (add multiple files)
;;      2. M-x gptel
;;      3. Ask: "How do these files interact?"
;;
;;    Workflow C - Tool-assisted development:
;;      1. M-x gptel
;;      2. C-u M-x gptel-send (enable tools: shell_command, create_file)
;;      3. Ask: "Create a Python script that lists all .org files"

(provide 'my-gptel-tools)
;;; my-gptel-tools.el ends here
