;;; forester.el --- a major mode for forester files

;; Copyright (C) 2025 Topos Institute

;; Author: Topos Staff <jason@topos.institute>
;; Maintainer: Topos Staff <jason@topos.institute>
;; Created: 2025
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (rainbow-delimiters "2.0"))
;; URL: https://github.com/ToposInstitute/forester.el
;; Keywords: forester

;; Forester.el is free software: you can redistribute it and/or modify it under
;; the terms of the MIT license, as included in the git repository.

;;; Commentary:

;; Forester.el enhances the forester experience by providing wrapper commands
;; around common tasks like creating new trees

;;; Code:

(defgroup forester-fonts
  nil ; No initial customization
  "Customization options for the Forester markup language"
  :group 'faces
  )

(defface forester-title
  '((t :inherit 'bold))
  "Forester title font. Bold for now."
  :group 'forester-fonts
  )

(defface forester-em
  '((t :inherit 'italic))
  "Forester title font. Bold for now."
  :group 'forester-fonts
  )

(defface forester-strong
  '((t :inherit 'bold))
  "Forester title font. Bold for now."
  :group 'forester-fonts
  )


(defface forester-builtin
  '((t :inherit font-lock-builtin-face))
  "Forester builtin functions"
  :group 'forester-fonts
  )

(defface forester-punctuation
  '((t :inherit font-lock-builtin-face))
  "Forester punctuation"
  :group 'forester-fonts
  )

(defface forester-address
  '((t :inherit 'underline))
  "Forester punctuation"
  :group 'forester-fonts
  )

(defvar forester-ts-font-lock-rules
  '(
    :language forester
    :feature title
    ;; :override t
    ;; There's an outer wrapper here which is semantically meaningless,
    ;; it's just a quotation operator.
    (((title (_)@forester-title)))

    :language forester
    :feature rich-text
    ((em "\\" "em" "{" (_)@forester-em "}")
     (strong "\\" "strong" "{" (_)@forester-strong "}")
     (addr (_)@forester-address)
     (ref "\\" "ref" "{" (_)@forester-address "}")
     (author "\\" "author" "{" (_)@forester-address "}")
     (markdown_link "[" (_) "]" "(" (_)@forester-address ")")
     )

    :language forester
    :feature comments
    ((comment) @font-lock-comment-face)
    )
  )

(defvar forester-ts-indent-rules
  '((forester
     ((parent-is "source_file") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "subtree") parent-bol 2)
     ((parent-is "ul") parent-bol 2)
     ((parent-is "ol") parent-bol 2)
     (catch-all parent-bol 0))))

(defun forester-ts-setup ()
  "Setup treesit for forester-ts-mode."
  ;; Our tree-sitter setup goes here.

  ;; This handles font locking
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     forester-ts-font-lock-rules))

  (setq-local treesit-font-lock-feature-list
	            '((inline-math title keyword builtin rich-text comments) () ()))

  ;; This handles indentation
  (setq-local treesit-simple-indent-rules forester-ts-indent-rules)

  ;; ... everything else we talk about go here also ...

  ;; End with this
  (treesit-major-mode-setup))

(defun forester--root ()
  (project-root (project-current)))

(defun forester--get-binary (binary)
  (let* ((local-forester (concat (forester--root) binary)))
    (if (file-exists-p local-forester)
        local-forester
      (executable-find binary))))

(defun forester--whoami ()
  (let ((whoami (concat (forester--root) ".whoami")))
    (if (file-exists-p whoami)
        (json-read-file whoami))))


;;; This is unused
(defun forester--start (&rest args)
  (let ((path (forester--get-binary "forester")))
    (apply #'start-process `(,path "*forester-output*" ,@args))))

(defun forester--call (&rest args)
  (let* ((path (forester--get-binary "forester"))
         (default-directory (forester--root)))
    (with-temp-buffer
      (apply #'call-process `(,path nil t nil ,@args))
      (buffer-string))))

(defun forester--new (&optional template namespace dest author no-title)
  (let* ((whoami (forester--whoami))
         (namespace (if (and whoami (not namespace))
                        (alist-get 'namespace whoami)))
         (author (if (and whoami (not author))
                     (alist-get 'author whoami)))
         (dest (if (and whoami (not dest))
                   (alist-get 'dest whoami)))
         (output (apply #'forester--call
                        `("new"
                          ,(concat "--dest=" dest)
                          ,(concat "--prefix=" namespace)
                          ,@(if template (list (concat "--template=" template)) '()))))
         (tree (car (last (split-string output "\n" t))))
         (treepath (concat (forester--root) tree))
         (content (with-temp-buffer
                    (insert-file-contents treepath)
                    (goto-char (point-min))
                    (if (not no-title) (insert "\\title{}\n"))
                    (insert (concat "\\author{" author "}\n"))
                    (buffer-string)))
         )
    (write-region content nil treepath)
    tree))

(defun forester--create-tree-link (tree-name label)
  (insert (format "[%s](%s)" label tree-name))
  )

(defun forester--create-tree-ref (tree-name)
  (insert (format "\\ref{%s}" tree-name))
  )

(defun forester-link ()
  "Link an existing tree at the current point with given label."
  (interactive)
  (let* (
         (file (read-file-name "Select tree: " (forester--root) nil t nil
                               (lambda (f) (or (file-directory-p f)
                                               (string-match-p "\\.tree\\'" (downcase f))))))
         (tree-name (file-name-base file))
         (label (read-string "Label: "))
         )
    (forester--create-tree-link tree-name label)
    )
  )

(defun forester-ref ()
  "Create a ref to an existing tree at the current point."
  (interactive)
  (let* (
         (file (read-file-name "Select tree: " (forester--root) nil t nil
                               (lambda (f) (or (file-directory-p f)
                                               (string-match-p "\\.tree\\'" (downcase f))))))
         (tree-name (file-name-base file))
         )
    (forester--create-tree-ref tree-name)
    )
  )

(defun forester--transclude-string (string)
  (interactive "sString: ")  
  (unless (bolp)  
    (newline))    
  (insert (format "\\transclude{%s}" string)) 
  )

(defun forester-transclude ()
  "Transclude a tree from the root directory"
  (interactive)
  (let* (
         (file (read-file-name "Select tree: " (forester--root) nil t nil
                               (lambda (f) (or (file-directory-p f)
                                               (string-match-p "\\.tree\\'" (downcase f))))))
         (tree-name (file-name-base file))
         )
    (forester--transclude-string tree-name)
    )
  )


(defvar forester--preview-proc nil
  "Whether './forester' is currently running.")

(defun forester--sentinel (process event)
  "Sentinel for forester-managed processes."
  (when (memq (process-status process) '(exit signal))
    (setq forester--preview-proc nil)
    (message "%s terminated: %s" process event)))

(defun forester-preview (&optional prefix)
  "Run or interact with the preview shell script.
   
If the preview process is already running, send an Enter key to trigger
recompilation.  Otherwise, start the preview process.

With a prefix argument, instead terminate the preview process.
"
  (interactive)
  (if (and forester--preview-proc (process-live-p forester--preview-proc))
      (progn
        (message "Sending Enter to the preview process...")
        (process-send-string forester--preview-proc "\n"))
    (progn
      (message "Starting the preview process...")
      (setq forester--preview-proc
            (start-process "forester-preview" "*preview*" (forester--get-binary "preview")))
      (set-process-sentinel forester--preview-proc #'forester--sentinel)
      (set-process-query-on-exit-flag forester--preview-proc nil)
      (display-buffer "*preview*"))))

(defun forester-end-preview ()
  "Kill the preview process, if currently running."
  (interactive)
  (if (and forester--preview-proc (process-live-p forester--preview-proc))
      (progn
        (message "Killing the preview process...")
        (delete-process forester--preview-proc))
    (progn
      (message "There is no preview process to terminate."))))

(defun forester--template-options ()
  (let* ((tdir (concat (forester--root) "/templates")))
    (if (file-exists-p tdir)
        (cl-map 'list #'file-name-base (directory-files tdir nil ".*\.tree"))
      '()
      )))

(defun forester--select-template ()
  (completing-read "Select template: " (forester--template-options)))

(defun forester-new (&optional template namespace dest author no-title)
  (interactive)
  (let ((template (unless template (forester--select-template))))
    (message (forester--new template namespace dest author no-title))))

(defun forester-new-and-goto (&optional template namespace dest author)
  (interactive)
  (let* ((tree (forester-new template namespace dest author)))
    (find-file (concat (forester--root) tree))
                                        ; \title{<cursor>}
    (goto-char 8)))

(defun forester-new-and-transclude (&optional template namespace dest author)
  (interactive)
  (let* ((treepath (forester-new template namespace dest author))
         (tree (file-name-base treepath)))
    (insert (concat "\\transclude{" tree "}"))))

(defun forester-new-and-transclude-and-goto (&optional template namespace dest author)
  (interactive)
  (let* ((treepath (forester-new template namespace dest author))
         (tree (file-name-base treepath)))
    (insert (concat "\\transclude{" tree "}"))
    (save-buffer)
    (find-file (concat (forester--root) treepath))
    (goto-char 8)))

(defun forester-export-to-subtree (&optional template namespace dest author)
  (interactive)
  (let* ((treepath (forester-new template namespace dest author 't))
         (tree (file-name-base treepath)))
    (kill-region (region-beginning) (region-end))
    (insert (concat "\\transclude{" tree "}"))
    (find-file (concat (forester--root) treepath))
    (goto-char (point-max))
    (yank)))

(defun forester-goto ()
  "Jump to the tree address at point"
  (interactive)
  (let* ((address (current-word))
         (file (forester--find-tree-file address)))
    (if file
        (find-file file)
      (message "Could not find tree at point")
      (project-find-file)
      )))

(defun forester--find-tree-file (tree)
  "find the (first) project file matching the name TREE"
  (interactive)
  (let* (
         (files (directory-files-recursively (forester--root)
                                             (concat "^" tree ".tree"))))
    (if files
        (car files)
      )))

(defun forester-find-parents ()
  "Use rgrep to find trees which transclude the current tree"
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if current-file
        (let
            ((search-term 
              (format "\\\\transclude{%s}" (file-name-base current-file)))
             (file-pattern "*.tree")
             )
          (rgrep search-term file-pattern (forester--root))
          )
      (message "buffer file name is 'nil'")
      )))

(defun forester-jump-in-namespace (&optional namespace)
  (interactive)
  (let* ((whoami (forester--whoami))
         (root (forester--root))
         (namespace (unless namespace (alist-get 'namespace whoami)))
         (files (directory-files-recursively root (concat namespace "-[0-9A-Z]*\.tree")))
         (files (map 'list (lambda (file) (substring file (length root))) files))
         (file (completing-read "Select tree:" files)))
    (find-file (concat root file))))

(defun forester-open ()
  (interactive)
  (let* ((fn (file-name-nondirectory (buffer-file-name)))
         (id (file-name-sans-extension fn))
         (ext (file-name-extension fn))
         (url (concat "http://localhost:8080/" id)))
    (if (string-equal ext "tree")
        (with-temp-buffer (call-process "xdg-open" nil t nil url)))))

(defun forester-today ()
  (interactive)
  (let* ((project-root (projectile-project-root))
         (date-string (format-time-string "%Y-%m-%d"))
         (file (expand-file-name (format "trees/day-notes/%s-note.tree" date-string)
                                 project-root)))
    (find-file file)))

(define-derived-mode forester-mode text-mode "Forester" "A major mode for editing forester files (trees)"
  (visual-line-mode)
  (setq-local font-lock-defaults nil)
  (setq-local comment-start "% ")
  (setq-local comment-end "")
  (unless (treesit-ready-p 'forester)
    (error "must install forester treesitter grammar"))
  (treesit-parser-create 'forester)
  (forester-ts-setup))

(add-hook 'forester-mode-hook 'rainbow-delimiters-mode)

(unless (member '("\\.tree\\'" . forester-mode) auto-mode-alist)
  (push (cons "\\.tree\\'" 'forester-mode) auto-mode-alist))

(unless (member '(forester "https://github.com/olynch/tree-sitter-forester") treesit-language-source-alist)
  (push '(forester "https://github.com/olynch/tree-sitter-forester") treesit-language-source-alist))

(provide 'forester)
;;; forester.el ends here


