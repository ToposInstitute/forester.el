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
  (if-let ((project (project-current)))
      (project-root project)
    (error "Not in a project")))

(defun forester--get-binary (binary)
  (let* ((local-forester (concat (forester--root) binary)))
    (if (file-exists-p local-forester)
        local-forester
      (executable-find binary))))

(defun forester--whoami ()
  (let ((whoami (concat (forester--root) ".whoami")))
    (if (file-exists-p whoami)
        (json-read-file whoami))))

(defgroup forester nil
  "Forester customisation"
  :group 'applications
  )

(defcustom forester-bib-subdir "trees/bib/"
  "The subdirectory of the root in which bib refs are kept."
  :type 'string
  :group 'forester
  )


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

(defun forester-ref (&optional prefix)
  "Create a ref to an existing tree at the current point.
  
  With a single prefix argument, search in 'whoami.dest'.
  With a double prefix argument, search in 'forester-bib-subdir'."
  (interactive "p")
  (let* (
         (root (forester--root))
         (whoami (forester--whoami))
         (search-dir (pcase prefix 
                       (1 root)
                       (4 (if whoami (concat root (alist-get 'dest whoami)) root))
                       (16 (concat root forester-bib-subdir))
                       (_ root)
                       )
                     )
         (file (read-file-name "Select tree: " search-dir nil t nil
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
  (interactive "P")
  (if prefix
      (progn
        (forester-end-preview))
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
  )

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
  (if-let* ((address (current-word))
            (file (forester--find-tree-file address)))
      (find-file file)
    (message "Could not find tree at point")
    (let ((completion-extra-properties '(:category tree))) (project-find-file))
    ))

(defun forester--find-tree-file (tree)
  "find the (first) project file matching the name TREE"
  (interactive)
  (let* (
         (files (directory-files-recursively (forester--root)
                                             (concat "^" tree ".tree"))))
    (if files
        (car files)
      )))

(defun forester--tree-files ()
  "Return absolute paths of .tree files in the current project."
  (let ((root (forester--root)))
    (mapcar
     (lambda (file)
       (if (file-name-absolute-p file)
           file
         (expand-file-name file root)))
     (seq-filter
      (lambda (file) (string-match-p "\\.tree\\'" file))
      (project-files (project-current))))))

(defun forester--grep-trees-elisp (string)
  "Return a list of .tree files in the project that contain STRING."
  (let ((needle (format "%s" string))
        matches)
    (dolist (file (forester--tree-files) (nreverse matches))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (search-forward needle nil t)
          (push file matches))))))

(defun forester--grep-trees-rg (string)
  "Return a list of .tree files in the project that transclude STRING, using ripgrep."
  (when (executable-find "rg")
    (let ((default-directory (forester--root)))
      (condition-case nil
          (mapcar #'expand-file-name
                  (delete-dups
                   (process-lines
                    "rg" "-l" "-F" "-g" "*.tree"
                    (format "%s" string)
                    ".")))
        (error '())))))

(defun forester--grep-trees (string)
  "Return a list of .tree files matching for regexp STRING."
  (or (forester--grep-trees-rg string)
      (forester--grep-trees-elisp string)))

(defun forester--read-match-tree (matches)
  "Prompt for one file from MATCHES and return it."
  (find-file
   (let ((completion-extra-properties '(:category tree)))
     (completing-read "Select tree: " matches nil t))))

(defun forester--show-match-trees-grep (string)
  "Show parent trees for STRING in an `rgrep' buffer."
  (rgrep (format "%s" string)
         "*.tree"
         (forester--root)))

(defun forester--handle-matches (matches on-many &optional thing)
  "Handle MATCHES as 0, 1, or many results.

ON-MANY determines how to handle multiple matches:
- `visit' means prompt with completion and visit the selection
- `grep' means show an `rgrep' buffer

THING is used in user messages."
  (pcase matches
    (`() (message "No matches found%s"
                  (if thing (format " for %s" thing) "")))
    (`(,file) (find-file file))
    (_
     (pcase on-many
       ('visit (forester--read-match-tree matches))
       ('grep
        (if thing
            (forester--show-match-trees-grep thing)
          (user-error "Need THING to show grep results")))
       (_ (user-error "Unknown multiple-match handler: %S" on-many))))))

(defun forester-find-parents (&optional use-grep)
  "Find trees which transclude the current tree.

If there is exactly one parent, visit it.
If there are multiple parents, prompt to choose one.

With prefix argument USE-GREP, show an `rgrep' buffer instead when there
are multiple matches."
  (interactive "P")
  (if-let* ((current-file (buffer-file-name))
            (tree-name (file-name-base current-file))
            (parents (forester--grep-trees (format "\\transclude{%s}" tree-name))))
      (forester--handle-matches
       parents
       (if use-grep 'grep 'visit)
       tree-name)
    (message "Current file has no parents")))

(defun forester-find-title (&optional use-grep)
  "Find trees with titles containing TITLE-STRING.

If there is exactly one match, visit it.
If there are multiple matches, prompt to choose one.

With prefix argument USE-GREP, show an `rgrep' buffer instead when there
are multiple matches."
  (interactive "P")
  (if-let* ((title-string (read-string "title string (can be a prefix): "))
            (matches (forester--grep-trees (format "\\title{%s" title-string))))
      (forester--handle-matches
       matches
       (if use-grep 'grep 'visit)
       title-string)
    (message "No matches")))

(defun forester--append-transcluded (upstream str)
  "Append a list of all trees downstream of the current"
  (let (
        (local-transcludes '())
        (downstreams '())
        (pos 0))
    (while (string-match "\\\\transclude{\\(.*?\\)}" str pos)
      (let ((tree (substring-no-properties (match-string 1 str))))
        (if (member tree upstream)
            (error "circular reference in transclusions at %s\nPath: %S)" tree upstream)
          (push tree local-transcludes)
          (setq pos (match-end 0)))
        ))
    (dolist (tree local-transcludes)
      (if-let* ((tree-file (forester--find-tree-file tree))
                (str (with-temp-buffer 
                       (insert-file-contents tree-file)
                       (buffer-string)
                       ))
                )
          (push (forester--append-transcluded (cons tree upstream) str) downstreams)
        (error "Could not find the tree %s" tree)
        )
      )
    (apply 'append local-transcludes downstreams)
    )
  )

(defun forester--get-transcluded ()
  "Return a list of all trees downstream of the current"
  (forester--append-transcluded '() (buffer-string))
  )

(defun forester-dired-display-downstream ()
  "Create a dired buffer with files downstream of this tree.

These files include the current one (an error is raised if the current
buffer is not visiting a file).
"
  (interactive)
  (if-let (
         (this-file (buffer-file-name))
         (downstream-files (mapcar 'forester--find-tree-file (forester--get-transcluded)))
         )
      (dired (cons (forester--root) (cons this-file downstream-files)))
    (error "The current buffer is not visiting a file")
    )
  )

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


