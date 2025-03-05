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

(defface forester-inline-math
  '((t :inherit font-lock-string-face))
  "Forester inline math"
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

(defvar forester-ts-font-lock-rules
  '(
    :language forester
    :feature title
    ;; :override t
    ;; There's an outer wrapper here which is semantically meaningless,
    ;; it's just a quotation operator.
    (((title (_)@forester-title)))

    :language forester
    :feature inline-math
    ;; :override t
    ((inline_math "#" "{" (_)@forester-inline-math "}"))

    :language forester
    :feature rich-text
    ((em "\\" "em" "{" (_)@forester-em "}")
     (strong "\\" "strong" "{" (_)@forester-strong "}")
     )
    )
  )

(defvar forester-ts-indent-rules
  '((forester
     ((parent-is "source_file") parent 0)
     ((node-is "command") grand-parent 2)
     ((node-is "text") (nth-sibling 1) 1)
     ((node-is "inline_math") (nth-sibling 1) 1)
     ((node-is "}") (nth-sibling 0) -1)
     (no-node parent 1)
     (catch-all parent 0)
     )))

(defun forester-ts-setup ()
  "Setup treesit for forester-ts-mode."
  ;; Our tree-sitter setup goes here.

  ;; This handles font locking
  (setq-local treesit-font-lock-settings
               (apply #'treesit-font-lock-rules
                      forester-ts-font-lock-rules))

  (setq-local treesit-font-lock-feature-list
	      '((inline-math title keyword builtin rich-text) () ()))

  ;; This handles indentation
  (setq-local treesit-simple-indent-rules forester-ts-indent-rules)

   ;; ... everything else we talk about go here also ...

  ;; End with this
  (treesit-major-mode-setup))

(defun forester--root ()
  (project-root (project-current)))

(defun forester--get-binary ()
  (let* ((local-forester (concat (forester--root) "forester")))
    (if (file-exists-p local-forester)
        local-forester
      (executable-find "forester"))))

(defun forester--whoami ()
  (let ((whoami (concat (forester--root) ".whoami")))
    (if (file-exists-p whoami)
        (json-read-file whoami))))

(defun forester--start (&rest args)
  (let ((path (forester--get-binary)))
    (apply #'start-process `("forester" "*forester-output*" ,@args))))

(defun forester--call (&rest args)
  (let* ((path (forester--get-binary))
         (default-directory (forester--root)))
    (with-temp-buffer
      (apply #'call-process `(,path nil t nil ,@args))
      (buffer-string))))

(defun forester--new (&optional template namespace dest author)
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
         (tree (replace-regexp-in-string "\n" "" output)))
    (write-region (concat "\\author{" author "}\n") nil (concat (forester--root) tree) t)
    tree))

(defun forester-new (&optional template namespace dest author)
  (interactive)
  (message (forester--new template namespace dest author)))

(defun forester-new-and-goto (&optional template namespace dest author)
  (interactive)
  (let* ((tree (forester--new template namespace dest author)))
    (find-file (concat (forester--root) tree))))

(defun forester-new-and-transclude (&optional template namespace dest author)
  (interactive)
  (let* ((treepath (forester--new template namespace dest author))
         (tree (file-name-base treepath)))
    (insert (concat "\\transclude{" tree "}"))))

(defun forester-goto ()
  "Jump to the tree address at point"
  (interactive)
  (let* ((address (current-word))
         (files (directory-files-recursively (forester--root)
                                             (concat "^" address ".tree"))))
    (if files
        (find-file (car files))
      (error "Could not find tree"))))

(define-derived-mode forester-mode text-mode "Forester" "A major mode for editing forester files (trees)"
  (visual-line-mode)
  (setq-local font-lock-defaults nil)
  (setq-local comment-start "% ")
  (setq-local comment-end "")
  (unless (treesit-ready-p 'forester)
    (error "must install forester treesitter grammar"))
  (treesit-parser-create 'forester)
  (forester-ts-setup))

(add-hook 'forester-mode-hook #'rainbow-delimiters-mode)

(unless (member '("\\.tree\\'" . forester-mode) auto-mode-alist)
  (push (cons "\\.tree\\'" 'forester-mode) auto-mode-alist))

(unless (member '(forester "https://github.com/olynch/tree-sitter-forester") treesit-language-source-alist)
  (push '(forester "https://github.com/olynch/tree-sitter-forester") treesit-language-source-alist))

(provide 'forester)
;;; forester.el ends here


