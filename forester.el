;;; forester.el --- a major mode for forester files

(defun forester--root ()
  (project-root (project-current)))

(defun forester--get-binary ()
  (let* (
         (local-forester (concat (forester--root) "forester"))
         )
    (if (file-exists-p local-forester)
        local-forester
      (executable-find "forester")
      ))
  )

(defun forester--whoami ()
  (let ((whoami (concat (forester--root ".whoami"))))
    (if (file-exists-p whoami)
        (json-read-file whoami)
      ))
  )

(defun forester-new (&optional template namespace author)
    (interactive)
    )

(defun forester-go-to ()
  "Returns the file name of the transcluded tree at point"
  (interactive)
  (let* (
         (address (current-word))
         (files (directory-files-recursively (forester--root)
                                             (concat "^" address ".tree")))
         )
    (if files
        (find-file (car files))
      (error "Could not find tree"))
    )
  )

(define-derived-mode forester-mode text-mode "Forester" "A major mode for editing forester files (trees)"
  (visual-line-mode)
  )

(unless (member '("\\.tree\\'" . forester-mode) auto-mode-alist)
  (push (cons "\\.tree\\'" 'forester-mode) auto-mode-alist))





(provide 'forester)
;;; forester.el ends here


