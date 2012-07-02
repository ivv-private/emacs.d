(defvar jdb-sourcepath-root "~/projects"
  "Root directory which contains a sources")

(defvar jdb-sourcepath-prune '("target" ".svn" "arch" "examples" "tests" "test" "samples" "." "..")
  "Directories to skip while searching")

(defvar jdb-rcfile "~/.jdbrc" 
  "Jdb rc file")

(defconst jdb-sourcepath-java-file ".+\.java$" 
  "Java file regexp")

(defun jdb-setup-get-package-name (file)
   "Get package name from java file"
   (when (file-readable-p file)
     (with-temp-buffer
       (insert-file-contents-literally file)
       (goto-char (point-min))
       (let ((begin (re-search-forward "^package " nil t))
             (end (re-search-forward "[^a-zA-Z0-9._]" nil t)))
         (if (and begin end)
           (buffer-substring begin (- end 1)))))))

(defun jdb-setup-directory-predicate (file dir) 
  "Filter directories which has `jdb-sourcepath-java-file' files and prune its subdirs."
  (not
   (or
    (member file jdb-sourcepath-prune)
    (and 
     (file-directory-p (expand-file-name file dir)) 
     (directory-files (expand-file-name file dir) nil jdb-sourcepath-java-file)))))

(defun jdb-setup-file-predicate (file dir) 
  "Filter directories which has `jdb-sourcepath-java-file'"
  (let ((file-name (expand-file-name file dir)))
    (and
     (not (member file jdb-sourcepath-prune))
     (file-directory-p file-name)
     (directory-files file-name nil jdb-sourcepath-java-file))))

(defun jdb-setup ()
  "Find  java-sourcepathes within `jdb-sourcepath-root' and write to `jdb-rcfile'"
  (interactive)
  (let ((sources 
         (delete-dups
          (mapcar
           (lambda (source-dir)
             (let ((package 
                    (jdb-setup-get-package-name
                     (car (directory-files source-dir t jdb-sourcepath-java-file)))))
               (if package
                   (replace-regexp-in-string
                    (concat "." package) "" source-dir) ; cut package postfix with path delimiter
                 (message (format "error: %s" source-dir)))))
           (find-lisp-find-files-internal 
            jdb-sourcepath-root
            'jdb-setup-file-predicate 
            'jdb-setup-directory-predicate)))))
    (save-excursion
      (with-current-buffer 
          (find-file-noselect jdb-rcfile)
        (goto-char (point-min))
        (if (re-search-forward "^use " nil t)
            (delete-region (point) (line-end-position))
          (insert-string "use "))
        (insert (mapconcat 'identity sources ":"))
        (save-buffer)
        (kill-buffer)))))

(defun jdb-sourcepath-from-rc ()
  "Get list of sourcepathes from `jdb-rcfile' file"
  (with-temp-buffer
    (insert-file-contents-literally jdb-rcfile)
    (goto-char (point-min))
    (if (re-search-forward "^use " nil t)
        (split-string (buffer-substring (point) (line-end-position)) ":")
      ())))

(provide 'jdb-sourcepath)