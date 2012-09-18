(server-start)

;; CEDET
(load-file (concat user-emacs-directory "/site-lisp/cedet-1.1/common/cedet.elc"))
(require 'cedet)
(require 'semantic)
(require 'semanticdb)
(setq semantic-default-submodes 
      '(global-semanticdb-minor-mode
        global-semantic-decoration-mode
        global-semantic-mru-bookmark-mode
        global-srecode-minor-mode))
(semanticdb-enable-gnu-global-databases 'java-mode)

;; load-path
(let ((default-directory
        (concat user-emacs-directory
                (convert-standard-filename "site-lisp/"))
        ))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; settings
(add-to-list 'load-path 
             (concat user-emacs-directory
                     (convert-standard-filename "etc/")) t)

;; interface
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) (if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) (if (fboundp 'tooltip-mode) (tooltip-mode -1)) (global-set-key "\C-w" 'backward-kill-word) (global-set-key "\C-x\C-k" 'kill-region) (global-set-key "\M-`" 'other-window)
(ansi-color-for-comint-mode-on)
(setq redisplay-dont-pause t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default dabbrev-case-fold-search t) (put 'narrow-to-region 'disabled nil) (global-set-key (kbd "C-x C-b") 'ibuffer)

;; backup
(setq
 backup-by-copying t ; don't clobber symlinks  backup-directory-alist (list (cons "." (concat user-emacs-directory "var/backup")))  delete-old-versions t  kept-new-versions 6  kept-old-versions 2  version-control t  auto-save-file-name-transforms (list (list ".*" (concat user-emacs-directory "var/autosaves/\\1") t)))

;; bookmark
(setq
 bookmark-save-flag 1
 bookmark-default-file (concat user-emacs-directory (convert-standard-filename "var/bookmarks.bmk"))  bookmark-file bookmark-default-file)

;; dired
(require 'dired-x)
(setq dired-omit-files "^\\...+$")

;; uniquify buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; ;; abbrevs
;; (setq abbrev-file-name             
;;       (concat user-emacs-directory "var/abbrev_defs"))
;; (setq save-abbrevs t)
;; (if (file-exists-p abbrev-file-name)
;;     (quietly-read-abbrev-file))
;; ;; (setq default-abbrev-mode t)

;; I hate tabs!
(setq-default
 indent-tabs-mode nil
 indent-tabs-mode nil
 c-basic-indent 4
 c-basic-offset 4
 tab-width 4)
;; (highlight-tabs)
;; (highlight-trailing-whitespace)

;;
(show-paren-mode t)

;;
(ido-mode t)
(require 'uniquify)

;;
(ffap-bindings)

;;
(global-set-key [f9] 'toggle-truncate-lines)

;; eshell
(require 'eshell)
(require 'em-smart)
(require 'em-term)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(add-to-list 'eshell-visual-commands "ssh")

(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("Default"
               ("dired" (mode . dired-mode))
               ("java-tests" (filename . ".+Test.java"))
               ("java" (or (mode . jde-mode) (mode . java-mode))) 
               ("lisp" (or (mode . lisp-mode) (mode . emacs-lisp-mode)))
               ("groovy" (mode . groovy-mode))
               ("scripts" (or (mode . shell-script-mode) (name . ".*sh$")))
               ("db" (or (mode . sql-mode) (mode . sqlplus-mode)))
               ("logs" (or (name . ".*log") (name . "catalina.out")))
               ("xml" (mode . nxml-mode))
               ("org" (mode . org-mode))
               ("utils" (name . "*.+"))
               ("sys" (or (name . "\*Malabar.*") (mode . grep-mode)))
               ("shell" (or (mode . shell-mode) (mode . eshell-mode)))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "Default")
            (ibuffer-do-sort-by-filename/process)))


;; org-mode
(setq org-agenda-files (list "~/org"))
(setq org-log-done t
      org-use-fast-todo-selection t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "WAITING(w@/!)" "|" "CANCELLED(c!/!)")
        (sequence "SOMEDAY(s!/!)" "|")
        (sequence "ONGOING(o)" "|")))

(setq org-todo-keyword-faces
      '(("TODO"  . (:foreground "red" :weight bold))
        ("NEXT"  . (:foreground "red" :weight bold))
        ("DONE"  . (:foreground "forest green" :weight bold))
        ("WAITING"  . (:foreground "orange" :weight bold))
        ("CANCELLED"  . (:foreground "forest green" :weight bold))
        ("SOMEDAY"  . (:foreground "orange" :weight bold))
        ("ISSUE"  . (:foreground "forest green" :weight bold))
        ("ONGOING"  . (:foreground "orange" :weight bold))))

;;(org-remember-insinuate)                                                                            
;;(setq org-directory "~/org/")
;; (setq org-default-notes-file (concat org-directory "/notes.org"))2 ;; (setq org-clock-persist 'history) ;; (org-clock-persistence-insinuate)

(setq org-link-abbrev-alist 
      (quote 
       (("issue" . "http://support.samara.lanit.ru/issues/"))
       ))

(global-set-key "\C-cl" 'org-store-link) (global-set-key "\C-ca" 'org-agenda) (global-set-key "\C-cb" 'org-iswitchb) (global-set-key "\C-cr" 'org-remember) 

;; gnus
;; (setq gnus-init-file
;;       (concat user-emacs-directory
;;               (convert-standard-filename "gnus")))

;; ;; linphone
;; (require 'linphone)

;; ;; w3m
;; (require 'w3m-load)

;; w3
(require 'w3-auto)

;; auto-clean buffers
(require 'midnight)
(add-to-list 'clean-buffer-list-kill-regexps
             "\*GTAGS SELECT.*")

;; ;; bbdb
;; (require 'bbdb)
;; ;; (bbdb-initialize 'message 'gnus)

;; ;; emms
;; (require 'emms-setup)
;; (emms-standard)
;; (emms-default-players)

;; ;; jabber
;; (require 'jabber-autoloads)

;; ;; rainbow
;; (require 'rainbow-mode)

;; ; Setup email sending
;; (require 'smtpmail)

;; ;; dict
;; (require 'dictem)
;; (dictem-initialize)
;; (global-set-key [f5] 'dictem-run-search) ;; (global-set-key "\C-cm" 'dictem-run-match) ;; (global-set-key "\C-cd" 'dictem-run-define) ;; (global-set-key "\C-c\M-r" 'dictem-run-show-server) ;; (global-set-key "\C-c\M-i" 'dictem-run-show-info) ;; (global-set-key "\C-c\M-b" 'dictem-run-show-databases)

;; lang

(global-unset-key [f8] )
(global-set-key (kbd "C-z") 'compile)

;; log4j
(require 'log4j-mode)
(setq auto-mode-alist
      (append '(("server.log" . log4j-mode) 
                ("catalina.out" . log4j-mode) 
                ("tomcat.log" . log4j-mode))
              auto-mode-alist))

(add-hook
 'log4j-mode-hook
 (lambda ()
   (setq truncate-lines t)
   (text-scale-set -1)
   (toggle-read-only t)
   (buffer-disable-undo)
   (end-of-buffer)))


;; gtags
(require 'gtags)
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-suggested-key-mapping t)
(global-set-key (kbd "C-c t r") 'gtags-find-rtag)
(global-set-key (kbd "C-c t f") 'gtags-find-file)
(global-set-key (kbd "C-c t t") 'gtags-find-tag)
(global-set-key (kbd "M-.") 'gtags-find-tag)
(add-hook 'gtags-select-mode-hook
  '(lambda ()
     (setq hl-line-face 'underline)
     (hl-line-mode 1)
))


;; java 

;; compiller
(require 'compile)
(setq compilation-error-regexp-alist
      (list
       ;; works for maven 3.x
       '("^\\(\\[ERROR\\] \\)?\\(/[^:]+\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 2 3 4)
       ;; works for maven jde javac server
       '("^\\(/[^:]+\\):\\([0-9]+\\):" 1 2)
       ;; surefire 
       '("^\\sw+(\\(\\sw+\\.\\)+\\(\\sw+\\)).+<<< \\(FAILURE\\|ERROR\\)!$"2)
))

;; append compilation snippets
(let ((snippets-buf (find-file-noselect (concat user-emacs-directory "etc/java-compile.org"))))
  (setq compile-history nil)
  (with-current-buffer 
      snippets-buf
    (org-map-region
     '(lambda nil
        (add-to-list 
         'compile-history 
         (format 
          "#%s\\\n%s"
          (nth 4 (org-heading-components))
          (mapconcat 'string (org-get-entry) ""))))
     1 (buffer-end 1))
    (kill-buffer snippets-buf)))

;; yasnippet
;; (require 'yasnippet-bundle)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/ac-dict")
(add-to-list 'ac-modes 'jde-mode)
(add-to-list 'ac-modes 'sqlplus-mode)
(setq ac-ignore-case 'smart)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(require 'gud)
(defun java-my-minor () 
 (progn
  (gtags-mode t)
  (glasses-mode t)
  (subword-mode t)
  ;; (jde-abbrev-mode t)
  (add-to-list 'ac-sources ac-source-gtags)
  (local-set-key [f8] 'gud-next)
  (local-set-key [f9] 'gud-cont)
  (local-set-key (kbd "M-/") 'dabbrev-expand)
  (local-set-key (kbd "C-c C-v .") 'jde-complete-minibuf)
  (local-set-key (kbd "C-c j r") 'jde-junit-run)
  (add-hook 
   'before-save-hook
   (lambda ()
     (jde-import-kill-extra-imports)
     (jde-import-all)
     (jde-import-organize))
   nil t)
  (add-hook 'after-save-hook 'jde-compile nil t)))

(add-hook 'jde-mode-hook 'java-my-minor)

;; jdb
(gud-def 
 gud-redefine 
 (gud-call 
  (format 
   "redefine %%c %s/%s.class"
   (file-truename jde-compile-option-directory)
   (replace-regexp-in-string "\\." "/" (gud-format-command "%c" arg))))
 "\C-r" "Redefine class")

(require 'jdb-sourcepath)
(add-hook 
 'jdb-mode-hook
 (lambda ()
   (set 'gud-jdb-sourcepath (jdb-sourcepath-from-rc))))

;; (run-at-time "1:00am" (* 60 60 24) 'jdb-setup)


;; ; make completion buffers disappear after 3 seconds.
;; (add-hook 'completion-setup-hook
;;   (lambda () (run-at-time 3 nil
;;     (lambda () (delete-windows-on "*Completions*")))))

;; sql
(setenv "SQLPATH" (expand-file-name "etc/sql" user-emacs-directory))

;;(add-hook
;; 'after-init-hook
;; (lambda nil 
;;   (progn
;;     (require 'sqlplus)
;;     (require 'plsql))))
(add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))
(add-to-list 'auto-mode-alist '("\\.sql\\'" . sqlplus-mode))

(setq auto-mode-alist
      (append '(("\\.pls\\'" . plsql-mode) ("\\.pkg\\'" . plsql-mode)
                ("\\.pks\\'" . plsql-mode) ("\\.pkb\\'" . plsql-mode)
                ("\\.PLS\\'" . plsql-mode) ("\\.PKS\\'" . plsql-mode)
                ("\\.PKB\\'" . plsql-mode) ("\\.PKG\\'" . plsql-mode)
                ("\\.prc\\'" . plsql-mode) ("\\.fnc\\'" . plsql-mode)
                ("\\.trg\\'" . plsql-mode) ("\\.vw\\'" . plsql-mode)
                ("\\.PRC\\'" . plsql-mode) ("\\.FNC\\'" . plsql-mode)
                ("\\.TRG\\'" . plsql-mode) ("\\.VW\\'" . plsql-mode))
              auto-mode-alist ))

(defun sqlplus-x-describe ()
  (interactive)
  (sqlplus-check-connection)
  (sqlplus-send-user-string (concat "desc " (read-from-minibuffer "Describe object: " (word-at-point)))))

(defun sqlplus-x-select-all ()
  (interactive)
  (sqlplus-check-connection)
  (sqlplus-send-user-string (concat "select * from " (read-from-minibuffer "Select * from: " (word-at-point)) ";\n")))

(require 'org-table)
(defvar sqlplus-x-columns '(sqlplus-x-service sqlplus-x-user sqlplus-x-pwd))
(defun sqlplus-x-connect ()
  "Build a connection string and make a connection. The point must be in an org-mode table.
Columns of the table must correspond to the `sqlplus-x-columns' variable."
  (interactive)
  (org-table-force-dataline)
  (let*
      ((cur-row (nth (org-table-current-dline) (org-table-to-lisp)))
       (connetion (mapcar (lambda (column) (cons column (nth (position column sqlplus-x-columns) cur-row))) sqlplus-x-columns))
       (user (if (= (org-table-current-column) (+ 1 (position 'sqlplus-x-user sqlplus-x-columns)))
                 (thing-at-point 'symbol)
               (cdr (assq 'sqlplus-x-user connetion)))))
    (sqlplus
     (format 
      "%s/%s@%s" user (cdr (assq 'sqlplus-x-pwd connetion)) (cdr (assq 'sqlplus-x-service connetion)))
     (format
      "%s@%s.sqp" user (cdr (assq 'sqlplus-x-service connetion))))))

(global-set-key [f4] 'sqlplus-x-connect)



;; timer-list



(add-hook 
 'sqlplus-mode-hook
 (lambda () 
   (progn
     (local-set-key [f4] 'sqlplus-x-describe)
     (local-set-key [f5] 'sqlplus-x-select-all)
     (local-set-key (kbd "C-x C-e") 'sqlplus-explain)
     (local-set-key (kbd "C-x C-r") 'sqlplus-send-region)
     (local-set-key (kbd "C-c C-c") 'sqlplus-send-current)
     (local-set-key [f2] 'sqlplus-show-buffer))))

;; nxml
(add-to-list 'auto-mode-alist '("\\.xhtml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.jspx$" . nxml-mode))

;; ;; javaScript
;; (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

;; groovy
;; turn on syntax highlighting
;;(global-font-lock-mode 1)

;; ;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
;; (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
;; (add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
;; (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; ;;; make Groovy mode electric by default.
;; (add-hook 'groovy-mode-hook
;;           '(lambda ()
;;              (require 'groovy-electric)
;;              (groovy-electric-mode)))

;; (add-hook 'ggroovy-mode-hook
;;           (lambda () 
;;             (add-hook 'after-save-hook 'malabar-compile-file-silently
;;                       nil t)))

;; ;; scala
;; (require 'scala-mode-auto)
;; (add-hook 'scala-mode-hook
;;           '(lambda ()
;;              (yas/minor-mode-on)))
;; ;; (setq yas/my-directory "/path/to/some/directory/scala-mode/contrib/yasnippet/snippets")
;; ;; (yas/load-directory yas/my-directory)

;; jdee
(load "jde-autoload")
(setq jde-check-version-flag nil)
(require 'jde-junit)
;; w3 is failed to load local file, so skip this feature
(require 'jde-help)
(defmethod jde-jdhelper-show-url ((this jde-jdhelper) url)
  (let ((doc-url (jde-url-name url)))
   (message "Displaying %s from %s"
            (oref url :class)
	     (oref (oref url :docset) :description))
   (jde-jdhelper-show-document this doc-url)))
(setq jde-compile-option-directory (concat user-emacs-directory "tmp"))
(setq jde-compile 'compile)
(add-hook 
   'jde-project-hooks
   (lambda () 
     (progn
       (setq 
        jde-global-classpath (add-to-list 'jde-global-classpath jde-compile-option-directory)
        jde-junit-working-directory (file-name-directory (car (last (jde-find-project-files (car jde-sourcepath))))))
       )))

;; make prj.el from pom.xml
(require 'find-lisp)
(require 'jdb-sourcepath)

(defun jde-x-generate-projects ()
  "Find pom files and run mvn jdee:jdee"
  (interactive)
  (let ((pom-file "pom.xml")
        (dirs-to-prune (cons "vendor" jdb-sourcepath-prune)))
    (async-shell-command
     (mapconcat 
      '(lambda (pom) 
         (format 
          "echo %s && cd %s && $MAVEN2_HOME/bin/mvn install jdee:jdee -DskipTests=true -q"
          pom
          (file-name-directory pom)))
      (find-lisp-find-files-internal 
       jdb-sourcepath-root
       (lambda (file dir) 
         (string= file pom-file))
       (lambda (file dir) 
         (not (member file dirs-to-prune))))
      "; "))))


(require 'jde-package)
(defun jde-x-jump-to-test ()
  "Open corresponded test"
  (interactive)
  (let ((package-dir (jde-package-get-package-directory))
        (test-class
          (jde-junit-get-tester-name 
           (file-name-sans-extension 
            (file-name-nondirectory buffer-file-name)))))
    (dolist (test-file
             (list 
              (car
               (delete-if-not
                'file-exists-p
                (mapcar
                 (lambda (source-dir)
                   (format "%s/%s%s.java" source-dir package-dir test-class))
                 jde-sourcepath)))))
      (find-file test-file))))


;; secrets
(load (concat user-emacs-directory (convert-standard-filename "etc/secrets.el")))
(require 'secrets)


;; custom settings
(setq custom-file
      (concat user-emacs-directory
              (convert-standard-filename "etc/")
              (convert-standard-filename "emacs-custom")))

(load custom-file)
