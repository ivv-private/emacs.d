(server-start)

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
                     (convert-standard-filename "settings/"))
             t)

;; interface
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(ansi-color-for-comint-mode-on)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")
;;
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default dabbrev-case-fold-search t)
(put 'narrow-to-region 'disabled nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; packages
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("jabber"
                (mode . jabber-chat-mode))
               ("shell"
                (or
                 (mode . shell-mode)
                 (mode . eshell-mode)))
               ("java"
                (or
                 (mode . malabar-mode)
                 (mode . malabar-groovy-mode)))
               ("groovy"
                (mode . groovy-mode))
               ("dired"
                (mode . dired-mode))
               ("xml"
                (mode . nxml-mode))
               )))
)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

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
;; (setq org-default-notes-file (concat org-directory "/notes.org"))2
;; (setq org-clock-persist 'history)
;; (org-clock-persistence-insinuate)

(setq org-link-abbrev-alist 
      (quote 
       (("issue" . "http://support.samara.lanit.ru/issues/"))
       ))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cr" 'org-remember) 

;; gnus
;; (setq gnus-init-file
;;       (concat user-emacs-directory
;; 	      (convert-standard-filename "gnus")))

;; linphone
(require 'linphone)

;; linphone
;; (require 'linphone)
;; (require 'linphone-modeline)
;; (setq linphcsh-binary "/usr/bin/linphonecsh")

;; w3m
(require 'w3m-load)

;; switch-window
(require 'switch-window)

;; auto-clean buffers
(require 'midnight)

;; bbdb
(require 'bbdb)
;; (bbdb-initialize 'message 'gnus)

;; emms
(require 'emms-setup)
(emms-standard)
(emms-default-players)

;; jabber
(require 'jabber-autoloads)

;; rainbow
(require 'rainbow-mode)

; Setup email sending
(require 'smtpmail)

;; dict
(require 'dictem)
(dictem-initialize)
(global-set-key [f5] 'dictem-run-search)
(global-set-key "\C-cm" 'dictem-run-match)
(global-set-key "\C-cd" 'dictem-run-define)
(global-set-key "\C-c\M-r" 'dictem-run-show-server)
(global-set-key "\C-c\M-i" 'dictem-run-show-info)
(global-set-key "\C-c\M-b" 'dictem-run-show-databases)

;; lang

;; yasnipped
(require 'yasnippet-bundle)

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
   (text-scale-decrease 1)
   (toggle-read-only 1)
   (buffer-disable-undo t)))

;; CEDET
(require 'cedet)
(global-ede-mode 1)
(setq semantic-default-submodes '(global-semanticdb-minor-mode                                  
                                  global-semantic-highlight-func-mode
                                  global-semantic-decoration-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)
(require 'semantic/sb)


;; malabar
(require 'malabar-mode)
(setq malabar-groovy-lib-dir "~/.emacs.d/site-lisp/malabar-1.5-SNAPSHOT/lib")
(add-to-list 'auto-mode-alist
             '("\\.java\\'" . malabar-mode))
(add-hook 'malabar-mode-hook
          (lambda () 
            (add-hook 'after-save-hook 'malabar-compile-file-silently
                      nil t)))


;; sql
(setq auto-mode-alist
      (append '(("\\.pls\\'" . sql-mode) ("\\.pkg\\'" . sql-mode)
                ("\\.pks\\'" . sql-mode) ("\\.pkb\\'" . sql-mode)
                ("\\.sql\\'" . sql-mode) ("\\.PLS\\'" . sql-mode)
                ("\\.PKG\\'" . sql-mode) ("\\.PKS\\'" . sql-mode)
                ("\\.PKB\\'" . sql-mode) ("\\.SQL\\'" . sql-mode)
                ("\\.prc\\'" . sql-mode) ("\\.fnc\\'" . sql-mode)
                ("\\.trg\\'" . sql-mode) ("\\.vw\\'" . sql-mode)
                ("\\.PRC\\'" . sql-mode) ("\\.FNC\\'" . sql-mode)
                ("\\.TRG\\'" . sql-mode) ("\\.VW\\'" . sql-mode))
              auto-mode-alist ))

;; nxml
(add-to-list 'auto-mode-alist '("\\.xhtml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.jspx$" . nxml-mode))

;; javaScript
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

;; groovy
;; turn on syntax highlighting
;;(global-font-lock-mode 1)

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))

(add-hook 'groovy-mode-hook
          (lambda () 
            (add-hook 'after-save-hook 'malabar-compile-file-silently
                      nil t)))

;; scala
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))
;; (setq yas/my-directory "/path/to/some/directory/scala-mode/contrib/yasnippet/snippets")
;; (yas/load-directory yas/my-directory)

;; jdb
(add-hook 'jdb-mode-hook
          (lambda ()
            (load-file "~/.emacs.d/site-lisp/jdb-directories.el")))

(put 'scroll-left 'disabled nil)

;; secrets
(load "secrets.el")
(require 'secrets)

;; custom settings
(setq custom-file
      (concat user-emacs-directory
              (convert-standard-filename "settings/")
              (convert-standard-filename "emacs-custom")))

(load custom-file)

