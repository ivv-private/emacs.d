;;
;;

(make-directory (locate-user-emacs-file "local/lisp") :no-error)
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; Setup package manager
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package use-package
  :config
  (setq use-package-always-ensure t))

;; Customization
(setf user-full-name "Slava Ignatyev"
      user-mail-address "ivv.private@gmail.com"
      backup-inhibited t
      auto-save-default nil
      auto-save-list-file-prefix (locate-user-emacs-file "local/saves")
      inhibit-startup-message t
      initial-scratch-message nil
      echo-keystrokes 0.1
      disabled-command-function nil
      custom-file (make-temp-file "emacs-custom")
      large-file-warning-threshold 536870911
      gc-cons-threshold (* 1024 1024 32)
      ring-bell-function (lambda ())
      redisplay-dont-pause t
      column-number-mode t
      comint-completion-autolist t
      comint-input-ignoredups t
      comint-use-prompt-regexp t
      uniquify-buffer-name-style 'post-forward-angle-brackets
      fill-column 120

      standard-indent 1
      indent-tabs-mode t
      tab-width 4)

(show-paren-mode)
(prefer-coding-system 'utf-8-unix)
(ansi-color-for-comint-mode-on)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'dired-mode-hook (lambda () (rename-buffer (format "*Dired: %s*" (buffer-name)))))


(transient-mark-mode 1)

(when (fboundp 'set-horizontal-scroll-bar-mode)
  (set-horizontal-scroll-bar-mode nil))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keybindings
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

(global-set-key (kbd "M-s M-s") 'rgrep)
(global-set-key (kbd "M-r") 'replace-regexp)
(global-set-key (kbd "C-x C-g") (lambda () (interactive) (revert-buffer t t t)))
(global-set-key [f8] 'toggle-truncate-lines)


;; Packages
(use-package vc
  :config(setf vc-handled-backends nil
	       vc-follow-symlinks t))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package whitespace
  :config (setf whitespace-line-column nil
		whitespace-style '(face trailing space-before-tab newline
					indentation empty space-after-tab)))

(use-package bookmark
  :config (setf bookmark-default-file (locate-user-emacs-file "local/bookmarks")))

(use-package dabbrev
  :defer t
  :init (setf abbrev-file-name (locate-user-emacs-file "local/abbrev_defs"))
  :config (setf dabbrev-case-fold-search nil))


(use-package glasses
  :config (setq glasses-original-separator "-"
		glasses-separator "-"
		glasses-uncapitalize-p t))

(use-package edit-server
  :init  (setq server-socket-dir (locate-user-emacs-file "local/srv"))
  (add-hook 'after-init-hook 'server-start t))

(use-package compile
  :bind (("C-z" . compile))
  :config (setq compilation-always-kill t
		compilation-ask-about-save nil
		compilation-auto-jump-to-first-error t
		compilation-scroll-output nil
		compilation-window-height 20))

(use-package ivy
  :config (setq ivy-ignore-buffers (quote ("\\` " "^\\*magit-\\(process\\|diff\\)"))))

(use-package counsel
  :config (setq counsel-find-file-at-point t))

(use-package counsel-gtags
  :bind (("M-." . counsel-gtags-find-definition)
	 ("C-c t f" . counsel-gtags-find-file)
	 ("C-c t r" . counsel-gtags-find-reference)
	 ("C-c t s" . counsel-gtags-find-symbol)))

(use-package semantic
  :config (setq semanticdb-default-save-directory (locate-user-emacs-file "local/semanticdb"))
  (make-directory (locate-user-emacs-file "local/semanticdb") :no-error))

(use-package jdee
  :bind (("<f6>" . jdee-x-run-test)
	 :map jdee-mode-map
	 ("M-RET" . jdee-complete-minibuf)
	 ("C-c C-v C-u" . jdee-package-update)
	 ("<f5>" . jdee-x-single-test-set)
	 ("<f10>" . jdee-open-project-file))

  :config (setq jdee-compile-option-hide-classpath t
		jdee-flycheck-enable-p nil
		jdee-import-group-function 'jdee-import-group-first-level)

  (defvar jdee-x-buffer-test nil
    "Test buffer that is binded to current java source")

  (defvar jdee-x-single-test ""
    "Selected test for single executionn")
  (add-hook 'jdee-mode-hook 'java-my-minor))
  
  (defun java-my-minor ()
    (progn
      (glasses-mode t)
      (whitespace-mode t)
      (add-hook 'before-save-hook
		(lambda ()
		  (delete-trailing-whitespace)
		  (jdee-import-kill-extra-imports)
		  (jdee-import-organize))
		nil t)
      (add-hook 'after-save-hook  'jdee-compile nil t))


  (defun jdee-import-group-first-level (import-tag)
    "My order for importing"
    (let* ((import-name (semantic-tag-name import-tag))
	   (prefix (substring import-name 0 (string-match "\\." import-name))))
      (cond ((string-match "java" prefix) "1-java")
	    ((string-match "javax" prefix) "2-javax")
	    ((string-match "org" prefix) "3-org")
	    (t prefix))))

  (defun jdee-x-jump-to-test ()
    "Open corresponded test"
    (interactive)
    (let* ((sources (delete-if-not
		     'file-exists-p
		     (mapcar (apply-partially
			      'expand-file-name
			      (jdee-package-get-package-directory)) jdee-sourcepath)))
	   (test-file-name (concat (jdee-junit-get-tester-name
				    (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
				   ".java"))
	   (fn (find-if 'file-exists-p  (mapcar (apply-partially 'expand-file-name test-file-name) sources))))
      (if fn
	  (find-file fn)
	(progn
	  (setq default-directory (car (last sources)))
	  (call-interactively 'find-file)))))

  (defun jdee-x-run-test ()
    "Run junit test"
    (interactive)

    (unless jdee-x-buffer-test
      (error "There is no selected unit test buffer. Jump to the junit buffer and press f6"))

    (with-current-buffer (get-buffer jdee-x-buffer-test)
      (jdee-load-project-file)
      (add-to-list 'jdee-global-classpath (expand-file-name "~/usr/lib/single-test-runner-1.0.jar"))
      (if jdee-x-single-test
	  (setq jdee-junit-testrunner-type "x.junit.SingleTestRunner")
	(setq jdee-junit-testrunner-type "org.junit.runner.JUnitCore"))
      (if current-prefix-arg
	  (add-to-list 'jdee-run-option-vm-args "-agentlib:jdwp=transport=dt_socket,address=localhost:9009,server=y,suspend=y"))
      (let ((saved-dir default-directory))
	(jdee-junit-run)
	(setq default-directory saved-dir))
      (if (remove-if 'null (mapcar
			    (lambda (arg) (string-match "-agentlib:jdwp.*suspend=y" arg))
			    jdee-run-option-vm-args))
	  (jdb "jdb -connect com.sun.jdi.SocketAttach:port=9009,hostname=localhost")))

    (jdee-load-project-file))

  (defun jdee-x-single-test-set ()
    "Toggle Single/All test running"
    (interactive)
    (setq jdee-x-buffer-test (current-buffer))

    (if current-prefix-arg
	(setq jdee-x-single-test (read-string "Single test to run: "))
      (let ((method (jdee-parse-get-method-at-point)))
	(setq jdee-x-single-test
	      (if method
		  (let ((test-name (cdr (car method))))
		    (message (format "Single running test: %s" test-name))
		    test-name)
		(progn
		  (message "Toggle all running tests")
		  ()))))))

  (defmethod jdee-run-vm-args ((this jdee-run-vm))
    "Get command line args."
    (if jdee-x-single-test
	(append jdee-run-option-vm-args
		(list (format "-Dtest.single=%s" jdee-x-single-test)))
      jdee-run-option-vm-args))

      )


(use-package gud
  :bind (("<f2>" . gud-step)
	 ("<f3>" . gud-next)
	 ("<f7>" . gud-cont))
  :config
  (setq gud-jdb-use-classpath t)
  (gud-def
   gud-redefine
   (gud-call
    (format
     "redefine %%c %s/%s.class"
     (file-truename jdee-compile-option-directory)
     (gud-format-command "%c" arg)))
   ;; (replace-regexp-in-string "\\." "/" (gud-format-command "%c" arg)))))
   "\C-r" "Redefine class")

  ;; TODO Make jdb-sourcepath as package
  ;; autoload
  (use-package jdb-sourcepath
    :config
    (add-hook
     'jdb-mode-hook
     (lambda ()
       (setq gud-jdb-classpath (jdb-sourcepath-from-rc)
	     comint-prompt-regexp "^.*\] ")))))

(use-package groovy-mode)

(use-package yasnippet
  :config (yas-global-mode t))

(use-package ffap
  :config (ffap-bindings))

(use-package ido
  :config (ido-mode t))

(use-package ivy
  :bind (("C-c SPC" . ivy-resume)
	 ("M-o" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x r b" . counsel-bookmark)
   ("C-x l" . counsel-locate)
   ("C-x g" . counsel-git)))

(use-package swiper
  :bind
  (("C-s" . swiper))

  ;; (require 'thingatpt)
  ;; ;; ivy counsel swiper
  ;;   (require 'thingatpt)


  ;;   (global-set-key "\C-s" (lambda ()
  ;; 			   (interactive)
  ;; 			   (swiper (let ((thing (thing-at-point 'symbol)))
  ;; 				     (if thing
  ;; 					 (format "\\<%s\\>" thing)
  ;; 				       ())))))
  )

(use-package avy
  :bind (("M-," . avy-goto-word-1)))

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)))

(use-package magit
  :bind (("C-c g s" . magit-status)))

(use-package midnight
  :init
  (midnight-mode 1))

(use-package ibuffer
  :config
  (setq ibuffer-saved-filter-groups
	(quote (("default"
		 ("java" (or
			  (mode . jdee-mode)
			  (mode . java-mode)))
		 ("gradle" (name . ".*gradle$"))
		 ("shell" (mode . shell-mode))
		 ("dired" (mode . dired-mode))
		 ("emacs" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")))
		 ))))
  (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package windmove
  :bind (("M-p" . windmove-up)
	 ("M-n" . windmove-down)
	 ("M-j" . windmove-left))
  ("M-l" . windmove-right))


(use-package multiple-cursors
  :bind ("C-c m l" . mc/edit-lines)
  ("C-c m a" . mc/mark-all-like-this)
  ("C-c >" . mc/mark-next-like-this)
  :config (setq mc/list-file (locate-user-emacs-file "local/.mc-lists.el")))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))


(use-package compile
  :config
  (setq compilation-error-regexp-alist
	(list
	 ;; gradle
	 '("\\(/.+\\):\\([0-9]+\\): error:" 1 2)

	 ;; eclipce java compiler
	 '("\\(.+\\):\\([0-9]+\\): error:" 1 2)
	 '("\\(.+\\):\\([0-9]+\\): warning:" 1 2 nil 1)

	 ;; gradle
	 '("\\(^:.+?:\\(jar\\|testJar\\)\\)\\{0,1\\}\\(.*?\\):\\([0-9]*?\\): error:" 3 4)
	 '("\\(^:.+?:\\(jar\\|testJar\\)\\)\\{0,1\\}\\(.*?\\):\\([0-9]*?\\): warning:" 3 4 nil 1))))


;; TODO Cedet, Semantic
;; (require 'cedet)
;; (require 'semantic)
;; ;; (require 'semanticdb)
;; ;; (setq semantic-default-submodes
;; ;;       '(global-semanticdb-minor-mode
;; ;;         global-semantic-decoration-mode
;; ;;         ;; global-semantic-mru-bookmark-mode
;; ;;         global-srecode-minor-mode
;; ;;         global-semantic-highlight-func-mode
;; ;;         global-semantic-stickyfunc-mode))


;; (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode))
;; (semantic-mode 0)

(use-package org
  :bind (("<f10>" . org-clock-goto)
	 ("<f11>" . org-clock-in-last)
	 ("<f12>" . org-clock-out)
	 ("\C-cl" . org-store-link)
	 ("\C-ca" . org-agenda)
	 ("\C-cb" . org-iswitchb))


  :config
  (setq org-directory (expand-file-name "~/org")
	org-todo-keywords '((sequence "TODO(t)" "BG(b)" "|" "DONE(d!/!)")
			    (sequence "TODO(t)" "FG(b)" "|" "DONE(d!/!)")
			    (sequence "BG" "FG(b)" "|" "DONE(d!/!)")
			    (sequence "FG" "BG(b)" "|" "DONE(d!/!)"))
	org-todo-keyword-faces '(("TODO"  . (:foreground "orange red" :weight bold))
				 ("DONE"  . (:foreground "forest green" :weight bold))
				 ("BG"  . (:foreground "color-148" :weight bold))
				 ("FG"  . (:foreground "color-34" :weight bold)))
	org-clock-persist-file (locate-user-emacs-file "local/org-clock-save.el")
	org-log-done t
	org-use-fast-todo-selection t
	org-clock-persist 'history
	org-completion-use-ido t)

  (org-clock-persistence-insinuate)
  ;; active Babel languages
  (org-babel-do-load-languages 'org-babel-load-languages '((sh . t))))


;; load local settings
(dolist
    (local-lib (directory-files (locate-user-emacs-file "local/lisp") t "\\w+"))
  (load-library local-lib))
