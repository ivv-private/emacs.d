(setq user-emacs-directory "~/config/emacs.d/")

(make-directory (locate-user-emacs-file "local") :no-error)
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; Setup package manager
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)) 
(require 'use-package)

;; Customization
(setf backup-inhibited t
      auto-save-default nil
      auto-save-list-file-prefix (locate-user-emacs-file "local/saves")
      inhibit-startup-message t
      initial-scratch-message nil
      echo-keystrokes 0.1
      delete-active-region nil
      disabled-command-function nil
      custom-file (make-temp-file "emacs-custom")
      large-file-warning-threshold 536870911
      gc-cons-threshold (* 1024 1024 32)
      ring-bell-function (lambda ())
      redisplay-dont-pause t
      column-number-mode t)

(prefer-coding-system 'utf-8-unix)

(ansi-color-for-comint-mode-on)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setf vc-handled-backends nil
      vc-follow-symlinks t)

 ;; '(explicit-sh-args (quote ("-login" "-i")))
 ;; '(fill-column 120)
;; '(indent-tabs-mode t)
;; '(midnight-mode t nil (midnight))
;; '(show-paren-mode t nil (paren))
 ;; '(standard-indent 1)
 ;; '(tab-width 4)
 ;; '(user-full-name "Ignatyev Vjacheslav")
;; '(user-mail-address "ignatyev@lanit.ru")
;; '(warning-suppress-types (quote ((\(undo\ discard-info\)))))
 ;; '(whitespace-line-column nil)
 ;; '(whitespace-style
 ;;   (quote
 ;; 	(face trailing space-before-tab newline indentation empty space-after-tab)))




;; UI Customization

(transient-mark-mode 1)

(when (fboundp 'set-horizontal-scroll-bar-mode)
  (set-horizontal-scroll-bar-mode nil))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keybindings
(global-set-key [f8] 'toggle-truncate-lines)
(global-set-key "\C-w" 'backward-kill-word) 
(global-set-key "\C-x\C-k" 'kill-region) 
(global-set-key "\M-`" 'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-s M-s") 'rgrep)
(global-set-key (kbd "M-r") 'replace-regexp)
(global-set-key (kbd "C-x C-g") 'revert-buffer)
(global-set-key [f8] 'toggle-truncate-lines)
(global-set-key (kbd "C-z") 'compile)


;; Packages

(use-package dabbrev
  :defer t
  :init (setf abbrev-file-name (locate-user-emacs-file "local/abbrev_defs"))
  :config (setf dabbrev-case-fold-search nil))


(use-package glasses
  :config  (setq glasses-original-separator "-"
		 glasses-separator "-"
		 glasses-uncapitalize-p t))

(use-package edit-server
  :init
  (setq server-socket-dir (format "%sserver" user-emacs-directory))
  (add-hook 'after-init-hook 'server-start t))    
  
(use-package comint
  :config (setq
	   comint-completion-autolist t
	   comint-input-ignoredups t
	   comint-use-prompt-regexp t))

(use-package compile
  :config (setq
	   compilation-always-kill t
	   compilation-ask-about-save nil
	   compilation-auto-jump-to-first-error t
	   compilation-scroll-output nil
	   compilation-window-height 20))

(use-package ivy
  :config (setq
	   ivy-ignore-buffers (quote ("\\` " "^\\*magit-\\(process\\|diff\\)"))))

(use-package counsel
  :config (setq counsel-find-file-at-point t))

(use-package jdee
  :bind (("C-c C-v C-u" jdee-package-update)
	 ([f5] jdee-x-single-test-set)
	 ([f10] jdee-open-project-file)
	 ([f6] jdee-x-run-test)
	 ([f1] (lambda () (interactive) (switch-to-buffer (car (get-buffers-matching-mode 'gud-mode)))))
	 ([f2] gud-step)
	 ([f3] gud-next)
	 ([f7] gud-cont)
	 ([f7] gud-cont)

	 )
  :config (setq
	   jdee-jdk-registry '(("1.8.0_60" . "~/opt/jdk/8u60"))
	   jdee-compiler '(("eclipse java compiler server" "~/usr/lib/ecj-4.5.jar"))	   
	   jdee-compile-option-hide-classpath t
	   jdee-flycheck-enable-p nil
	   jdee-server-dir "~/usr/lib/jdee"
	   jdee-import-group-function 'jdee-import-group-first-level
	   )

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
    (let* ((sources
	    (delete-if-not
	     'file-exists-p
	     (mapcar (apply-partially
		      'expand-file-name
		      (jdee-package-get-package-directory)) jdee-sourcepath)))
	   (test-file-name
	    (concat (jdee-junit-get-tester-name
		     (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
		    ".java"))
	   (fn (find-if 'file-exists-p  (mapcar (apply-partially 'expand-file-name test-file-name) sources))))
      (if fn
	  (find-file fn)
	(progn
	  (setq default-directory (car (last sources)))
	  (call-interactively 'find-file)))))
  (defvar jdee-x-buffer-test nil
    "Test buffer that is binded to current java source")

  (defvar jdee-x-single-test ""
    "Selected test for single executionn")

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
      ;; (helm :sources (helm-build-sync-source "tests" :candidates '()) :buffer "*select junit test*")
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

  (defun java-x-before-save ()
    (delete-trailing-whitespace)
    ;; (jdee-import-kill-extra-imports)
    (jdee-import-organize))

  (defun java-x-after-save ()
    (jdee-compile))

  (defun java-my-minor ()
    (progn
      (glasses-mode t)
      (setq-default indent-tabs-mode t)
      (whitespace-mode t)

      (local-set-key (kbd "M-RET") 'jdee-complete-minibuf)
      (add-hook 'before-save-hook 'java-x-before-save nil t)
      (add-hook 'after-save-hook  'java-x-after-save nil t)
      ))

  (add-hook 'jdee-mode-hook 'java-my-minor)

  (defgroup maglev-faces nil
    "Faces for displaying Maglev logs."
    :group 'applications)

  (defface maglev-log
    '((t (:background "gold" :foreground "black")))
    "General face for Maglev log"
    :group 'maglev-faces)

  (defface maglev-client-in
    '((t (:inherit 'maglev-log)))
    "Face for client to market."
    :group 'maglev-faces)

  (defface maglev-external-out
    '((t (:inherit 'maglev-log)))
    "Face for adapter to market."
    :group 'maglev-faces)


  (defface maglev-client-out
    '((t (:inherit 'maglev-log :inverse-video t)))
    "Face for adapter to client."
    :group 'maglev-faces)

  (defface maglev-external-in
    '((t (:inherit 'maglev-log :inverse-video t)))
    "Face for market to adapter."
    :group 'maglev-faces)

  (setq jdee-run-mode-hook ())

  (add-hook
   'jdee-run-mode-hook
   (lambda ()
     (setq truncate-lines t)
     (text-scale-set -1)
     (buffer-disable-undo)


     (setq hi-lock-interactive-patterns ())

     (highlight-regexp "\\<ERROR\\>.*" 'compilation-error)
     (highlight-regexp "\\<WARN\\>.*" 'compilation-warning)
     (highlight-regexp "\\<DEBUG\\>.*" 'font-lock-preprocessor-face)
     (highlight-regexp "\\<INFO\\>.*" 'font-lock-function-name-face)

     
     (highlight-regexp "Expected:.*" 'compilation-info)
     (highlight-regexp "Actual:.*" 'compilation-warning)

     (highlight-regexp "CLIENT_IN.*" 'maglev-client-in)
     (highlight-regexp "CLIENT_OUT.*" 'maglev-client-out)
     (highlight-regexp "EXTERNAL_IN.*" 'maglev-external-in)
     (highlight-regexp "EXTERNAL_OUT.*" 'maglev-external-out)

     (highlight-regexp "\\(Executing test:.*\\| TEST STARTED.* \\)" 'bold)

     (highlight-regexp "|35=[^|]+|" 'mode-line-highlight)

     ))


)

(use-package gud
  :bind (([f1] (lambda () (interactive) (switch-to-buffer (car (get-buffers-matching-mode 'gud-mode)))))
	 ([f2] gud-step)
	 ([f3] gud-next)
	 ([f7] gud-cont)
	 ([f7] gud-cont))
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

  (use-package jdb-sourcepath
    :config
    (add-hook
     'jdb-mode-hook
     (lambda ()
       (setq gud-jdb-classpath (jdb-sourcepath-from-rc)
	     comint-prompt-regexp "^.*\] ")))))

(use-package yas
  :config (yas-global-mode t))

(use-package ffap
  :config (ffap-bindings))

(use-package ido
  :config (ido-mode t))

(use-package ivy
  :bind (("C-c SPC" ivy-resume)
	 ("M-o" ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t))
  
(use-package counsel
    :bind
    (("M-x" counsel-M-x)
     ("C-x C-f" counsel-find-file)
     ("C-x r b" counsel-bookmark)
     ("C-x l" counsel-locate)
     ("C-x g" counsel-git)))
;; (global-set-key "\M-." 'counsel-gtags-find-definition)

(use-package swiper
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
  :bind (("M-," avy-goto-word-1)))


(use-package helm-gtags
  :bind (("M-." helm-gtags-find-tag)
	 ("C-c t t" helm-gtags-find-tag)
	 ("C-c t f" helm-gtags-find-files)
	 ("C-c t r" helm-gtags-find-rtag)
	 ("C-c t s" helm-gtags-find-symbol))

  
(use-package magit
  :bind (("C-c g s" magit-status)))

(use-package dired-x
  :config
  (setq dired-omit-files "^\\...+$")
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (rename-buffer (format "*Dired: %s*" (buffer-name))))))


(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

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


(use-package multiple-cursors
  :bind
  ("C-c m l" mc/edit-lines)
  ("C-c m a" mc/mark-all-like-this)
  ("C-c >" mc/mark-next-like-this))

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
  :bind (([f10] org-clock-goto)
	 ([f11] org-clock-in-last)
	 ([f12] org-clock-out)
	 ("\C-cl" 'org-store-link)
	 ("\C-ca" 'org-agenda)
	 ("\C-cb" 'org-iswitchb)
)

  :config

  (setq org-directory (expand-file-name "~/org")
	org-agenda-files (list (expand-file-name "~/org/db.org")
			       (expand-file-name "~/org/db-backlog.org")
			       (expand-file-name "~/org/snippets.org")
			       (expand-file-name "~/org/environments.org")
			       (expand-file-name "~/org/notes.org"))
	org-todo-keywords '((sequence "TODO(t)" "BG(b)" "|" "DONE(d!/!)")
			    (sequence "TODO(t)" "FG(b)" "|" "DONE(d!/!)")
			    (sequence "BG" "FG(b)" "|" "DONE(d!/!)")
			    (sequence "FG" "BG(b)" "|" "DONE(d!/!)"))
  	org-log-done t
	org-use-fast-todo-selection t
	org-todo-keyword-faces '(("TODO"  . (:foreground "orange red" :weight bold))
				 ("NEXT"  . (:foreground "dodger blue" :weight bold))
				 ("DONE"  . (:foreground "forest green" :weight bold))
				 ("WAITING"  . (:foreground "LightSalmon1" :weight bold))
				 ("HOLD"  . (:foreground "gray" :weight bold))
				 ("CANCELLED"  . (:foreground "forest green" :weight bold))
				 ("SOMEDAY"  . (:foreground "pink" :weight bold))
				 ("BG"  . (:foreground "color-148" :weight bold))
				 ("FG"  . (:foreground "color-34" :weight bold)))
	org-clock-persist 'history
	org-completion-use-ido t


	)
  (org-clock-persistence-insinuate)
  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh . t)))
  )
	



