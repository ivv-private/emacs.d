;; load-path
(let ((default-directory
        (concat user-emacs-directory
                (convert-standard-filename "site-lisp/"))
        ))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; settings
(setq custom-file
      (concat user-emacs-directory
	      (convert-standard-filename ".emacs-custom")))
(server-start)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")
;; interface
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(ansi-color-for-comint-mode-on)
;;
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default dabbrev-case-fold-search t)
(put 'narrow-to-region 'disabled nil)
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
                (mode . shell-mode))
               ("java"
                (or
                 (mode . malabar-mode)))
               ("groovy"
                (mode . groovy-mode))
               ("dired"
                (mode . dired-mode))
               ("xml"
                (mode . nxml-mode))
               ))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

;; switch-window
(require 'switch-window)

;; auto-clean buffers
(require 'midnight)

;; vm
(require 'vm-autoloads)

;; sip

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
(require 'starttls)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      user-mail-address "ivv.private@gmail.com" ; make sure to change this
      smtpmail-debug-info t)


;; dict
(require 'dictem)
(dictem-initialize)
(global-set-key "\C-cs" 'dictem-run-search)
(global-set-key "\C-cm" 'dictem-run-match)
(global-set-key "\C-cd" 'dictem-run-define)
(global-set-key "\C-c\M-r" 'dictem-run-show-server)
(global-set-key "\C-c\M-i" 'dictem-run-show-info)
(global-set-key "\C-c\M-b" 'dictem-run-show-databases)

;; lang

;; log4j
(require 'log4j-mode)
(add-to-list 'auto-mode-alist '("server.log" . log4j-mode))
(add-hook
 'log4j-mode-hook
 (lambda ()
   (setq truncate-lines t)
   (text-scale-decrease 1)
   (toggle-read-only 1)
   (buffer-disable-undo t)))

;; jtags
(autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
(add-hook 'java-mode-hook 'jtags-mode)

;; CEDET
(require 'cedet)

;; Enable EDE (Project Management) features
(global-ede-mode 1)
(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
(semantic-load-enable-gaudy-code-helpers)

(require 'semantic-ia)
(require 'semantic-java)

;; malabar
(require 'malabar-mode)
(setq malabar-groovy-lib-dir "~/.emacs.d/site-lisp/malabar-1.5-SNAPSHOT/lib")
(add-to-list 'auto-mode-alist
             '("\\.java\\'" . malabar-mode))

;; lang sql
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

;; javaScript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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

;;(require 'groovy-mode)
;;(add-to-list 'auto-mode-alist
;;             '("\\.groovy\\'" . groovy-mode))


(load custom-file)
