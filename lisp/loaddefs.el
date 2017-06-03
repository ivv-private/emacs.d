;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "jdb-sourcepath" "jdb-sourcepath.el" (22830
;;;;;;  65120 119051 600000))
;;; Generated autoloads from jdb-sourcepath.el

(defvar jdb-sourcepath-root "~/usr/src" "\
Root directory which contains a sources")

(defvar jdb-sourcepath-prune '("target" ".svn" "arch" "examples" "tests" "test" "samples" "." "..") "\
Directories to skip while searching")

(defvar jdb-rcfile "~/.jdbrc" "\
Jdb rc file")

(autoload 'jdb-setup "jdb-sourcepath" "\
Find  java-sourcepathes within `jdb-sourcepath-root' and write to `jdb-rcfile'

\(fn)" t nil)

(autoload 'jdb-sourcepath-from-rc "jdb-sourcepath" "\
Get list of sourcepathes from `jdb-rcfile' file

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "jira" "jira.el" (22831 1243 300113 758000))
;;; Generated autoloads from jira.el

(defvar org-jira-x-jira-url "" "\
Jira server url")

(defvar org-jira-x-username "" "\
Jira username")

(defvar org-jira-x-password "" "\
Jira password")

(defvar org-jira-x-progress-trans nil "\
Jira transitions")

(autoload 'org-jira-x-issue "jira" "\
Add Jira ticket as todo item

\(fn JID)" t nil)

(autoload 'org-jira-x-update-worklog "jira" "\
Update worklog

\(fn)" t nil)

(autoload 'org-jira-x-do-progress "jira" "\
Update Jira ticket

\(fn)" t nil)

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
