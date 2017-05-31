;;;###autoload
(defvar org-jira-x-jira-url ""
  "Jira server url")

;;;###autoload
(defvar org-jira-x-username ""
  "Jira username")

;;;###autoload
(defvar org-jira-x-password ""
  "Jira password")

;;;###autoload
(defvar org-jira-x-progress-trans ()
  "Jira transitions")

(defun org-jira-x-get-issue (jid)
  "Get jira issue"
  (let (
	(url-request-extra-headers
	 (list
	  (cons "Authorization"
		(concat "Basic " (base64-encode-string (concat org-jira-x-username ":" org-jira-x-password))))))
	issue)
    (with-current-buffer
	(url-retrieve-synchronously (format "%s/rest/api/latest/issue/%s" org-jira-x-jira-url jid))
      (goto-char (point-min))
      (when (search-forward-regexp "^$" nil t)
	(set 'issue (json-read))
	(kill-buffer (current-buffer))
	issue))))

(defun org-jira-x-post-issue (jid path data)
  "Post data to Jira"
  (let ((url-request-extra-headers
	 (list
	  (cons "Authorization" (concat "Basic " (base64-encode-string (concat org-jira-x-username ":" org-jira-x-password))))
	  (cons "Content-Type" "Application/json")))
	(url-request-data data)
	(url-request-method "POST"))
    (with-current-buffer
	(url-retrieve-synchronously
	 (format "%s/rest/api/latest/issue/%s/%s" org-jira-x-jira-url jid path))
      ;; todo analyze result
      (kill-ring-save (point-min) (point-max))
      (kill-buffer (current-buffer)))))

;;;###autoload
(defun org-jira-x-issue(jid)
  "Add Jira ticket as todo item"
  (interactive (list (read-string "Jira ID: ")))
  (let* ((issue (org-jira-x-get-issue jid))
	 (issue-id (assoc-default 'key issue))
	 (issue-summary (assoc-default 'summary (assoc-default 'fields issue)))
	 (timetracking (assoc-default 'timetracking (assoc-default 'fields issue)))
	 (tags (let ((end 0)
		     (str issue-summary)
		     (tags '()))
		 (while (string-match "\\[\\(.+?\\)\\]" str end)
		   (add-to-list 'tags (replace-regexp-in-string "[. -]" "" (match-string 1 str)))
		   (setq end (match-end 1)))
		 tags)))
    
    (if (string-match "pre-prod" issue-summary)
	(add-to-list 'tags "preprod"))
    
    (org-insert-todo-heading-respect-content t)
    (org-insert-link "t" (concat "jira:" issue-id) issue-id)
    (insert " | " (replace-regexp-in-string "\\( *\\[.*?\\] *\\|DEV:\\)" "" issue-summary))
    (org-set-tags-to (mapcar 'downcase tags))
    (org-set-property "JiraId" (assoc-default 'key issue))
    (org-set-property "Effort" (format "%d" (/ (assoc-default 'originalEstimateSeconds timetracking) 60)))))

;;;###autoload
(defun org-jira-x-update-worklog()
  "Update worklog"
  (interactive)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (if (org-clock-find-position t)
	  (let* (beg end comment
		     (jira-id (org-entry-get (point) "JiraId"))
		     (synced (mapcar
			      (lambda (it) (assoc-default 'comment it))
			      (assoc-default 'worklogs (assoc 'worklog (assoc 'fields (org-jira-x-get-issue jira-id)))))))
	    (while (re-search-forward org-tr-regexp-both nil t)
	      (setq beg (match-string 1)
		    end (match-string 2)
		    comment (concat beg "--" end))
	      (let* ((pdate (org-parse-time-string beg))
		     (yy (nth 5 pdate))
		     (mm (nth 4 pdate))
		     (dd (nth 3 pdate))
		     (hh (nth 2 pdate))
		     (mi (nth 1 pdate))
		     (xmlbeg (format "%d-%02d-%02dT%02d:%02d:00.000+0000" yy mm dd hh mi)))
		(if (not (member comment synced))
		    (org-jira-x-post-issue
		     jira-id
		     "worklog"
		     (json-encode-list
		      (list
		       (cons "comment" comment)
		       (cons "started" xmlbeg)
		       (cons "timeSpentSeconds"
			     (int-to-string (* 60 (org-clock-sum
						   (apply 'encode-time (org-parse-time-string beg))
						   (apply 'encode-time (org-parse-time-string end))))))
		       )))
		  ))))))))

;;;###autoload
(defun org-jira-x-do-progress ()
  "Update Jira ticket"
  (interactive)
  (save-excursion
    (save-restriction
      (let ((jira-id (org-entry-get (point) "JiraId")))

	(dolist (next-transition (helm :sources (helm-build-sync-source "test"
									:candidates org-jira-x-progress-trans
									:fuzzy-match t)
				       :buffer "*jira do progress*"))
	  (message next-transition)
	  (org-jira-x-post-issue jira-id "transitions"
				 (json-encode-list
				  (list
				   (cons "fields"
					 (list
					  (cons "assignee"
						(list
						 (cons "name" org-jira-x-username)))))
				   (cons "transition" (list (cons "id" next-transition)))))))))))


