;;; elfeed-comments.el --- Command to visit comments   -*- lexical-binding: t; -*-

(require 'elfeed)

(defun elfeed-extras--get-link-by-title (title)
  "Return the link in entry contents with TITLE, if any."
  (when-let ((entry (or (elfeed-search-selected :ignore-region)
                        elfeed-show-entry))
             (content (elfeed-deref (elfeed-entry-content entry))))
    (when (string-match
           (format "<a href=\"\\([^\"]+\\)\">[^<]*%s[^<]*</a>" title)
           content)
      (match-string 1 content))))

(defun elfeed-comments ()
  "Browse comments in current entry."
  (interactive)
  (when-let ((url (elfeed-extras--get-link-by-title "comments")))
    (browse-url url)))

(provide 'elfeed-comments)
