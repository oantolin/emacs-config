;;; elfeed-extras.el --- Commands for specific feeds   -*- lexical-binding: t; -*-

(require 'elfeed)

(defun elfeed-extras--get-link-by-title (title)
  "Return the link in entry contents with TITLE, if any."
  (when-let ((entry (or (elfeed-search-selected t) elfeed-show-entry))
             (content (elfeed-deref (elfeed-entry-content entry))))
    (when (string-match
           (format "<a href=\"\\([^\"]+\\)\">[^<]*%s[^<]*</a>" title)
           content)
      (match-string 1 content))))

(defun elfeed-extras-comments (&optional generic)
  "Browse comments in current entry."
  (interactive "P")
  (when-let ((url (elfeed-extras--get-link-by-title "comments")))
    (if generic (browse-url-generic url) (browse-url url))))

(defun elfeed-extras-link (&optional generic)
  "Browse link named 'link' in current entry."
  (interactive "P")
  (when-let ((url (elfeed-extras--get-link-by-title "link")))
    (if generic (browse-url-generic url) (browse-url url))))

(defun elfeed-extras-arxiv-pdf (&optional generic)
  "View the PDF corresponing to an arXiv submission in Elfeed."
  (interactive "P")
  (when-let ((abs "^https?://arxiv.org/abs/\\(.*\\)$")
             (pdf "https://arxiv.org/pdf/%s.pdf")
             (item (or (elfeed-search-selected t) elfeed-show-entry))
             (url (elfeed-entry-link item))
             ((string-match abs url)))
    (if generic (browse-url-generic url) (browse-url url))))

(provide 'elfeed-extras)
