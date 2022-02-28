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

(defun elfeed-extras-comments ()
  "Browse comments in current entry."
  (interactive)
  (when-let ((url (elfeed-extras--get-link-by-title "comments")))
    (browse-url url)))

(defun elfeed-extras-arxiv-pdf ()
  "View the PDF corresponing to an arXiv submission in Elfeed."
  (interactive)
  (when-let ((abs "^https?://arxiv.org/abs/\\(.*\\)$")
             (pdf "https://arxiv.org/pdf/%s.pdf")
             (item (or (elfeed-search-selected :single) elfeed-show-entry))
             (url (elfeed-entry-link item))
             ((string-match abs url)))
    (browse-url (format pdf (match-string 1 url)))))

(provide 'elfeed-extras)
