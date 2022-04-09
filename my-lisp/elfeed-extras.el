;;; elfeed-extras.el --- Commands for specific feeds   -*- lexical-binding: t; -*-

(require 'elfeed)

(defun elfeed-extras--entry ()
  "Return the current entry in either Elfeed search or show mode."
  (or (elfeed-search-selected t) elfeed-show-entry))

(defun elfeed-extras--browse (url generic)
  "Browse URL with `browse-url' or, if GENERIC, with `browse-url-generic'."
  (if generic (browse-url-generic url) (browse-url url)))

(defun elfeed-extras--get-link-by-title (title)
  "Return the link in entry contents with TITLE, if any."
  (when-let ((entry (elfeed-extras--entry))
             (content (elfeed-deref (elfeed-entry-content entry))))
    (when (string-match
           (format "<a href=\"\\([^\"]+\\)\">[^<]*%s[^<]*</a>" title)
           content)
      (match-string 1 content))))

(defun elfeed-extras-comments (&optional generic)
  "Browse comments in current entry."
  (interactive "P")
  (when-let ((url (elfeed-extras--get-link-by-title "comments")))
    (elfeed-extras--browse url generic)))

(defun elfeed-extras-link (&optional generic)
  "Browse link named 'link' in current entry."
  (interactive "P")
  (when-let ((url (elfeed-extras--get-link-by-title "link")))
    (elfeed-extras--browse url generic)))

(defun elfeed-extras-arxiv-pdf (&optional generic)
  "View the PDF corresponing to an arXiv submission in Elfeed."
  (interactive "P")
  (when-let ((abs "^https?://arxiv.org/abs/\\(.*\\)$")
             (entry (elfeed-extras--entry))
             (url (elfeed-entry-link entry))
             ((string-match abs url))
             (pdf (format "https://arxiv.org/pdf/%s.pdf" (match-string 1 url))))
    (elfeed-extras--browse pdf generic)))

(defun elfeed-extras-youtube ()
  "Browse first embedded YouTube link in an external browser."
  (interactive)
  (when-let ((entry (elfeed-extras--entry)))
    (url-retrieve
     (elfeed-entry-link entry)
     (lambda (_)
       (when (re-search-forward
              "https://www\\.youtube\\.com/embed/\\([A-Za-z0-9_-]+\\)" nil t)
         (browse-url-generic
          (concat "https://www.youtube.com/watch?v=" (match-string 1)))))
     nil t t)))

(provide 'elfeed-extras)
