;;; pocket-reader-extras.el --- Minor tweaks to pocket-reader   -*- lexical-binding: t; -*-

(require 'pocket-reader)

(defun pocket-reader-eww-add-page ()
  "Add the URL of the current page to Pocket.
Compare with `pocket-reader-eww-add-link', which adds the URL of
the link at point (and is actually unnecessary because of
`pocket-reader-shr-add-link'...)."
  (interactive)
  (let ((url (plist-get eww-data :url)))
    (when (pocket-lib-add-urls url)
      (message "Added: %s" url))))

(with-eval-after-load 'elfeed
  (defun pocket-reader-extras--mark-as-read ()
    "Mark selected entries as read."
    (let ((entries (elfeed-search-selected)))
      (elfeed-untag entries 'unread)
      (mapc #'elfeed-search-update-entry entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line))))
  (advice-add 'pocket-reader-elfeed-search-add-link
              :after-while #'pocket-reader-extras--mark-as-read))

(provide 'pocket-reader-extras)
