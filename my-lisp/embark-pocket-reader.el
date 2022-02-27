;;; embark-pocket-reader.el --- Target URLs in pocket-reader   -*- lexical-binding: t; -*-

(require 'pocket-reader)
(require 'embark)

(defun embark-pocket-reader-target-url ()
  "Target the URL of the pocket-reader item at point."
  (when-let (((derived-mode-p 'pocket-reader-mode))
             (id (tabulated-list-get-id))
             (item (gethash id pocket-reader-items))
             (url (pocket-reader--get-url item)))
    `(url ,url ,(line-beginning-position) . ,(line-end-position))))

(add-to-list 'embark-target-finders #'embark-pocket-reader-target-url)

(provide 'embark-pocket-reader)
