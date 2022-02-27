;;; embark-elfeed.el --- Target feed entry URLs in elfeed   -*- lexical-binding: t; -*-

(require 'elfeed)
(require 'embark)

(defun embark-elfeed-target-url ()
  "Target the URL of the elfeed entry at point."
  (when-let (((derived-mode-p 'elfeed-search-mode))
             (entry (elfeed-search-selected :ignore-region))
             (url (elfeed-entry-link entry)))
    `(url ,url ,(line-beginning-position) . ,(line-end-position))))

(defun embark-elfeed-url-candidates ()
  "Target the URLs of the selected elfeed entries."
  (when-let (((derived-mode-p 'elfeed-search-mode))
             (entries (elfeed-search-selected))
             (urls (mapcar #'elfeed-entry-link entries)))
    (cons 'url urls)))

(defun embark-elfeed-target-first-url-in-content ()
  "Target the first URL in the content of the entry at point."
  (when-let (((derived-mode-p 'elfeed-search-mode))
             (entry (elfeed-search-selected :ignore-region))
             (content (elfeed-deref (elfeed-entry-content entry)))
             (url (when (string-match "a href=\"\\([^\"]+\\)\"" content)
                    (match-string 1 content))))
    `(url ,url ,(line-beginning-position) . ,(line-end-position))))

(add-to-list 'embark-target-finders #'embark-elfeed-target-first-url-in-content)
(add-to-list 'embark-target-finders #'embark-elfeed-target-url)
(add-to-list 'embark-candidate-collectors #'embark-elfeed-url-candidates)

(provide 'embark-elfeed)
