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

(add-to-list 'embark-target-finders #'embark-elfeed-target-url)
(add-to-list 'embark-candidate-collectors #'embark-elfeed-url-candidates)

(provide 'embark-elfeed)
