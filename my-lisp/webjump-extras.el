;;; webjump-extras.el --- webjump-sites = eww-bookmarks + searches   -*- lexical-binding: t; -*-

(require 'webjump)
(require 'eww)

(defcustom webjump-search-engines
  '(("DuckDuckGo" . "https://html.duckduckgo.com/html/?q=")
    ("Google" . "https://google.com/search?q=")
    ("Wikipedia" . "https://en.wikipedia.org/wiki/")
    ("DLE RAE" . "https://dle.rae.es/")
    ("IMDb" . "https://www.imdb.com/find?q=")
    ("YouTube" . "https://www.youtube.com/results?search_query=")
    ("Merriam-Webster" . "https://www.merriam-webster.com/dictionary/")
    ("Collins" . "https://www.collinsdictionary.com/dictionary/english/")
    ("arXiv" . "https://arxiv.org/search/?searchtype=all&query=")
    ("math.??" . "https://arxiv.org/list/math. /recent"))
  "Search engines to add to `webjump-sites' as `simply-query' sites."
  :group 'webjump
  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "URL")))

(defun webjump-reload ()
  "Reload `webjump-sites' from eww bookmarks and `webjump-search-engines'."
  (interactive)
  (setq webjump-sites
        (append
         (progn
           (eww-read-bookmarks)
           (mapcar (lambda (bm)
                     (cons (plist-get bm :title) (plist-get bm :url)))
                   eww-bookmarks))
         (mapcar (pcase-lambda (`(,name . ,url))
                   (setq url (split-string url " "))
                   `(,name . [simple-query
                              ,(when (string-match "^.*://[^/]+" (car url))
                                 (match-string 0 (car url)))
                              ,(car url) ,(or (cadr url) "")]))
                 webjump-search-engines))))

(define-advice webjump (:around (fn &rest args) in-eww)
  (let ((browse-url-browser-function
         (if current-prefix-arg #'browse-url-default-browser #'eww)))
    (apply fn args)))

(provide 'webjump-extras)
