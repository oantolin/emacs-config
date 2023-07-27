;;; webjump-extras.el --- webjump-sites = eww-bookmarks + searches   -*- lexical-binding: t; -*-

(require 'webjump)
(require 'eww)

(defcustom webjump-search-engines
  '(("DuckDuckGo" . "https://html.duckduckgo.com/html/?q=")
    ("Google" . "https://google.com/?q=")
    ("Wikipedia" . "https://en.wikipedia.org/wiki/")
    ("DLE RAE" . "https://dle.rae.es/")
    ("IMDb" . "https://www.imdb.com/find?q=")
    ("YouTube" . "https://www.youtube.com/results?search_query=")
    ("Merriam-Webster" . "https://www.merriam-webster.com/dictionary/")
    ("Collins" . "https://www.collinsdictionary.com/dictionary/english/")
    ("arXiv" . "https://arxiv.org/search/?searchtype=all&query="))
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
                   `(,name . [simple-query
                              ,(when (string-match "^.*://[^/]+" url)
                                 (match-string 0 url))
                              ,url ""]))
                 webjump-search-engines))))

(provide 'webjump-extras)
