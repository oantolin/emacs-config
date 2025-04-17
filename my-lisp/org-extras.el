;;; org-extras.el --- Miscellaneous Org extras    -*- lexical-binding: t; -*-

;;; arXiv links

(require 'org)

(defun org-extras--link-exporter
    (path-regexp url-format title-format-1 &optional title-format-2)
  (lambda (path description backend)
    (when (and (memq backend '(latex html))
               (string-match path-regexp path))
      (format (pcase backend
                ((or 'latex 'beamer) "\\href{%s}{%s}")
                ('html "<a href=\"%s\">%s</a>"))
              (format url-format (match-string 1 path))
              (or description
                  (format (pcase backend
                            ((or 'latex 'beamer) "\\texttt{%s}")
                            ('html "<code>%s</code>"))
                          (format (if (match-string 2 path)
                                      title-format-2
                                    title-format-1)
                                  (match-string 1 path)
                                  (match-string 2 path))))))))

(let ((arxiv-regexp "^\\(.*?\\)\\(?:/\\(.*\\)\\)?$")
      (arxiv-url-format "https://arxiv.org/abs/%s"))
  (org-link-set-parameters
   "arXiv"
   :face 'org-link
   :follow (lambda (path in-emacs)
             (when-let (((string-match arxiv-regexp path))
                        (url (format arxiv-url-format (match-string 1 path))))
               (if in-emacs (eww url) (browse-url url))))
   :export (org-extras--link-exporter
            arxiv-regexp arxiv-url-format "arXiv:%s" "arXiv:%s [%s]")))

;;; doi links

(defun beamer-is-latex (args)
  "Treat Beamer backend same as LaTeX."
  (pcase-let ((`(,path ,desc ,backend ,info) args))
    `(,path ,desc ,(if (eq backend 'beamer) 'latex backend) ,info)))

(defun default-doi-desc (args)
  "If no description is given use doi:... format."
  (pcase-let ((`(,path ,desc ,backend ,info) args))
    `(,path ,(or desc (concat "doi:" path)) ,backend ,info)))
  
(advice-add 'org-link-doi-export :filter-args #'beamer-is-latex)
(advice-add 'org-link-doi-export :filter-args #'default-doi-desc)

;;; Inline JavaScript

(add-to-list 'org-src-lang-modes '("inline-js" . javascript))

(defvar org-babel-default-header-args:inline-js
  '((:results . "html")
    (:exports . "results")))

(defun org-babel-execute:inline-js (body _params)
  (format "<script type=\"text/javascript\">\n%s\n</script>" body))

;;; Source blocks for declaring LaTeX macros

(add-to-list 'org-src-lang-modes '("latex-macros" . latex))

(defvar org-babel-default-header-args:latex-macros
  '((:results . "raw")
    (:exports . "results")))

(defun prefix-all-lines (pre body)
  (with-temp-buffer
    (insert body)
    (string-insert-rectangle (point-min) (point-max) pre)
    (buffer-string)))

(defun org-babel-execute:latex-macros (body _params)
  (concat
   (prefix-all-lines "#+LATEX_HEADER: " body)
   "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
   (prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
   "\n#+HTML_HEAD_EXTRA: \\)</div>\n"))

(defun no-short-tags (tags)
  "Do not offer very short tags as completion candidates.
Use as `:filter-return' advice for `org-get-buffer-tags'."
  (mapcar (lambda (group)
            (seq-filter (lambda (tag) (> (length tag) 2)) group))
          tags))

(advice-add 'org-get-buffer-tags :filter-return #'no-short-tags)

(provide 'org-extras)
