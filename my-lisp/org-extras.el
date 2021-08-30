;;; org-extras.el --- Miscellaneous Org extras    -*- lexical-binding: t; -*-

;;; arXiv links

(require 'org)

(defun org-extras--link-exporter
    (path-regexp url-format title-format-1 &optional title-format-2)
  (lambda (path description backend)
    (when (and (memq backend '(latex html))
               (string-match path-regexp path))
      (format (pcase backend
                ('latex "\\href{%s}{%s}")
                ('html "<a href=\"%s\">%s</a>"))
              (format url-format (match-string 1 path))
              (or description
                  (format (pcase backend
                            ('latex "\\texttt{%s}")
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
   :follow (lambda (path)
             (when (string-match arxiv-regexp path)
               (browse-url
                (format arxiv-url-format (match-string 1 path)))))
   :export (org-extras--link-exporter
            arxiv-regexp arxiv-url-format "arXiv:%s" "arXiv:%s [%s]")))

;;; doi links

(org-link-set-parameters
 "doi" ; already has a sensible :follow
 :export (org-extras--link-exporter "^\\(.*\\)$" "https://doi.org/%s" "doi:%s"))

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

(provide 'org-extras)
