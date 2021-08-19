;;; org-extras.el --- Miscellaneous Org extras    -*- lexical-binding: t; -*-

;;; arXiv links

(require 'org)

(org-link-set-parameters
 "arXiv"
 :face 'org-link
 :follow (lambda (path)
           (when (string-match "^\\(.*\\)/\\(.*\\)$" path)
             (browse-url
              (format "http://arxiv.org/abs/%s"
                      (match-string 1 path)))))
 :export (lambda (path desc backend)
           (when (and (memq backend '(latex html))
                      (null desc)
                      (string-match "^\\(.*\\)/\\(.*\\)$" path))
             (format
              (pcase backend
                ('latex "\\href{https://arxiv.org/abs/%s}{\\texttt{arXiv:%s [%s]}}")
                ('html "<a href=\"https://arxiv.org/abs/%s\"><code>arXiv:%s [%s]</code></a>"))
              (match-string 1 path)
              (match-string 1 path)
              (match-string 2 path)))))

;;; doi links

(org-link-set-parameters
 "doi" ; already has a sensible :follow
 :export (lambda (path desc backend)
           (when (and (memq backend '(latex html))
                      (null desc))
             (format
              (pcase backend
                ('latex "\\href{https://doi.org/%s}{\\texttt{doi:%s}}")
                ('html "<a href=\"https://doi.org/%s\"><code>doi:%s</code></a>"))
              path
              path))))

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
