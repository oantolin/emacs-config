;; arXiv.el --- Misc commands for arXiv -*- lexical-binding: t; -*-

(defun arXiv--id (paper)
  "Return the arXiv id of PAPER.
A ragtag bunch of ad hoc formats are recognized for PAPER."
  (setq paper (replace-regexp-in-string "_" "." paper)) ; Mastodon
  (cl-loop for pattern in '("^%s$"
                            "^arXiv:%s\\(?:/.*\\)?$"
                            "^https?://arxiv.org/abs/%s$" 
                            "^https://mathstodon.xyz/tags/arXiv.%s$")
           when (string-match (format pattern "\\([0-9v.]+\\)") paper)
           return (match-string 1 paper)))

(defun arXiv-pdf (paper &optional generic)
  "Open the PDF version of PAPER from the arXiv.
A ragtag bunch of ad hoc formats are recognized for PAPER.
If GENERIC is non-nil use `url-browse-generic' rather than
`browse-url'."
  (interactive "sarXiv paper: \nP")
  (when (and (not generic) (fboundp 'pdf-tools-install))
    (pdf-tools-install))
  (let ((pdf (format "https://arxiv.org/pdf/%s.pdf" (arXiv--id paper))))
    (if generic
        (browse-url-generic pdf)
      (browse-url pdf))))

(defvar url-http-end-of-headers)

(require 'dom)

(defun arXiv--with-metadata (paper fn)
  "Download and parse arXiv metadata for PAPER.
Call FN on an alist with keys `title', `authors', `url', `id' and
`abstract'."
  (url-retrieve
   (format "http://export.arxiv.org/api/query?id_list=%s" (arXiv--id paper))
   (lambda (_)
     (goto-char url-http-end-of-headers)
     (forward-line 2)
     (let* ((xml (libxml-parse-xml-region (point)))
            (entry (car (dom-by-tag xml 'entry)))
            (title (dom-text (dom-by-tag entry 'title)))
            (abstract (dom-text (dom-by-tag entry 'summary)))
            (authors (mapcar #'dom-texts (dom-by-tag entry 'author)))
            (url (dom-text (dom-by-tag entry 'id)))
            (id (string-remove-prefix "http://arxiv.org/abs/" url)))
       (funcall fn `((title . ,title) (authors . ,authors)
                     (abstract . ,abstract) (url . ,url) (id . ,id)))))))

(defun arXiv-show (paper)
  "Popup a buffer with the title, authors and abstract of PAPER.
A ragtag bunch of ad hoc formats are recognized for PAPER."
  (interactive "sarXiv paper: ")
  (arXiv--with-metadata
   paper
   (lambda (data)
     (let-alist data
       (with-output-to-temp-buffer (format "*arXiv:%s*" .id)
         (princ (format "%s\nBy: %s\n\n%s\n"
                        .title (string-join .authors ", ") .abstract)))))))

(defun arXiv-capture (paper)
  "Capture Org entry for arXiv PAPER at point.
(This depends on the `org-capture' template \"a\" just inserting
the kill ring head. 😬)"
  (interactive "sarXiv paper: ")
  (arXiv--with-metadata
   paper
   (lambda (data)
     (let-alist data
       (kill-new (format "[[%s][%s]]\nBy: %s\n\n%s\n"
                         .url .title (string-join .authors ", ") .abstract)))
     (org-capture nil "a"))))

(defvar-keymap arXiv-map
  "p" #'arXiv-pdf
  "s" #'arXiv-show
  "c" #'arXiv-capture)

(fset 'arXiv-map arXiv-map)

(provide 'arXiv)
