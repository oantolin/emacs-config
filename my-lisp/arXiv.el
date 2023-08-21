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

(defun arXiv-show (paper)
  "Popup a buffer with the title, authors and abstract of PAPER.
A ragtag bunch of ad hoc formats are recognized for PAPER."
  (interactive "sarXiv paper: ")
  (let ((id (arXiv--id paper)))
    (url-retrieve
     (format "http://export.arxiv.org/api/query?id_list=%s" id)
     (lambda (_)
       (goto-char url-http-end-of-headers)
       (forward-line 2)
       (let* ((xml (libxml-parse-xml-region (point)))
              (entry (car (dom-by-tag xml 'entry)))
              (title (dom-text (dom-by-tag entry 'title)))
              (abstract (dom-text (dom-by-tag entry 'summary)))
              (authors (mapcar #'dom-texts (dom-by-tag entry 'author))))
         (with-output-to-temp-buffer (format "*arXiv:%s*" id)
           (princ
            (format "%s\nBy: %s\n\n%s\n"
                    title (string-join authors ", ") abstract))))))))

(provide 'arXiv)
