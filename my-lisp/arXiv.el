;; arXiv.el --- Misc commands for arXiv -*- lexical-binding: t; -*-

(defun arXiv-pdf (paper &optional generic)
  "Open the PDF version of PAPER from the arXiv.
A ragtag bunch of ad hoc formats are recognized for PAPER.
If GENERIC is non-nil use `url-browse-generic' rather than
`browse-url'."
  (interactive "sarXiv paper: \nP")
  (when (and (not generic) (fboundp 'pdf-tools-install))
    (pdf-tools-install))
  (setq paper (replace-regexp-in-string "_" "." paper))
  (dolist (pattern
           '("^%s$"
             "^arXiv:%s\\(?:/.*\\)?$"
             "^https?://arxiv.org/abs/%s$" 
             "^https://mathstodon.xyz/tags/arXiv.%s$"))
    (setq paper (replace-regexp-in-string
                 (format pattern "\\([0-9v.]+\\)")
                 "https://arxiv.org/pdf/\\1.pdf"
                 paper)))
  (if generic
      (browse-url-generic paper)
    (browse-url paper)))

(provide 'arXiv)
