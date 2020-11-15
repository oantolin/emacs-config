;;; -*- lexical-binding: t; -*-

(defun copy-org-link-at-point ()
  "Save the link at point in the kill ring."
  (interactive)
  (when (org-in-regexp org-link-bracket-re 1)
    (kill-ring-save (match-beginning 0) (match-end 0))))

(provide 'org-extras)
