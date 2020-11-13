;;; -*- lexical-binding: t; -*-

(defun copy-org-link-at-point ()
  "Save the link at point in the kill ring."
  (interactive)
  (when (org-in-regexp org-link-bracket-re 1)
    (kill-ring-save (match-beginning 0) (match-end 0))))

(defun insert-list-of-stored-links ()
  "Insert ‘org-stored-links’ as a bulleted list."
  (interactive)
  (unless (= (point) (line-beginning-position))
    (newline))
  (dolist (link org-stored-links)
    (insert (apply #'format "- [[%s][%s]]\n" link))))

(provide 'org-extras)
