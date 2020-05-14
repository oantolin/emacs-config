;;; -*- lexical-binding: t; -*-

(defun isearch-exit-at-start ()
  "Exit search at the beginning of the current match."
  (interactive)
  (isearch-exit)
  (when isearch-forward (goto-char isearch-other-end)))

(defun isearch-exit-at-end ()
  "Exit search at the end of the current match."
  (interactive)
  (isearch-exit)
  (unless isearch-forward (goto-char isearch-other-end)))

(defun isearch-delete-wrong ()
  "Revert to previous successful search."
  (interactive)
  (if (string= isearch-string "")
      (ding)
    (isearch-pop-state)
    (while (or (not isearch-success) isearch-error)
      (isearch-pop-state)))
  (isearch-update))

(provide 'isearch-extras)
