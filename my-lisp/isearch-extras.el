;;; -*- lexical-binding: t; -*-

(defun isearch-exit-at-start ()
  "Exit search at the beginning of the current match."
  (interactive)
  (isearch-exit)
  (goto-char (min (point) isearch-other-end)))

(defun isearch-save-and-exit ()
  "Exit search and save text from initial location to beginning of
current match in the kill ring."
  (interactive)
  (isearch-exit-at-start)
  (kill-ring-save (mark) (point)))

(defun isearch-kill-and-exit ()
  "Exit search and kill from initial location to beginning of current match."
  (interactive)
  (isearch-exit-at-start)
  (kill-region (mark) (point)))

(provide 'isearch-extras)
