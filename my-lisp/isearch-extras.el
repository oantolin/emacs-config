;;; isearch-extras.el --- Some isearch tweaks    -*- lexical-binding: t; -*-

(defun isearch-exit-at-start ()
  "Exit search at the beginning of the current match."
  (unless (or isearch-mode-end-hook-quit
              (bound-and-true-p isearch-suspended)
              (not isearch-forward)
              (not isearch-other-end)
              (and (boundp 'avy-command)
                   (eq avy-command 'avy-isearch)))
    (goto-char isearch-other-end)))

(defun isearch-exit-at-end ()
  "Exit search at the end of the current match."
  (interactive)
  (let ((isearch-other-end (point)))
    (isearch-exit))
  (unless isearch-forward (goto-char isearch-other-end)))

(defun isearch-delete-wrong ()
  "Revert to previous successful search."
  (interactive)
  (while (or (not isearch-success) isearch-error)
    (isearch-pop-state))
  (isearch-update))

(defun isearch-yank-region ()
  "If the region is active add it to the isearch search string.
Either bind this to a key in `isearch-mode-map' or add it to
`isearch-mode-hook'."
  (interactive)
  (when (use-region-p)
    (isearch-yank-string
     (buffer-substring-no-properties
      (region-beginning) (region-end)))
    (deactivate-mark)))

(defun isearch-replace-regexp (&optional from-here)
  "Regexp isearch followed by query-replace.
A more interactive replacement for `query-replace-regexp': it
starts with a regexp isearch from the top of the buffer, which
allows you to verify visually you typed the correct regexp; upon
exiting the isearch runs a query-replace operation; and finally
restores point.

If called when the region is active, it first narrows to the
region.  When FROM-HERE is non-nil (interactively, if called with
a prefix argument), the search and replacements are done from
point until the end of the buffer."
  (interactive "P")
  (save-excursion
    (save-restriction
      (unless from-here (goto-char (point-min)))
      (when (use-region-p)
        (narrow-to-region (region-beginning) (region-end))
        (deactivate-mark))
      (let ((isearch-mode-end-hook
             (lambda ()
               (unless isearch-mode-end-hook-quit
                 (let (isearch-mode-end-hook)
                   (unless from-here (isearch-beginning-of-buffer))
                   (isearch-query-replace-regexp))))))
        (isearch-forward-regexp)))))

(defun isearch-next ()
  "Go to next isearch match."
  (interactive)
  (if (equal isearch-string "")
      (user-error "No previous search.")
    (let (isearch-lazy-highlight)
      (isearch-repeat 'forward))
    (isearch-exit)))

(defun isearch-previous ()
  "Go to previous isearch match."
  (interactive)
  (if (equal isearch-string "")
      (user-error "No previous search.")
    (let (isearch-lazy-highlight)
      (isearch-repeat 'backward))
    (isearch-exit)))

(provide 'isearch-extras)
