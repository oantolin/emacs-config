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

(provide 'isearch-extras)
