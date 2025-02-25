;;; vc-extras --- Miscellaneous commands to use with VC   -*- lexical-binding: t; -*-

(require 'log-view)
(require 'log-edit)
(require 'vc-git)

(defun clear-log-edit-buffer (&optional _)
  "Clear the buffer if it is in `log-edit-mode'.
Intended to be used as advice for `consult-history'."
  (when (derived-mode-p 'log-edit-mode)
    (erase-buffer)))

(defun log-view-save-commit-hash ()
  "Save commit hash of log entry at point to `kill-ring'."
  ;; This is Protesilaos' prot-vc-log-kill-hash function
  (interactive)
  (let ((commit (cadr (log-view-current-entry (point) t))))
    (kill-new (format "%s" commit))
    (message "Copied: %s" commit)))

(provide 'vc-extras)
