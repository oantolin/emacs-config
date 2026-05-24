;;; vc-extras --- Miscellaneous commands to use with VC   -*- lexical-binding: t; -*-

(require 'log-view)
(require 'log-edit)
(require 'vc-git)

;;;###autoload
(defun vc-git-commit (message)
  "Run git commit -m MESSAGE.
Interactively MESSAGE is just \"Merge resolving conflicts\", but
with a prefix argument you are prompted for a message."
  (interactive
   (list (if current-prefix-arg
             (read-from-minibuffer "Commit message: " "Merge")
           "Merge resolving conflicts")))
  (vc-git-command nil 0 nil "commit" "-m" message)
  (message "Commited with message: %s" message)
  (when (derived-mode-p 'vc-dir-mode)
    (revert-buffer)))

(provide 'vc-extras)
