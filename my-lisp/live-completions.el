;;; -*- lexical-binding: t; -*-

(defun live-completions--update (&optional _start _end _length)
  (while-no-input (save-match-data (minibuffer-completion-help))))

(defun live-completions--setup ()
  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions #'live-completions--update)
  (live-completions--update))

(define-minor-mode live-completions-mode
  "Live updating of the *Completions* buffer."
  :global t
  (if live-completions-mode
      (add-hook 'minibuffer-setup-hook #'live-completions--setup)
    (remove-hook 'minibuffer-setup-hook #'live-completions--setup)
    (dolist (buffer (buffer-list))
      (when (minibufferp buffer)
        (with-current-buffer buffer
          (setq after-change-functions
                (remove #'update-completions after-change-functions)))))))

(provide 'live-completions)
