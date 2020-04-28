;;; -*- lexical-binding: t; -*-

(defun update-completions (_start _end _length)
  (minibuffer-completion-help))

(defun live-completions-setup ()
  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions #'update-completions)
  (minibuffer-completion-help))

(define-minor-mode live-completions-mode
  "Live updating of the *Completions* buffer."
  :global t
  (if live-completions-mode
      (add-hook 'minibuffer-setup-hook #'live-completions-setup)
    (remove-hook 'minibuffer-setup-hook #'live-completions-setup)
    (dolist ((buffer (buffer-list)))
      (when (minibufferp buffer)
        (with-current-buffer buffer
          (setq after-change-functions
                (remove #'update-completions after-change-functions)))))))

(provide 'live-completions)
