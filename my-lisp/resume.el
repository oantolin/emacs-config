;; -*- lexical-binding: t; -*-

(defvar resume--command nil)
(defvar resume--input nil)

(defun resume--update-input ()
  (setq resume--input (minibuffer-contents)))

(defun resume--save ()
  (setq resume--command this-command)
  (add-hook 'post-command-hook #'resume--update-input nil t))

(defun resume--load ()
  (delete-minibuffer-contents)
  (insert resume--input))

(defun resume ()
  "Resume last completion command."
  (interactive)
  (if (null resume--command)
      (user-error "No command to resume")
    (setq this-command resume--command)
    (minibuffer-with-setup-hook #'resume--load
      (command-execute resume--command))))

(define-minor-mode resume-minor-mode
  "Global minor mode enabling the `resume' command."
  :global t
  (if resume-minor-mode
      (add-hook 'minibuffer-setup-hook #'resume--save)
    (remove-hook 'minibuffer-setup-hook #'resume--save)))

(provide 'resume)
