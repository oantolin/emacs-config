;;; man-help.el --- Use man for help in shell command prompts    -*- lexical-binding: t; -*-


(defun use-man-for-local-help (fn &rest args)
  "Remap `display-local-help' to `man' in the minibuffer.
Intended as `:around' advice for `read-shell-command'."
  (minibuffer-with-setup-hook
      (lambda ()
        (use-local-map
         (define-keymap :parent (current-local-map)
           "<remap> <display-local-help>" #'man)))
    (apply fn args)))

(advice-add 'read-shell-command :around #'use-man-for-local-help)

(provide 'man-help)
