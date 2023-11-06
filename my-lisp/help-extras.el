;;; help-extras.el --- Miscellaneous help-related commands    -*- lexical-binding: t; -*-

(defun command-of-the-day ()
  "Show the documentation for a random command."
  (interactive)
  (let (commands)
    (mapatoms (lambda (s) (when (commandp s) (push s commands))))
    (describe-function (nth (random (length commands)) commands))
    (with-current-buffer "*Help*"
      (setq help-xref-stack-item '(command-of-the-day)))))

(defalias 'cotd #'command-of-the-day)

(defun show-help ()
  "Show the *Help* buffer."
  (interactive)
  (display-buffer "*Help*"))

(provide 'help-extras)
