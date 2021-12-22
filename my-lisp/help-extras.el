;;; help-extras.el --- Miscellaneous help-related commands    -*- lexical-binding: t; -*-

(defun command-of-the-day ()
  "Show the documentation for a random command."
  (interactive)
  (let* ((commands (cl-loop for s being the symbols
                            when (commandp s) collect s))
         (command (nth (random (length commands)) commands)))
    (describe-function command)))

(defalias 'cotd #'command-of-the-day)

(defun show-help ()
  "Show the *Help* buffer."
  (interactive)
  (display-buffer "*Help*"))

(provide 'help-extras)
