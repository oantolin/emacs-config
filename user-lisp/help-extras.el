;;; help-extras.el --- Miscellaneous help-related commands    -*- lexical-binding: t; -*-

(defvar help-xref-stack-item)

;;;###autoload
(defun command-of-the-day ()
  "Show the documentation for a random command."
  (interactive)
  (let (commands)
    (mapatoms (lambda (s) (when (commandp s) (push s commands))))
    (describe-function (nth (random (length commands)) commands))
    (with-current-buffer "*Help*"
      (setq help-xref-stack-item '(command-of-the-day)))))

;;;###autoload
(defalias 'cotd #'command-of-the-day)

;;;###autoload
(defun show-help ()
  "Show the *Help* buffer."
  (interactive)
  (if help-window-select
      (pop-to-buffer "*Help*")
    (display-buffer "*Help*")))

(provide 'help-extras)
