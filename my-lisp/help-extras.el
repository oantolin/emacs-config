;;; -*- lexical-binding: t; -*-

(defun describe-keymap (keymap)
  "Describe keys bound in KEYMAP.
Starting from Emacs 28, there is a built-in function for this."
  (interactive
   (list
    (completing-read "Keymap: "
     (cl-loop for x being the symbols
              if (and (boundp x) (keymapp (symbol-value x)))
              collect (symbol-name x))
     nil t nil 'variable-name-history)))
  (help-setup-xref (list #'describe-keymap keymap)
                   (called-interactively-p 'interactive))
  (with-output-to-temp-buffer (help-buffer)
    (princ keymap) (terpri) (terpri)
    (let ((doc (documentation-property
                (intern keymap)
                'variable-documentation)))
      (when doc (princ doc) (terpri) (terpri)))
    (princ (substitute-command-keys (format "\\{%s}" keymap)))))

(defun command-of-the-day ()
  "Show the documentation for a random command."
  (interactive)
  (with-output-to-temp-buffer "*Command of the day*"
    (setq-local revert-buffer-function #'command-of-the-day)
    (let* ((commands (cl-loop for s being the symbols
                              when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ (describe-function command)))))

(defun show-help ()
  "Show the *Help* buffer."
  (interactive)
  (display-buffer "*Help*"))

(defalias 'cotd #'command-of-the-day)

(provide 'help-extras)
