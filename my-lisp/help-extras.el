;;; help-extras.el --- Miscellaneous help-related commands    -*- lexical-binding: t; -*-

(defun embark-describe-keymap (keymap)
  "Prompt for KEYMAP and show its bindings using `completing-read'."
  (interactive
   (list
    (completing-read "Keymap: "
     (cl-loop for x being the symbols
              if (and (boundp x) (keymapp (symbol-value x)))
              collect (symbol-name x))
     nil t nil 'variable-name-history)))
  (require 'embark)
  (embark-completing-read-prompter (symbol-value (intern keymap)) nil))

(defun command-of-the-day ()
  "Show the documentation for a random command."
  (interactive)
  (let* ((commands (cl-loop for s being the symbols
                            when (commandp s) collect s))
         (command (nth (random (length commands)) commands)))
    (describe-function command)))

(defun show-help ()
  "Show the *Help* buffer."
  (interactive)
  (display-buffer "*Help*"))

(defalias 'cotd #'command-of-the-day)

(provide 'help-extras)
