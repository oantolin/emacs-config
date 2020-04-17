;;; -*- lexical-binding: t; -*-

(defcustom placeholder-string "<++>"
  "Placeholder string. Pick a string unlikely to appear in your buffers."
  :type 'string
  :group 'placeholder)

(defun placeholder-insert ()
  "Insert the placeholder-string in the current buffer."
  (interactive)
  (insert placeholder-string))

(defun placeholder-forward (count)
  "Delete the next COUNTth occurrence of the placeholder string,
leaving point there ready for insertion. If called again
immediately after, it will restore that occurence of the
placeholder and move to the next."
  (interactive "p")
  (let ((n (length placeholder-string )))
    (when (eq last-command 'placeholder)
      (insert placeholder-string)
      (when (< count 0) (backward-char n)))
    (search-forward placeholder-string nil nil count)
    (delete-char (if (> count 0) (- n) n))
    (setq this-command 'placeholder)))

(defun placeholder-backward (count)
  "Delete the previous COUNTth occurrence of the placeholder string,
leaving point there ready for insertion. If called again
immediately after, it will restore that occurence of the
placeholder and move to the previous."
  (interactive "p")
  (placeholder-forward (- count)))

(provide 'placeholder)
