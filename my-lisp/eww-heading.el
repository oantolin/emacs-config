;;; eww-heading.el --- Navigation by heading   -*- lexical-binding: t; -*-

(defun eww-heading-next (&optional arg)
  "Move forward by ARG headings (any h1-h4).
If ARG is negative move backwards, ARG defaults to 1."
  (interactive "p")
  (unless arg (setq arg 1))
  (catch 'return
    (dotimes (_ (abs arg))
      (when (> arg 0) (end-of-line))
      (unless (funcall
               (if (> arg 0)
                   #'text-property-search-forward
                 #'text-property-search-backward)
               'face '(shr-h1 shr-h2 shr-h3 shr-h4)
               (lambda (tags face)
                 (cl-loop for x in (ensure-list face) thereis (memq x tags))))
        (throw 'return nil))
      (when (< arg 0) (beginning-of-line)))
    (beginning-of-line)
    (point)))

(defun eww-heading-previous (&optional arg)
  "Move backward by ARG headings (any h1-h4).
If ARG is negative move forwards instead, ARG defaults to 1."
  (interactive "p")
  (eww-heading-next (- (or arg 1))))

(defun eww-heading--line-at-point ()
  "Return the current line."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun eww-heading-setup-imenu ()
  "Setup imenu for h1-h4 headings in eww buffer.
Add this function to `eww-mode-hook'."
  (setq-local
   imenu-prev-index-position-function #'eww-heading-previous
   imenu-extract-index-name-function  #'eww-heading--line-at-point))

(provide 'eww-heading)
