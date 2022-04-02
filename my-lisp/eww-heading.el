;;; eww-heading.el --- Navigation by heading   -*- lexical-binding: t; -*-

(defun eww-heading-next (&optional arg)
  "Move forward by ARG headings (any h1-h4).
If ARG is negative move backwards, ARG defaults to 1."
  (interactive "p")
  (unless arg (setq arg 1))
  (catch 'return
    (cl-flet ((headingp ()
                (when-let ((face (get-text-property (point) 'face)))
                  (and (consp face)
                       (cl-intersection face '(shr-h1 shr-h2 shr-h3 shr-h4))))))
      (dotimes (_ (abs arg))
        (when (headingp)
          (unless (zerop (forward-line (if (> arg 0) 1 -1)))
            (throw 'return nil)))
        (while (not (headingp))
          (if-let ((find-change (if (> arg 0)
                                    #'next-single-property-change
                                  #'previous-single-property-change))
                   (change (funcall find-change (point) 'face)))
              (goto-char change)
            (goto-char (if (> arg 0) (point-max) (point-min)))
            (throw 'return nil))))
      (beginning-of-line)
      (point))))

(defun eww-heading-previous (&optional arg)
  "Move backward by ARG headings (any h1-h4).
If ARG is negative move forwards instead, ARG defaults to 1."
  (interactive "p")
  (eww-next-heading (- (or arg 1))))

(defun eww-heading--line-at-point ()
  "Return the current line."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun eww-heading-setup-imenu ()
  "Setup imenu for h1-h4 headings in eww buffer.
Add this function to `eww-mode-hook'."
  (setq-local
   imenu-prev-index-position-function #'eww-previous-heading
   imenu-extract-index-name-function  #'line-at-point))

(provide 'eww-heading)
