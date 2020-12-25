;;; -*- lexical-binding: t; -*-

(defun among (letters)
  (interactive "sAmong: ")
  (save-excursion
    (goto-char (point-min))
    (with-output-to-temp-buffer "*among*"
      (while (search-forward-regexp (format "^[%s]+$" letters) nil t)
        (let ((word (buffer-substring-no-properties
                     (save-excursion
                       (beginning-of-line)
                       (point))
                     (point))))
          (when (cl-loop for c across word
                         always (<= (cl-count c word)
                                    (cl-count c letters)))
            (princ (format "%2d %s\n" (length word) word)))))))
  (with-current-buffer "*among*"
    (let ((inhibit-read-only t))
      (sort-numeric-fields 1 (point-min) (point-max))
      (reverse-region (point-min) (point-max))
      (set-buffer-modified-p nil)))
  (pop-to-buffer "*among*"))

(provide 'among)
