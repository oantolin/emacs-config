;;; apt-progress.el --- Render apt progress bars in eshell buffers  -*- lexical-binding: t; -*-

(require 'ansi-color)

(defun apt-progress-fix (begin end &optional _)
  (let ((end (copy-marker end)))
    (save-excursion
      (goto-char begin)
      (while (search-forward-regexp "\^[7\\(.*?\\)\^[8" end t)
        (let ((text (match-string 1)))
          (replace-match "")
          (when (string-match "Progress: \\[ *[0-9]+%\\]" text)
            (message "%s" (match-string 0 text))))))))

(advice-add 'ansi-color-apply-on-region :before 'apt-progress-fix)
  
(provide 'apt-progress)

