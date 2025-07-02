;;; apt-progress.el --- Render apt progress bars in eshell buffers  -*- lexical-binding: t; -*-

(require 'ansi-color)

(define-advice ansi-color-apply-on-region
    (:before (begin end &optional _) fix-apt-progress)
  (let ((end (copy-marker end)))
    (save-excursion
      (goto-char begin)
      (while (search-forward-regexp "\^[7\\(.*?\\)\^[8" end t)
        (let ((text (match-string 1)))
          (replace-match "")
          (when (string-match "Progress: \\[ *[0-9]+%\\]" text)
            (message "%s" (match-string 0 text))))))))
  
(provide 'apt-progress)

