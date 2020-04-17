;;; -*- lexical-binding: t; -*-

(require 'json)

(defun vimgolf ()
  "Setup buffers for vimgolf challenge."
  (interactive)
  (goto-char (point-min))
  (let ((json (json-read)))
    (switch-to-buffer "*VimGolf B*")
    (insert (cdr (assoc 'data (cdr (assoc 'out json)))))
    (goto-char (point-min))    
    (split-window-horizontally)
    (switch-to-buffer "*VimGolf A*")
    (insert (cdr (assoc 'data (cdr (assoc 'in json)))))
    (goto-char (point-min))))

(provide 'vimgolf)
