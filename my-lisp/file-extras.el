;; -*- lexical-binding: t; -*-

(defun rename-visiting-buffer (file newname &optional _ok-if-already-exists)
  "Rename buffer visiting FILE to NEWNAME.
Intended as :after advice for `rename-file'."
  (when (called-interactively-p 'any)
    (when-let ((buffer (get-file-buffer file)))
      (with-current-buffer buffer
        (set-visited-file-name newname nil t)))))

(advice-add 'rename-file :after 'rename-visiting-buffer)

(defun kill-visiting-buffer (file &optional _trash)
  "Kill buffer visiting FILE.
Intended as :after advice for `delete-file'."
  (when (called-interactively-p 'any)
    (when-let ((buffer (get-file-buffer file)))
      (kill-buffer buffer))))

(advice-add 'delete-file :after 'kill-visiting-buffer)
  
(provide 'file-extras)
