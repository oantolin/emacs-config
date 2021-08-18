;;;  visiting-buffer.el --- When deleting or renaming files deal with buffer too   -*- lexical-binding: t; -*-

(defun visiting-buffer-rename (file newname &optional _ok-if-already-exists)
  "Rename buffer visiting FILE to NEWNAME.
Intended as :after advice for `rename-file'."
  (when (called-interactively-p 'any)
    (when-let ((buffer (get-file-buffer file)))
      (with-current-buffer buffer
        (set-visited-file-name newname nil t)
        (when (derived-mode-p 'emacs-lisp-mode)
          (save-excursion
            (let* ((base (file-name-nondirectory file))
                   (sans (file-name-sans-extension base))
                   (newbase (file-name-nondirectory newname))
                   (newsans (file-name-sans-extension newbase)))
              (goto-char (point-min))
              (while (search-forward-regexp (format "^;;; %s" base) nil t)
                (replace-match (concat ";;; " newbase)))
              (goto-char (point-max))
              (when
                  (search-backward-regexp (format "^(provide '%s)" sans) nil t)
                (replace-match (format "(provide '%s)" newsans))))))))))

(advice-add 'rename-file :after 'visiting-buffer-rename)

(defun visiting-buffer-kill (file &optional _trash)
  "Kill buffer visiting FILE.
Intended as :after advice for `delete-file'."
  (when (called-interactively-p 'any)
    (when-let ((buffer (get-file-buffer file)))
      (kill-buffer buffer))))

(advice-add 'delete-file :after 'visiting-buffer-kill)
  
(provide 'visiting-buffer)
