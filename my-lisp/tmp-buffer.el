;;; -*- lexical-binding: t; -*-

(defcustom tmp-buffer-mode-alist
  '((?o . org-mode)
    (?t . text-mode)
    (?m . markdown-mode)
    (?l . lisp-interaction-mode)
    (?x . LaTeX-mode)
    (?f . fundamental-mode)
    (?? . tmp-buffer-prompt-for-mode))
  "List of major modes for temporary buffers and their hotkeys."
  :type '(alist :key-type character :value-type symbol)
  :group 'tmp-buffer)

(defvar tmp-buffer-mode-history nil)

(defun tmp-buffer-prompt-for-mode ()
  "Prompt for a major mode and switch to it."
  (funcall
   (intern
    (completing-read
     "Mode: "
     (cl-loop for m being the symbols
              when (and (commandp m) (string-suffix-p "-mode" (symbol-name m)))
              collect m)
     nil t nil 'tmp-buffer-mode-history))))

(defun tmp-buffer (spec)
  "Open temporary buffer in specified major mode."
  (interactive "c")
  (if (eq spec ?\C-h)
      (progn
        (help-setup-xref (list #'tmp-buffer ?\C-h)
                         (called-interactively-p 'interactive))
        (with-output-to-temp-buffer (help-buffer)
          (princ "Temporary buffers:\n\nKey\tMode\n")
          (dolist (km tmp-buffer-mode-alist)
            (princ (format " %c\t%s\n" (car km) (cdr km))))))
    (let ((mode (cdr (assoc spec tmp-buffer-mode-alist))))
      (if (not mode)
          (user-error "Unknown mode for temporary buffer.")
        (pop-to-buffer (generate-new-buffer "*tmp*"))
        (funcall mode)))))

(provide 'tmp-buffer)
