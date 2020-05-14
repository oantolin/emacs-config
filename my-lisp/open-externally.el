;;; -*- lexical-binding: t; -*-

(defun open-externally (x)
  "Open file using system's default application."
  (interactive "fOpen: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" x)
    (call-process "xdg-open" nil 0 nil x)))

(defun dired-open-externally (&optional arg)
  "Open marked or current file in operating system's default application."
  (interactive "P")
  (dired-map-over-marks
   (open-externally (dired-get-filename))
   arg))

(provide 'open-externally)
