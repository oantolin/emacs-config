;;; -*- lexical-binding: t; -*-

(defun open-externally (x)
  "Open file using system's default application."
  (interactive "fOpen: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" x)
    (start-process-shell-command
     "xdg-open" nil
     (format "setsid -w xdg-open %s"
             (shell-quote-argument
              (expand-file-name x))))))

(defun dired-open-externally (&optional arg)
  "Open marked or current line's file in operating system's default application."
  (interactive "P")
  (dired-map-over-marks
   (open-externally (dired-get-filename))
   arg))

(provide 'open-externally)
