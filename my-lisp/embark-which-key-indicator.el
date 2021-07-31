;; -*- lexical-binding: t; -*-

(require 'which-key)

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (kill-buffer which-key--buffer)
      (which-key--show-keymap
       (if (eq (caar targets) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (caar targets)
                 (embark--truncate-target (cdar targets))
                 (if (cdr targets) "…" "")))
       (if prefix (lookup-key keymap prefix) keymap)
       nil nil t))))

(provide 'embark-which-key-indicator)
