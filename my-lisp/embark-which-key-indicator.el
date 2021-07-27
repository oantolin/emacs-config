;; -*- lexical-binding: t; -*-

(require 'which-key)

(defun embark-which-key-indicator (keymap targets)
  "An embark indicator that displays KEYMAP with which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
TARGETS."
  (which-key--show-keymap
   (if (eq (caar targets) 'embark-become)
       "Become"
     (format "Act on %s '%s'%s"
             (caar targets)
             (embark--truncate-target (cdar targets))
             (if (cdr targets) "…" "")))
   keymap
   nil nil t)
  (lambda (prefix)
    (if prefix
        (embark-which-key-indicator (lookup-key keymap prefix) targets)
      (kill-buffer which-key--buffer))))

(provide 'embark-which-key-indicator)
