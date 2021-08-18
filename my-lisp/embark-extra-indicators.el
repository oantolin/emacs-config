;; -*- lexical-binding: t; -*-

(defvar which-key--buffer)
(declare-function which-key--show-keymap "ext:which-key")

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (require 'which-key)
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

(defun embark-minibuffer-indicator ()
  (let ((indicator-overlay))
    (lambda (&optional keymap targets _prefix)
      (if (null keymap)
          (when indicator-overlay
            (delete-overlay indicator-overlay))
        (when (minibufferp)
          (unless indicator-overlay
            (setq indicator-overlay (make-overlay
                                     (minibuffer-prompt-end) (point-max)))
            (overlay-put indicator-overlay 'face 'highlight))
          (overlay-put indicator-overlay
                       'display (plist-get (car targets) :target)))))))

(provide 'embark-extra-indicators)
