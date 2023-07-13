;;  embark-extra-indicators.el --- Which-key and minibuffer embark indicators   -*- lexical-binding: t; -*-

(defvar which-key--buffer)
(declare-function which-key--show-keymap "ext:which-key")

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(defun embark-minibuffer-indicator ()
  "An embark indicator for the minibuffer that shows the target in an overlay."
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
