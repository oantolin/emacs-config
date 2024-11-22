;;; window-extras.el --- Miscellaneous window commands    -*- lexical-binding: t; -*-

(defun toggle-window-split ()
  "Toggle window split from vertical to horizontal."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows.")
    (let ((was-full-height (window-full-height-p)))
      (delete-other-windows)
      (if was-full-height
          (split-window-vertically)
        (split-window-horizontally))
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun transpose-windows ()
  "Swap the buffers shown in current and next window."
  (interactive)
  (let ((this-buffer (window-buffer))
        (next-window (next-window nil :no-minibuf nil)))
    (set-window-buffer nil (window-buffer next-window))
    (set-window-buffer next-window this-buffer)
    (select-window next-window)))

;; Modified slightly from alphapapa's original at:
;; https://www.reddit.com/r/emacs/comments/idz35e/emacs_27_can_take_svg_screenshots_of_itself/g2c2c6y
(defun screenshot (arg)
  "Save a screenshot of the current frame to ~/Downloads.
By default save a PNG, but with a prefix argument, save a an SVG."
  (interactive "P")
  (let* ((type (if arg 'svg 'png))
         (filename (make-temp-file
                    "~/Downloads/Emacs" nil (format ".%s" type)
                    (x-export-frames nil type))))
    (kill-new filename)
    (message "Saved %s" filename)))

(defun toggle-mode-line ()
  "Toggle visibility of the mode line."
  (interactive)
  (if mode-line-format
      (setq-local mode-line-format nil)
    (kill-local-variable 'mode-line-format)
    (force-mode-line-update)))

(provide 'window-extras)
