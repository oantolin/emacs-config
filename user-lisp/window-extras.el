;;; window-extras.el --- Miscellaneous window commands    -*- lexical-binding: t; -*-

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
