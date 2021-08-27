;; coordinates.el --- Insert coordinates of mouse click    -*- lexical-binding: t; -*-

(require 'pdf-util)

(defvar coordinates--offset '(0 . 0)
  "Offsets to add to coordinates before inserting.")

(defcustom coordinates-format
  "\\begin{textblock*}{<++>}(%.1fcm,%.1fcm) <++> \\end{textblock*}\n"
  "Format string for coordinate insertion."
  :type 'string
  :group 'coordinates)

(defun coordinates--get (prompt)
    "Return coordinates (in centimeters) of mouse click."
    (interactive)
    (let ((pos (event-start (pdf-util-read-click-event prompt))))
      (with-selected-window (posn-window pos)
        (let ((pt (pdf-util-scale-pixel-to-points (posn-object-x-y pos))))
          (cl-flet ((f (x) (* 2.54 (/ x 72.0))))
            (cons (f (car pt)) (f (cdr pt))))))))

(defun coordinates-calibrate ()
  "Set coordinates offset to difference between two mouse clicks."
  (interactive)
  (let ((pt1 (coordinates--get "Click on true location"))
        (pt2 (coordinates--get "Click on wrong location")))
    (setq coordinates--offset
          (cons (- (car pt1) (car pt2)) (- (cdr pt1) (cdr pt2))))))

(defun coordinates-insert ()
  "Insert coordinates (in centimeters) of mouse click."
  (interactive)
  (let ((pt (coordinates--get "Click on PDF")))
    (save-excursion
      (insert (format coordinates-format
                      (+ (car pt) (car coordinates--offset))
                      (+ (cdr pt) (cdr coordinates--offset)))))))

(provide 'coordinates)
