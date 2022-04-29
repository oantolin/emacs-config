;;; ffap-eshell.el --- Extra rules for ffap    -*- lexical-binding: t; -*-

;;; Improved directory guessing in eshell buffers:

(require 'ffap)
(require 'eshell)

(defun guess-directory-from-face ()
  (save-excursion
    (backward-paragraph)
    (forward-char)
    (when (eq (get-text-property (point) 'face) 'eshell-ls-directory)
      (buffer-substring-no-properties
       (point) (next-single-property-change (point) 'face)))))

(defun guess-directory-from-prompt ()
  (save-excursion
    (eshell-previous-prompt 1)
    (end-of-line)
    (thing-at-point 'filename)))

(defun ffap-eshell-mode (name)
  (seq-find #'file-exists-p
            (mapcar (lambda (dir) (expand-file-name name dir))
                    (delq nil (list default-directory
                                    (guess-directory-from-face)
                                    (guess-directory-from-prompt))))))

(setf (alist-get 'eshell-mode ffap-alist) #'ffap-eshell-mode)

(provide 'ffap-eshell)
