;;; ffap-eshell.el --- Extra rules for ffap    -*- lexical-binding: t; -*-

;;; Improved directory guessing in eshell buffers:

(require 'ffap)

(defun ffap-eshell-guess-directory-from-face ()
  (save-excursion
    (backward-paragraph)
    (forward-char)
    (when (eq (get-text-property (point) 'face) 'eshell-ls-directory)
      (buffer-substring-no-properties
       (point) (next-single-property-change (point) 'face)))))

(declare-function eshell-previous-prompt "em-prompt")
 
(defun ffap-eshell-guess-directory-from-prompt ()
  (save-excursion
    (eshell-previous-prompt 1)
    (end-of-line)
    (thing-at-point 'filename)))

(defun ffap-eshell-mode (name)
  (seq-find
   #'file-exists-p
   (mapcar (lambda (dir) (expand-file-name name dir))
           (delq nil (list default-directory
                           (ffap-eshell-guess-directory-from-face)
                           (ffap-eshell-guess-directory-from-prompt))))))


(setf (alist-get 'eshell-mode ffap-alist) #'ffap-eshell-mode)

(provide 'ffap-eshell)
