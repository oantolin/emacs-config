;;; embark-kmacro.el --- Embark support for Hyperbole key series    -*- lexical-binding: t; -*-

(require 'embark)

(autoload 'kmacro-push-ring "kmacro")
(autoload 'kmacro-name-last-macro "kmacro")
(autoload 'kmacro-bind-to-key "kmacro")

(defun embark-kmacro-target ()
  "Target a textual kmacro in braces."
  (save-excursion
    (let ((beg (progn (skip-chars-backward "^{}\n") (point)))
          (end (progn (skip-chars-forward "^{}\n") (point))))
      (when (and (eq (char-before beg) ?{) (eq (char-after end) ?}))
        `(kmacro ,(buffer-substring-no-properties beg end)
                 . (,(1- beg) . ,(1+ end)))))))

(add-to-list 'embark-target-finders 'embark-kmacro-target)

(defun embark-kmacro-run (arg kmacro)
  (interactive "p\nsKmacro: ")
  (kmacro-call-macro arg t nil (kbd kmacro)))

(defun embark-kmacro-save (kmacro)
  (interactive "sKmacro: ")
  (kmacro-push-ring)
  (setq last-kbd-macro (kbd kmacro)))

(defun embark-kmacro-name (kmacro name)
  (interactive "sKmacro: \nSName: ")
  (let ((last-kbd-macro (kbd kmacro)))
    (kmacro-name-last-macro name)))

(defun embark-kmacro-bind (kmacro)
  (interactive "sKmacro: \n")
  (let ((last-kbd-macro (kbd kmacro)))
    (kmacro-bind-to-key nil)))

(defvar-keymap embark-kmacro-map
  :doc "Actions on kmacros."
  "RET" #'embark-kmacro-run
  "s" #'embark-kmacro-save
  "n" #'embark-kmacro-name
  "b" #'embark-kmacro-bind)

(let ((file-tail (cl-member 'file embark-keymap-alist :key #'car)))
  (setcdr file-tail (cons '(kmacro . embark-kmacro-map) (cdr file-tail))))

(provide 'embark-kmacro)
