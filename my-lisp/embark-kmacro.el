;;; -*- lexical-binding: t; -*-

(defun embark-kmacro-target ()
  "Target a textual kmacro in braces."
  (save-excursion
    (let ((beg (progn (skip-chars-backward "^{}\n") (point)))
          (end (progn (skip-chars-forward "^{}\n") (point))))
      (when (and (eq (char-before beg) ?{) (eq (char-after end) ?}))
        (cons 'kmacro (buffer-substring-no-properties beg end))))))

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

(embark-define-keymap embark-kmacro-map
  "Actions on kmacros."
  ("RET" embark-kmacro-run)
  ("s" embark-kmacro-save)
  ("n" embark-kmacro-name)
  ("b" embark-kmacro-bind))

(add-to-list 'embark-keymap-alist '(kmacro . embark-kmacro-map))

(provide 'embark-kmacro)
