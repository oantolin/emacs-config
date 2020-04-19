;; -*- lexical-binding: t; -*-

(defun interactive-cd (dir)
  "Prompt for a directory and cd to it."
  (interactive "Dcd ")
  (let ((inhibit-read-only t))
    (insert (concat "cd " dir)))
  (pcase major-mode
    ('shell-mode (comint-send-input))
    ('eshell-mode (eshell-send-input))
    ('term-mode (term-send-input))))

(defun eshell/for-each (cmd &rest args)
  "Run command once for each argument."
  (let ((fn (intern cmd))
        (dir default-directory))
    (dolist (arg (eshell-flatten-list args))
      (let ((default-directory dir))
        (funcall fn arg)))))

(defun eshell/in-term (prog &rest args)
  "Run shell command in term buffer."
  (switch-to-buffer (apply #'make-term prog prog nil args))
  (term-mode)
  (term-char-mode))

(provide 'eshell-extras)
