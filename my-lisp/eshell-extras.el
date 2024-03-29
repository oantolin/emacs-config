;;  eshell-extras.el --- Miscellaneous eshell commands   -*- lexical-binding: t; -*-

(defun interactive-cd (dir)
  "Prompt for a directory and cd to it."
  (interactive "Dcd ")
  (let ((inhibit-read-only t))
    (insert (concat "cd " (shell-quote-argument dir))))
  (pcase major-mode
    ('shell-mode (comint-send-input))
    ('eshell-mode (eshell-send-input))
    ('term-mode (term-send-input))))

(defun eshell/for-each (cmd &rest args)
  "Run command once for each argument."
  (let ((fn (intern cmd))
        (dir default-directory))
    (dolist (arg (flatten-tree args))
      (let ((default-directory dir))
        (funcall fn arg)))))

(defun eshell/in-term (prog &rest args)
  "Run shell command in term buffer."
  (switch-to-buffer (apply #'make-term prog prog nil args))
  (term-mode)
  (term-char-mode))

;; The built-in version of the following function doesn't ensure you
;; get an eshell buffer in the right directory!
(defun eshell-bookmark-jump (bookmark)
  "Default bookmark handler for Eshell buffers."
  (let ((default-directory (bookmark-prop-get bookmark 'location)))
    (eshell '(4))))

(provide 'eshell-extras)
