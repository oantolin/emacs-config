;; -*- lexical-binding: t; -*-

;; to use run: emacs --batch -l path/to/eshell-repl.el

(require 'em-prompt)

(let ((dir default-directory))
  (add-hook 'eshell-directory-change-hook
            (lambda () (setq dir default-directory)))
  (while t
    (let* ((default-directory dir)
           (command (read-from-minibuffer (funcall eshell-prompt-function))))
      (princ (eshell-command-result command)))))
