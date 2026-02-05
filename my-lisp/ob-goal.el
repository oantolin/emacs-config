;; ob-goal --- Babel support for Goal  -*- lexical-binding: t; -*-

(require 'org-babel)

(define-derived-mode goal-mode prog-mode "Goal")

(defun org-babel-execute:goal (body params)
  (with-temp-buffer
    (insert (format "say {%s} 0" body))
    (shell-command-on-region (point-min) (point-max) "goal -q" nil t)
    (buffer-string)))

(provide 'ob-goal)
