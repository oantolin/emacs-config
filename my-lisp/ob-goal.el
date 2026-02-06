;; ob-goal --- Babel support for Goal  -*- lexical-binding: t; -*-

(define-derived-mode goal-mode prog-mode "Goal")

(defun ob-goal-value (value)
  "Return a string of Goal code evaluating to the given Lisp VALUE."
  (cond
   ((numberp value) (number-to-string value))
   ((stringp value) (format "%S" value))
   ((listp value) (format "(%s)" (mapconcat #'ob-goal-value value ";")))))

(defun org-babel-execute:goal (body params)
  (let ((vars (mapconcat (lambda (param)
                           (pcase param
                             (`(:var . (,var . ,val))
                              (format "%s:%s\n" var (ob-goal-value val)))
                             (_ "")))
                         params))
        (value (eq (alist-get :result-type params) 'value)))
    (with-temp-buffer
      (insert (format (if value
                          "say {l:{\"(\"+(\" \"/x)+\")\"}
?[\"d\"=@x;l(o@!x;\"hline\"),o'+.x;(@x)=_@x;$x;l@o'x]}{%s%s}0"
                        "%s%s")
                      vars body))
      (shell-command-on-region (point-min) (point-max) "goal -q" nil t)
      (goto-char (point-min))
      (if value (read (current-buffer)) (buffer-string)))))

(provide 'ob-goal)
