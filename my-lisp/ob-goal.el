;; ob-goal --- Babel support for Goal  -*- lexical-binding: t; -*-

(define-derived-mode goal-mode prog-mode "Goal") ; org insists I have a mode…

(defun ob-goal-value (value)
  "Return a string of Goal code evaluating to the given Lisp VALUE."
  (cond
   ((numberp value) (number-to-string value))
   ((stringp value) (format "%S" value))
   ((listp value) (format "(%s)" (mapconcat #'ob-goal-value value ";")))))

(defun org-babel-execute:goal (body params)
  "Execute Goal code BODY according to PARAMS."
  (let ((lines (split-string body "\n"))
        (vars (mapconcat (lambda (param)
                           (pcase param
                             (`(:var . (,var . ,val))
                              (format "%s:%s\n" var (ob-goal-value val)))
                             (_ "")))
                         params))
        (value (eq (alist-get :result-type params) 'value)))
    (with-temp-buffer
      (insert (format (if value
                          "emacslist:{\"(\"+(\" \"/x)+\")\"}
emacslisp:{?[\"d\"=@x;emacslist(o@!x;\"hline\"),o'+.x;(@x)=_@x;$x;emacslist@o'x]}
%s%s\nsay emacslisp[%s]"
                        "%s%s\n%s")
                      vars
                      (string-join (butlast lines) "\n")
                      (car (last lines))))
      (shell-command-on-region (point-min) (point-max) "goal -q" nil t)
      (goto-char (point-min))
      (if value (read (current-buffer)) (buffer-string)))))

(provide 'ob-goal)
