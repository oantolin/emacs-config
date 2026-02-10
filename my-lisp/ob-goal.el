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
  (let* ((lines (split-string body "\n"))
         (tables (mapcar #'intern (split-string (alist-get :table params ""))))
         (vars (mapconcat (lambda (param)
                            (pcase param
                              (`(:var . (,var . ,val))
                               (format (if (member var tables)
                                           "%s:{(*x)!+1_x}%s\n"
                                         "%s:%s\n")
                                       var (ob-goal-value val)))))
                          params))
         (value (eq (alist-get :result-type params) 'value))
         (raw (member "raw" (alist-get :result-params params))))
    (with-temp-buffer
      (insert (format (cond
                       ((and value raw) "%s%s\nsay %s")
                       (value "%s%s\nsay {l:{\"(\"+(\" \"//x)+\")\"}
?[\"d\"=@x;l(o@!x;\"hline\"),o'+.x;(@x)=_@x;$x;l@o'x]}[%s]")
                       (t "%s%s\n%s"))
                      vars
                      (string-join (butlast lines) "\n")
                      (car (last lines))))
      (shell-command-on-region (point-min) (point-max) "goal -q" nil t)
      (goto-char (point-min))
      (if (and value (not raw)) (read (current-buffer)) (buffer-string)))))

(provide 'ob-goal)
