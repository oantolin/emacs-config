;; ob-goal --- Babel support for Goal  -*- lexical-binding: t; -*-

(require 'ob-eval)

(define-derived-mode goal-mode prog-mode "Goal") ; org insists I have a mode…

(defun ob-goal-value (value)
  "Return a string of Goal code evaluating to the given Lisp VALUE."
  (cond
   ((numberp value) (number-to-string value))
   ((stringp value) (format "%S" value))
   ((listp value) (format "(%s)" (mapconcat #'ob-goal-value value ";")))))

(defun org-babel-execute:goal (body params)
  "Execute Goal code BODY according to PARAMS."
  (save-match-data
    (string-match "\\`\\([^z-a]*\n\\)?\\(.*\\)\\'" body)
    (let* ((tables (split-string (alist-get :table params "")))
           (vars (mapconcat (lambda (param)
                              (pcase param
                                (`(:var . (,var . ,val))
                                 (format (if (member (symbol-name var) tables)
                                             "%s:{(*x)!+1_x}%s\n"
                                           "%s:%s\n")
                                         var (ob-goal-value val)))))
                            params))
           (value (eq (alist-get :result-type params) 'value))
           (raw (member "raw" (alist-get :result-params params)))
           (result
            (org-babel-eval
             "goal -q"
             (format (cond
                      ((and value raw) "%s%ssay %s")
                      (value "%s%ssay {l:{\"(\"+(\" \"//x)+\")\"}
?[\"d\"=@x;l(o@!x;\"hline\"),o'+.x;(@x)=_@x;$x;l@o'x]}[%s]")
                      (t "%s%s\n%s"))
                     vars
                     (or (match-string 1 body) "")
                     (match-string 2 body)))))
      (if (or raw (not value)) result (car (read-from-string result))))))

(provide 'ob-goal)
