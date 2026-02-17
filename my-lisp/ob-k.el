;; ob-k --- Babel support for ngn|growler/k  -*- lexical-binding: t; -*-

(define-derived-mode k-mode prog-mode "K") ; org insists I have a mode…

(defun ob-k-value (value)
  "Return a string of K code evaluating to the given Lisp VALUE."
  (cond
   ((numberp value) (number-to-string value))
   ((stringp value) (let ((print-escape-newlines t))
                      (format (if (= (length value) 1) ",%S" "%S") value)))
   ((listp value) (format "(%s)" (mapconcat #'ob-k-value value ";")))))

(defun org-babel-execute:k (body params)
  "Execute K code BODY according to PARAMS."
  (let* ((lines (split-string body "\n"))
         (tables (mapcar #'intern (split-string (alist-get :table params ""))))
         (vars (mapconcat (lambda (param)
                            (pcase param
                              (`(:var . (,var . ,val))
                               (format (if (member var tables)
                                           "%s:{+(`$*x)!+1_x}%s\n"
                                         "%s:%s\n")
                                       var (ob-k-value val)))))
                          params))
         (value (eq (alist-get :result-type params) 'value))
         (raw (member "raw" (alist-get :result-params params)))
         (convert (and value (not raw))))
    (with-temp-buffer
      (insert (format (cond
                       (convert "%s%s\n`0:{l:{\"(\",(\" \"/x),\")\"};\
$[`M=@x;l(l@$!x;\"hline\"),o'+.x;`m=@x;\
o@+(!x;.x);(@x)=_@x;$x;`C=@x;`k@x;l@o'x]}[%s]\n")
                       (t "%s%s\n%s\n"))
                      vars
                      (string-join (butlast lines) "\n")
                      (car (last lines))))
      (shell-command-on-region (point-min) (point-max) "k" nil t)
      (goto-char (point-min))
      (if convert (read (current-buffer)) (buffer-string)))))

(provide 'ob-k)
