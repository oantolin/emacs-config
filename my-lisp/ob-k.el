;; ob-k --- Babel support for ngn|growler/k  -*- lexical-binding: t; -*-

(require 'ob-eval)

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
  (save-match-data
    (string-match "\\`\\([^z-a]*\n\\)?\\(.*\\)\\'" body)
    (let* ((tables (split-string (alist-get :table params "")))
           (vars (mapconcat (lambda (param)
                              (pcase param
                                (`(:var . (,var . ,val))
                                 (format (if (member (symbol-name var) tables)
                                             "%s:{+(`$*x)!+1_x}%s\n"
                                           "%s:%s\n")
                                         var (ob-k-value val)))))
                            params))
           (convert
            (and (eq (alist-get :result-type params) 'value)
                 (not (member "raw" (alist-get :result-params params)))))
           (result (org-babel-eval
                    "k"
                    (format (if convert
                                "%s%s`0:{l:{\"(\",(\" \"/x),\")\"};\
$[`M=@x;l(l@$!x;\"hline\"),o'+.x;`m=@x;\
o@+(!x;.x);(@x)=_@x;$x;`C=@x;`k@x;l@o'x]}[%s]\n"
                              "%s%s%s\n")
                            vars
                            (or (match-string 1 body) "")
                            (match-string 2 body)))))
      (if convert (car (read-from-string result)) result))))

(provide 'ob-k)
