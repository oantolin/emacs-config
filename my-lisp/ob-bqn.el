;; ob-bqn --- Babel support for BQN  -*- lexical-binding: t; -*-

(defun ob-bqn-value (value)
  "Return a string of BQN code evaluating to the given Lisp VALUE."
  (cond
   ((numberp value) (number-to-string value))
   ((stringp value) (format "%S" value))
   ((listp value) (format "⟨%s⟩" (mapconcat #'ob-bqn-value value ",")))))

(defun org-babel-execute:bqn (body params)
  "Execute BQN code BODY according to PARAMS."
  (let* ((lines (split-string body "\n"))
         (tables (mapcar #'intern (split-string (alist-get :table params ""))))
         (vars (mapconcat (lambda (param)
                            (pcase param
                              (`(:var . (,var . ,val))
                               (if (not (member var tables))
                                   (format "%s←>⎊⊢%s\n" var (ob-bqn-value val))
                                 (format
                                  "%s←{[%s]⇐⍉>%s}"
                                  var
                                  (mapconcat
                                   (lambda (x) (format "%s" x)) (car val) ",")
                                  (ob-bqn-value (cdr val)))))))
                          params))
         (value (eq (alist-get :result-type params) 'value))
         (raw (member "raw" (alist-get :result-params params))))
    (with-temp-buffer
      (shell-command
       (format "bqn -e \"$(cat <<'EOF'\n%s\nEOF\n)\""
               (format (cond
                        ((and value raw) "%s%s\n•Show %s")
                        (value "%s%s\n•Out{ List ← {\"(\" ∾ (∾\" \"⊸∾¨𝕩) ∾ \")\"}
Lisp ← {  6=•Type 𝕩 ? k←•ns.Keys 𝕩 ⋄ List ⟨List k, \"hline\"⟩∾𝕊¨<˘⍉>𝕩⊸•ns.Get¨k ;
2=•Type 𝕩 ? •Fmt≍𝕩 ;
0=•Type 𝕩 ? { 1<=𝕩 ? List <∘Lisp˘𝕩 ; ∧´2=•Type¨𝕩 ? •Fmt 𝕩 ; List Lisp¨𝕩 } 𝕩 ;
'-'¨⌾(('¯'=r)⊸/)r←•Fmt 𝕩 }
Lisp } {𝕎𝕩} %s")
                        (t "%s%s\n%s"))
                       vars
                       (string-join (butlast lines) "\n")
                       (car (last lines))))
       t)
      (goto-char (point-min))
      (message "OUT«\n%s»" (buffer-string))
      (if (and value (not raw)) (read (current-buffer)) (buffer-string)))))

(provide 'ob-bqn)
