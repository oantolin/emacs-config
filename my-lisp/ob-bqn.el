;; ob-bqn --- Babel support for BQN  -*- lexical-binding: t; -*-

(defvar org-babel-error-buffer-name)

(defun ob-bqn-value (value)
  "Return a string of BQN code evaluating to the given Lisp VALUE."
  (cond
   ((numberp value) (number-to-string value))
   ((stringp value) (format "%S" value))
   ((listp value) (format "⟨%s⟩" (mapconcat #'ob-bqn-value value ",")))))

(defun org-babel-execute:bqn (body params)
  "Execute BQN code BODY according to PARAMS."
  (save-match-data
    (string-match "\\`\\([^z-a]*\n\\)?\\(.*\\)\\'" body)
    (let* ((tables (split-string (alist-get :table params "")))
           (vars (mapconcat
                  (lambda (param)
                    (pcase param
                      (`(:var . (,var . ,val))
                       (if (not (member (symbol-name var) tables))
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
                          ((and value raw) "%s%s•Show %s")
                          (value "%s%s•Out{ List ← {\"(\" ∾ (∾\" \"⊸∾¨𝕩) ∾ \")\"}
Lisp ← {  6=•Type 𝕩 ? k←•ns.Keys 𝕩 ⋄ List ⟨List k, \"hline\"⟩∾𝕊¨<˘⍉>𝕩⊸•ns.Get¨k ;
2=•Type 𝕩 ? •Fmt≍𝕩 ;
0=•Type 𝕩 ? { 1<=𝕩 ? List <∘Lisp˘𝕩 ; ∧´2=•Type¨𝕩 ? •Fmt 𝕩 ; List Lisp¨𝕩 } 𝕩 ;
'-'¨⌾(('¯'=r)⊸/)r←•Fmt 𝕩 }
Lisp } {𝕎𝕩} %s")
                          (t "%s%s%s"))
                         vars
                         (or (match-string 1 body) "")
                         (match-string 2 body)))
         (current-buffer)
         org-babel-error-buffer-name)
        (goto-char (point-min))
        (if (and value (not raw)) (read (current-buffer)) (buffer-string))))))

(provide 'ob-bqn)
