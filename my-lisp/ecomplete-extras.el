;;; ecomplete-extras.el --- A few extra function for ecomplete  -*- lexical-binding: t; -*-

(require 'ecomplete)

(defun email--name+address (email &optional prompt)
  "Return a pair of the name and address for an EMAIL.
If the EMAIL does not contain a name and PROMPT is non-nil, prompt for it."
  (let (name)
    (cond
     ((string-match "^\\(.*\\) <\\(.*\\)>$" email)
      (setq name (match-string 1 email)
            email (match-string 2 email)))
      (prompt (setq name (read-string "Name: "))))
    (cons name email)))

(defun add-email-to-ecomplete (email)
  "Add email address to ecomplete's database."
  (interactive "sEmail address: ")
  (pcase-let ((`(,name . ,email) (email--name+address email t)))
    (ecomplete-add-item
     'mail email
     (format (cond ((equal name "") "%s%s")
                   ((string-match-p "^\\(?:[A-Za-z0-9 ]*\\|\".*\"\\)$" name)
                    "%s <%s>")
                   (t "\"%s\" <%s>"))
             name email))
    (ecomplete-save)))

(defun remove-email-from-ecomplete (email)
  "Remove email address from ecomplete's database."
  (interactive
   (list (completing-read "Email address: "
                          (ecomplete-completion-table 'mail))))
  (when-let ((email (cdr (email--name+address email))) 
             (entry (ecomplete-get-item 'mail email)))
    (setf (cdr (assq 'mail ecomplete-database))
          (remove entry (cdr (assq 'mail ecomplete-database))))
    (ecomplete-save)))

(provide 'ecomplete-extras)
