;;; org-ql-usual-files --- Search in the usual files -*- lexical-binding: t; -*-

;; I usually want to search both agenda files and refile targets

(require 'org-ql-find)
(require 'org-capture)

(defcustom org-ql-usual-files
  (seq-union
   (seq-union (mapcan #'seq-copy (mapcar #'car org-refile-targets))
              org-agenda-files)
   (cl-loop for (_ _ _ (type file . _) . _) in org-capture-templates
            when (string-prefix-p "file" (symbol-name type))
            collect (file-name-concat org-directory file)))
  "Org files I usually want to search with `org-ql-find'."
  :type '(repeat file)
  :group 'org-ql)

(defun org-ql-usual-files ()
  "Return list of org files I usually want to search.
This returns `org-ql-usual-files' plus the current buffer if
it's in `org-mode'."
  (append
   (when (and (derived-mode-p 'org-mode)
              (not (and (buffer-file-name)
                        (member (abbreviate-file-name (buffer-file-name))
                                org-ql-usual-files))))
     (list (current-buffer)))
   org-ql-usual-files))

(defun org-ql-usual-files-find ()
  "Call `org-ql-find' on current buffer and `org-ql-usual-files'."
  (interactive)
  (org-ql-find (org-ql-usual-files)))

(defun org-ql-usual-files-open-link ()
  "Call `org-ql-open-link' on current buffer and `org-ql-usual-files'."
  (interactive)
  (org-ql-open-link (org-ql-usual-files)))

(provide 'org-ql-usual-files)
