;;; -*- lexical-binding: t; -*-

;;; tools for navigating the file system in the minibuffer
;;; bind in minibuffer-local-filename-map

(defun up-directory (arg)
  "Move up a directory (delete backwards to /)."
  (interactive "p")
  (condition-case nil
      (zap-up-to-char (- arg) ?/)
    (t (delete-minibuffer-contents))))

(autoload 'bookmark-maybe-load-default-file "bookmark")
(defvar bookmark-alist)

(defun cdb--bookmarked-directories ()
  (bookmark-maybe-load-default-file)
  (cl-loop for (name . props) in bookmark-alist
           for fn = (cdr (assq 'filename props))
           when (and fn (string-suffix-p "/" fn))
           collect (cons name fn)))

(defun cd-bookmark (bm)
  "Insert the path of a bookmarked directory."
  (interactive
   (list (let ((enable-recursive-minibuffers t))
           (completing-read
            "Directory: " (cdb--bookmarked-directories) nil t))))
  (when (minibufferp)
    (delete-region (minibuffer-prompt-end) (point-max)))
  (insert (cdr (assoc bm (cdb--bookmarked-directories)))))

;;; acting on the first completion
;;; bind in minibuffer-local-completion-map

(defmacro define-completion-action (name docstring &rest body)
  "Define a command that acts on the first minibuffer completion.
The defined command will exit the minibuffer unless provided with
a prefix argument. The BODY can use the variable `completion' to
refer to the first completion."
  (declare (indent defun))
  (let ((old (make-symbol "old"))
        (arg (make-symbol "arg")))
    `(defun ,name (,arg)
       ,(concat docstring "\nWith prefix ARG do not exit minibuffer.")
       (interactive "P")
       (let ((,old (minibuffer-contents)))
         (minibuffer-force-complete nil nil t)
         (let ((completion (minibuffer-contents)))
           ,@body)
         (if (null ,arg)
             (abort-recursive-edit)
           (delete-minibuffer-contents)
           (insert ,old))))))

(define-completion-action insert-minibuffer-contents
  "Insert minibuffer contents in previously selected buffer and exit."
  (with-minibuffer-selected-window
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert completion)))

(define-completion-action exit-minibuffer-save-contents
  "Exit minibuffer saving contents on the kill-ring."
  (kill-new completion))

(defun insert-region-in-minibuffer ()
  "Insert the active region in the minibuffer."
  (interactive)
  (insert
   (with-minibuffer-selected-window
     (if (use-region-p)
         (buffer-substring (region-beginning) (region-end))
       (user-error "No active region")))))

(defvar scheduled-minibuffer-insertion nil)
(define-completion-action schedule-for-next-minibuffer
  "Schedule insertion of first completion at next minibuffer prompt."
  (setq scheduled-minibuffer-insertion completion))

(defun insert-scheduled-minibuffer-text ()
  (when scheduled-minibuffer-insertion
    (delete-minibuffer-contents)
    (insert scheduled-minibuffer-insertion)
    (setq scheduled-minibuffer-insertion nil)))

(add-hook 'minibuffer-setup-hook #'insert-scheduled-minibuffer-text)

;;; use minibuffer for in-buffer completion
;;; set as completion-in-region-function

(defun completing-read-in-region (start end collection &optional predicate)
  "Prompt for completion of region in the minibuffer if non-unique.
Use as a value for `completion-in-region-function'."
  (if (and (minibufferp) (not (string= (minibuffer-prompt) "Eval: ")))
      (completion--in-region start end collection predicate)
    (let* ((initial (buffer-substring-no-properties start end))
           (limit (car (completion-boundaries initial collection predicate "")))
           (all (completion-all-completions initial collection predicate
                                            (length initial)))
           (completion (cond
                        ((atom all) nil)
                        ((and (consp all) (atom (cdr all)))
                         (concat (substring initial 0 limit) (car all)))
                        (t (completing-read
                            "Completion: " collection predicate t initial)))))
      (if (null completion)
          (progn (message "No completion") nil)
        (delete-region start end)
        (insert completion)
        t))))

(provide 'minibuffer-extras)
