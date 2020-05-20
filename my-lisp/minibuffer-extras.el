;;; -*- lexical-binding: t; -*-

;;; switching to completions

(defun right-char-or-completions ()
  "Move to right if possible, otherwise switch to completions buffer."
  (interactive)
  (if (= (point) (point-max))
      (switch-to-completions)
    (right-char)))

;;; tools for navigating the file system in the minibuffer
;;; bind in minibuffer-local-filename-map

(defun up-directory (arg)
  "Move up a directory (delete backwards to /)."
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (zap-up-to-char (- arg) ?/)
    (delete-minibuffer-contents)))

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
