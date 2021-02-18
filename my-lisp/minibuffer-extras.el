;;; -*- lexical-binding: t; -*-

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

(defun exit-with-top-completion ()
  "Exit minibuffer with top completion candidate."
  (interactive)
  (let ((content (minibuffer-contents-no-properties)))
    (unless (test-completion content
                             minibuffer-completion-table
                             minibuffer-completion-predicate)
      (when-let ((completions (completion-all-sorted-completions)))
        (delete-minibuffer-contents)
        (insert
         (concat
          (substring content 0 (or (cdr (last completions)) 0))
          (car completions)))))
    (exit-minibuffer)))

(provide 'minibuffer-extras)
