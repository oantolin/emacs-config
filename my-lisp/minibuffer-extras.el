;;; -*- lexical-binding: t; -*-

;; bind these in minibuffer-local-filename-map
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

(eval-when-compile (require 'subr-x))

;; bind this in minibuffer-local-map
(defun insert-minibuffer-contents (arg)
  "Insert minibuffer contents in previously selected buffer and exit.
With a prefix ARG do not exit minibuffer"
  (interactive "P")
  (minibuffer-force-complete)
  (let ((contents (minibuffer-contents)))
    (with-minibuffer-selected-window
      (when (use-region-p)
        (delete-region (region-beginning) (region-end)))
      (insert contents)))
  (unless arg (abort-recursive-edit)))

(defun exit-minibuffer-save-contents (arg)
  "Exit minibuffer saving contents on the kill-ring.
With prefix ARG do not exit minibuffer."
  (interactive)
  (minibuffer-force-complete)
  (kill-new (minibuffer-contents))
  (unless arg (abort-recursive-edit)))

(defun insert-region-in-minibuffer ()
  "Insert the active region in the minibuffer."
  (interactive)
  (insert
   (with-minibuffer-selected-window
     (if (use-region-p)
         (buffer-substring (region-beginning) (region-end))
       (user-error "No active region")))))

(defvar scheduled-minibuffer-insertion nil)

(defun insert-scheduled-minibuffer-text ()
  (when scheduled-minibuffer-insertion
    (insert scheduled-minibuffer-insertion)
    (setq scheduled-minibuffer-insertion nil)))

(add-hook 'minibuffer-setup-hook #'insert-scheduled-minibuffer-text)

(defun schedule-for-next-minibuffer ()
  "Schedule insertion minibuffer contents at next minibuffer prompt."
  (interactive)
  (minibuffer-force-complete)
  (setq scheduled-minibuffer-insertion (minibuffer-contents))
  (abort-recursive-edit))

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
