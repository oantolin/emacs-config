;;; -*- lexical-binding: t; -*-

(defun unfill-paragraph ()
  "Join a paragraph into a single line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil t)))

(defun copy-word-from-above (arg)
  "Copy ARG words from the nonblank line above. With a negative
argument, copy the rest of the line."
  (interactive "p")
  (let ((p (point))
        (c (current-column)))
    (beginning-of-line)
    (backward-char 1)
    (skip-chars-backward " \t\n\r")
    (move-to-column c)
    (let* ((beg (point))
           (lim (line-end-position))
           (end (if (< arg 0) lim (forward-word arg) (point))))
      (goto-char p)
      (insert (buffer-substring beg (min end lim))))))

(defmacro def-thing-marker (fn-name things forward-thing &rest extra)
  `(defun ,fn-name (&optional arg allow-extend)
     ,(format "Mark ARG %s starting with the current one. If ARG is negative,
mark -ARG %s ending with the current one.

Interactively (or if ALLOW-EXTEND is non-nil), if this command is
repeated or (in Transient Mark mode) if the mark is active, it
marks the next ARG %s after the ones already marked." things things things)
     (interactive "p\np")
     (unless arg (setq arg 1))
     (if (and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (,forward-thing arg)
            (point)))
       ,(plist-get extra :pre)
       (,forward-thing arg)
       ,(plist-get extra :post)
       (push-mark nil t t)
       (,forward-thing (- arg)))))

(def-thing-marker mark-line "lines" forward-line
  :post (unless (= (preceding-char) ?\n)
          (setq arg (1- arg))))

(def-thing-marker mark-char "characters" forward-char)

(def-thing-marker mark-my-word "words" forward-word
  :pre (when (and (looking-at "\\>") (> arg 0))
         (forward-word -1)))

(defun mark-inside-sexp ()
  "Mark inside a sexp."
  (interactive)
  (let (beg end)
    (backward-up-list 1 t t)
    (setq beg (1+ (point)))
    (forward-sexp)
    (setq end (1- (point)))
    (goto-char beg)
    (push-mark)
    (goto-char end))
  (activate-mark))

(defun kill-inside-sexp ()
  "Kill inside a sexp."
  (interactive)
  (mark-inside-sexp)
  (kill-region (mark) (point)))

(defun unwrap-sexp ()
  "Unwrap a sexp."
  (interactive)
  (let (end)
    (mark-inside-sexp)
    (delete-char 1)
    (setq end (1- (point)))
    (goto-char (mark))
    (delete-char -1)
    (set-mark end)))

(defun unwrap-mark-sexp ()
  "Unwrap a sexp and mark the contents."
  (interactive)
  (unwrap-sexp)
  (setq deactivate-mark nil))

(defun align-matches (arg start end regexp)
  "Align matches of the given regular expression.

By default align all matches, with universal prefix align only first match."
  (interactive "P\nr\nsAlign regexp: ")
  (align-regexp start end (concat "\\(\\s-*\\)" regexp) 1 1
                (not (equal arg '(4)))))

(defun goto-random-line ()
  "Goto a random line in the buffer."
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (forward-line (random (count-lines (point-min) (point-max)))))

;; From Stefan Monier (https://emacs.stackexchange.com/a/8177/2221)
(defun ordered-completion-table (completions)
  "Make a completion table that maintains the entries in the given order."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action completions string pred))))

(defun completing-yank ()
  "Insert item from kill-ring, selected with completion."
  (interactive)
  (when (eq last-command 'yank)
    (let ((inhibit-read-only t))
      (delete-region (point) (mark t))))
  (live-completions-single-column-do
      (concat "\n" (make-string (1- (window-width)) ?—) "\n")
    (insert-for-yank
     (completing-read "Yank: " (ordered-completion-table kill-ring) nil t))))

(defun goto-matching-line ()
  "Go to matching line selected with completion."
  (interactive)
  (let ((lines
         (save-excursion
           (goto-char (point-min))
           (cl-loop for start = (point) until (eobp)
                    do (forward-line)
                    collect (cons (buffer-substring start (1- (point)))
                                  start)))))
    (goto-char
     (cdr
      (assoc
       (live-completions-single-column-do ()
         (completing-read
          "Goto line: " (ordered-completion-table lines) nil t))
       lines)))))

(defun pipe-region (start end command)
  "Pipe region through shell command. If the mark is inactive,
pipe whole buffer."
  (interactive (append
                (if (use-region-p)
                    (list (region-beginning) (region-end))
                  (list (point-min) (point-max)))
                (list (read-shell-command "Pipe through: "))))
  (let ((exit-status (call-shell-region start end command t t)))
    (unless (equal 0 exit-status)
      (let ((error-msg (string-trim-right (buffer-substring (mark) (point)))))
        (undo)
        (cond
         ((null exit-status)
          (message "Unknown error"))
         ((stringp exit-status)
          (message "Signal %s" exit-status))
         (t
          (message "[%d] %s" exit-status error-msg)))))))

(defun forward-to-whitespace (arg)
  "Move forward to the end of the next sequence of non-whitespace
characters. With argument, do this that many times."
  (interactive "^p")
  (re-search-forward
   (if (> arg 0)
       "[^[:blank:]\n]\\(?:[[:blank:]\n]\\|\\'\\)"
     "\\(?:[[:blank:]\n]\\|\\`\\)[^[:blank:]\n]")
   nil t arg)
  (unless (= (point) (if (> arg 0) (point-max) (point-min)))
    (forward-char (if (> arg 0) -1 1))))

(defun backward-to-whitespace (arg)
  "Move backward to the beginning of the previous sequence of
non-whitespace characters. With argument, do this that many
times."
  (interactive "^p")
  (forward-to-whitespace (- arg)))

(def-thing-marker mark-non-whitespace "vim WORDS"
  forward-to-whitespace)

(defun force-truncate-lines ()
  "Force line truncation. For use in hooks."
  (setq truncate-lines t))

(provide 'misc-text)
