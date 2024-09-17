;;; text-extras.el --- Miscellaneous text editing commands    -*- lexical-binding: t; -*-

;;; the most miscellaneous commands of all

(defun unfill-paragraph ()
  "Join a paragraph into a single line."
  (interactive)
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
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

(defun duplicate-line-kill-word ()
  "Duplicate the current line and kill the word at point in the duplicate."
  (interactive)
  (let ((column (current-column)))
    (forward-line)
    (open-line 1)
    (copy-from-above-command)
    (beginning-of-line)
    (forward-char column)
    (unless (looking-at "\\<")
      (backward-word))
    (kill-word 1)))

(defun goto-random-line ()
  "Goto a random line in the buffer."
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (forward-line (random (count-lines (point-min) (point-max)))))

(defvar-keymap random-line-map
  :doc "Repeat map for `goto-random-line'"
  "r" #'goto-random-line)

(put 'goto-random-line 'repeat-map 'random-line-map)

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

;;; marking things

(defmacro define-thing-marker (fn-name things forward-thing &rest extra)
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

(define-thing-marker mark-line "lines" forward-line
  :post (unless (= (preceding-char) ?\n)
          (setq arg (1- arg))))

(define-thing-marker mark-char "characters" forward-char)

(define-thing-marker mark-my-word "words" forward-word
  :pre (when (and (looking-at "\\>") (> arg 0))
         (forward-word -1)))

(define-thing-marker mark-non-whitespace "vim WORDS"
  forward-to-whitespace)

;;; case change commands

(defmacro define-case-changer (case)
  "Define a CASE change command that does what I want.
The defined command will change the case of: the region if
active, and of the next prefix argument many words, starting with
the word point is either on or right after (the \"or right
after\" bit is the only difference with the built-in case-dwim
commands)."
  (cl-flet ((case-fn (suffix) (intern (format "%s-%s" case suffix))))
      `(defun ,(case-fn 'dwiw) (arg)
     ,(format "%s active region or next ARG words.
If called without a prefix argument and no active region with
point at the end of a word, then %s the previous word.  This is
the only difference between this command and %s-dwim."
              (capitalize (symbol-name case)) case case)
     (interactive "*p")
     (if (use-region-p)
         (,(case-fn 'region)
           (region-beginning) (region-end) (region-noncontiguous-p))
       (when (and (eolp) (= arg 1))
           (setq arg -1))
       (,(case-fn 'word) arg)))))

(define-case-changer upcase)
(define-case-changer downcase)
(define-case-changer capitalize)

;;; poor man's paredit

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

;;; functions for hooks

(defun force-truncate-lines ()
  "Force line truncation. For use in hooks."
  (setq truncate-lines t))

(defun turn-off-visual-line-mode ()
  "Turn off `visual-line-mode'.  For use in hooks."
  (visual-line-mode -1))

;;; Continue dabbreving with the greatest of ease

(defun dabbrev-next (arg)
  "Insert the next ARG words from where previous expansion was found."
  (interactive "p")
  (dotimes (_ arg)
    (insert " ")
    (dabbrev-expand 1)))

(defun dabbrev-complete-next ()
  "Choose a continuation for the previous expansion with completion."
  (interactive)
  (insert " ")
  (dabbrev-completion))

;;; pop up a buffer for text to send to clipboard

(defun text-to-clipboard--done ()
  "Copy buffer contents to clipboard and quit window."
  (interactive)
  (gui-set-selection
   'CLIPBOARD
   (buffer-substring-no-properties (point-min) (point-max)))
  (quit-window :kill))

(defvar-keymap text-to-clipboard-minor-mode-map
  "C-c C-c" #'text-to-clipboard--done)

(define-minor-mode text-to-clipboard-minor-mode
  "Minor mode binding a key to quit window and copy buffer to clipboard.")

(defun text-to-clipboard ()
  "Pop up a temporary buffer to collect text to send to the clipboard.
The pop up buffer is in `markdown-mode' and uses the TeX input
method.  Use \\<text-to-clipboard-minor-mode-map>\\[text-to-clipboard--done] to send the buffer contents to the clipboard
and quit the window, killing the buffer.

If the region is active, use the region as the initial contents
for the pop up buffer."
  (interactive)
  (let ((region (when (use-region-p)
                  (buffer-substring-no-properties
                   (region-beginning) (region-end)))))
    (pop-to-buffer (generate-new-buffer "*clipboard*"))
    (when region (insert region)))
  (markdown-mode)
  (text-to-clipboard-minor-mode))

(autoload 'embark--act "embark")
(autoload 'embark--targets "embark")

(defun insert-completion-candidate ()
  "Insert the current completion candidate and quit the minibuffer."
  (interactive)
  (embark--act 'embark-insert (car (embark--targets)) t))

(defun apply-macro-to-rest-of-paragraph ()
  "Apply last keyboard macro to each line in the rest of the current paragraph."
  (interactive)
  (when defining-kbd-macro (kmacro-end-macro nil))
  (apply-macro-to-region-lines
   (line-beginning-position 2)
   (save-excursion (end-of-paragraph-text) (point))))

(defun echo-area-tooltips ()
  "Show tooltips in the echo area automatically for current buffer."
  (setq-local help-at-pt-display-when-idle t
              help-at-pt-timer-delay 0)
  (help-at-pt-cancel-timer)
  (help-at-pt-set-timer))

(provide 'text-extras)
