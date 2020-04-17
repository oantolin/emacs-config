;;; -*- lexical-binding: t; -*-

(defun no-dollars ()
  "Convert all LaTeX math formulas in buffer from dollars to \\(\\) and \\[\\]."
  (interactive)
  (cl-flet ((replace-all (a b &optional c)
               (goto-char (point-min))
               (while (search-forward a nil t)
                 (replace-match b t t)
                 (when c
                   (search-forward a)
                   (replace-match c t t)))))
    (save-excursion
      (replace-all "\\$" "\\dollar ")
      (replace-all "$$" "\\[" "\\]")
      (replace-all "$" "\\(" "\\)")
      (replace-all "\\dollar " "\\$"))))

(defgroup math-delimiters nil
  "Delimiters for LaTeX math mode."
  :group 'tex)

(defcustom math-delimiters-inline
  '("\\(" . "\\)")
  "Delimiters to use for inline math mode."
  :group 'math-delimiters
  :type '(choice
          (const :tag "Parentheses" ("\\(" . "\\)"))
          (const :tag "Dollars" ("$" . "$"))
          (cons :tag "Other" string string)))

(defcustom math-delimiters-display
  '("\\[" . "\\]")
  "Delimiters to use for display math mode."
  :group 'math-delimiters
  :type '(choice
          (const :tag "Brackets" ("\\[" . "\\]"))
          (const :tag "Dollars" ("$$" . "$$"))
          (cons :tag "Other" string string)))

;;;###autoload
(defun math-delimiters-toggle (arg)
  "Toggle between $...$ and \\(...\\) for inline math.
When ARG is non-nil (interactively, if called with universal
prefix argument), also toggle the display math delimiters between
$$...$$ and \\[...\\]."
  (interactive "P")
  (setf math-delimiters-inline
        (if (equal math-delimiters-inline '("\\(" . "\\)"))
            '("$" . "$")
          '("\\(" . "\\)")))
  (when arg
    (setf math-delimiters-display
          (if (equal math-delimiters-display '("\\[" . "\\]"))
              '("$$" . "$$")
            '("\\[" . "\\]"))))
  (message "%sinline%s and %sdisplay%s"
           (car math-delimiters-inline)
           (cdr math-delimiters-inline)
           (car math-delimiters-display)
           (cdr math-delimiters-display)))

;;;###autoload
(defun math-delimiters-insert ()
  "Insert math delimiters.
If region is active surround it.  When repeated, toggle between
display and inline math.  Also toggle between display and inline
if called from inside empty math delimiters, or just after math
delimeters."
  (interactive)
  (if (and (eq major-mode 'org-mode)
           (save-excursion
             (beginning-of-line)
             (looking-at "^\\s-*#\\+TBLFM: ")))
      (insert "$")
    (let ((open (car math-delimiters-inline))
          (close (cdr math-delimiters-inline))
          (display-open (car math-delimiters-display))
          (display-close (cdr math-delimiters-display)))
      (cl-flet ((after-p (close)
                         (looking-back (regexp-quote close)
                                       (- (point) (length close))))
                (flip-after (from-open from-close to-open to-close)
                            (delete-char (- (length from-close)))
                            (insert to-close)
                            (let ((end (point)))
                              (backward-char (length to-close))
                              (search-backward from-open)
                              (delete-char (length from-open))
                              (insert to-open)
                              (goto-char (+ end (- (length to-open)
                                                   (length from-open))))))
                (middle-p (open close)
                          (and (looking-at (regexp-quote close))
                               (looking-back (regexp-quote open)
                                             (- (point) (length open)))))
                (flip-middle (from-open from-close to-open to-close)
                             (delete-char (- (length from-open)))
                             (delete-char (length from-close))
                             (insert to-open to-close)
                             (backward-char (length to-close))))
        (cond
         ((use-region-p)
          (let ((end (region-end)))
            (goto-char (region-beginning))
            (insert open)
            (goto-char (+ end (length open)))
            (insert close)))
         ((middle-p display-open display-close)
          (flip-middle display-open display-close open close))
         ((after-p display-close)
          (flip-after display-open display-close open close))
         ((middle-p open close)
          (flip-middle open close display-open display-close))
         ((after-p close)
          (flip-after open close display-open display-close))
         (t
          (insert open close)
          (backward-char (length close))))))))

(provide 'math-delimiters)
