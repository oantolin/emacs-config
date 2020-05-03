;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup live-completions nil
  "Live updating of the *Completions* buffer."
  :group 'completion)

(defcustom live-completions-horizontal-separator "\n"
  "Candidate separator for live-completions in single-column mode.
The separator should contain at least one newline."
  :type 'string
  :group 'live-completions)

(defun live-completions-set-columns (columns)
  "Set how many COLUMNS of candidates are displayed.

Called from Lisp code COLUMNS should be one of the symbols
`single', `multiple' or `toggle'. 

When called interactively without prefix argument, toggle between
single and multiple columns.  Called with a numerical prefix of 1,
set single column mode, any other prefix argument sets multiple
columns."
  (interactive
   (list (pcase current-prefix-arg
           ('nil 'toggle)
           (1 'single)
           (_ 'multiple))))
  (pcase columns
    ('single
     (advice-add 'completion--insert-strings :around
                 #'live-completions--single-column '((depth . 1))))
    ('multiple
     (advice-remove 'completion--insert-strings
                    #'live-completions--single-column))
    ('toggle
     (live-completions-set-columns
      (if (advice-member-p #'live-completions--single-column
                           'completion--insert-strings)
          'multiple
        'single))))
  (when (and (bound-and-true-p live-completions-mode)
             (not (eq columns 'toggle)))
    (live-completions--update)))

(defcustom live-completions-columns 'single
  "How many columns of candidates live-completions displays.
To change the value from Lisp code use
`live-completions-set-columns'."
  :type '(choice
          (const :tag "Single column" single)
          (const :tag "Multiple columns" multiple))
  :set (lambda (_ columns)
         (live-completions-set-columns columns))
  :group 'live-completions)

(defface live-completions-forceable-candidate
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :background "PaleGreen4")
    (((class color) (min-colors 88) (background light)) :background "PaleGreen")
    (t :foreground "blue"))
  "Face for candidate that force-completion would select."
  :group 'live-completions)

(defun live-completions--update ()
  (let ((while-no-input-ignore-events '(selection-request)))
    (while-no-input
      (when minibuffer-completion-table
        (condition-case nil
            (save-excursion
              (goto-char (point-max))
              (let ((minibuffer-message-timeout 0))
                (minibuffer-completion-help)))
          (quit (abort-recursive-edit)))))))

(defun live-completions--highlight-forceable (completions &optional _common)
  (let ((first (car (member (car (completion-all-sorted-completions))
                            completions))))
    (when first
      (font-lock-prepend-text-property
       0 (length first)
       'face 'live-completions-forceable-candidate
       first))))

(defun live-completions--setup ()
  (sit-for 0.01)
  (add-hook 'post-command-hook #'live-completions--update nil t))

(defun live-completions--hide-first-line (&rest _)
  "Make first line invisible in current buffer.
Used to hide the message at the top of the *Completions* buffer."
  (save-excursion
    (goto-char (point-min))
    (put-text-property (point) (1+ (line-end-position)) 'invisible t)))

(defun live-completions--single-column (_oldfun strings)
  "Insert completion candidates into current buffer in a single column."
  (dolist (str strings)
    (if (not (consp str))
        (put-text-property (point) (progn (insert str) (point))
                           'mouse-face 'highlight)
      (put-text-property (point) (progn (insert (car str)) (point))
                         'mouse-face 'highlight)
      (let ((beg (point))
            (end (progn (insert (cadr str)) (point))))
        (put-text-property beg end 'mouse-face nil)
        (font-lock-prepend-text-property beg end 'face
                                         'completions-annotations)))
    (insert live-completions-horizontal-separator))
  (delete-region (- (point) (length live-completions-horizontal-separator))
                 (point))
  (insert "\n"))

(define-minor-mode live-completions-mode
  "Live updating of the *Completions* buffer."
  :global t
  (if live-completions-mode
      (progn
        (when (bound-and-true-p icomplete-mode) (icomplete-mode -1))
        (add-hook 'minibuffer-setup-hook #'live-completions--setup)
        (advice-add 'display-completion-list :before
                    #'live-completions--highlight-forceable)
        (advice-add 'completion--insert-strings :before
                    #'live-completions--hide-first-line))
    (remove-hook 'minibuffer-setup-hook #'live-completions--setup)
    (advice-remove 'display-completion-list
                   #'live-completions--highlight-forceable)
    (advice-remove 'completion--insert-strings
                   #'live-completions--hide-first-line)
    (dolist (buffer (buffer-list))
      (when (minibufferp buffer)
        (remove-hook 'post-command-hook #'live-completions--update t)))))

(provide 'live-completions)
