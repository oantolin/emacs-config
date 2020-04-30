;;; -*- lexical-binding: t; -*-

(defun live-completions--update (&optional _start _end _length)
  (when minibuffer-completion-table
    (save-match-data (while-no-input (minibuffer-completion-help)))))

(defconst live-completions--postpone-update
  '(minibuffer-complete
    minibuffer-force-complete)
  "List of commands during which updating completions is postponed.")

(defconst live-completions--cancel-update
  '(choose-completion
    minibuffer-complete-and-exit
    minibuffer-force-complete-and-exit)
  "List of commands for which updating completions is canceled.")

(defun live-completions--postpone-update (fn &rest args)
  (combine-after-change-calls (apply fn args)))

(defun live-completions--cancel-update (fn &rest args)
  (let ((inhibit-modification-hooks t)) (apply fn args)))

(defun live-completions--setup ()
  (live-completions--update)
  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions #'live-completions--update))

(defvar live-completions-horizontal-separator "\n")

(defun live-completions--delete-first-line (&rest _)
  "Delete first line in current buffer.
Used to remove the message at the top of the *Completions* buffer."
  (goto-char (point-min))
  (delete-region (point) (1+ (line-end-position)))
  (insert (propertize "@" 'invisible t)))

(defun live-completions--single-column (strings)
  "Insert completion candidates into current buffer in a single column."
  (live-completions--delete-first-line)
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
        (add-hook 'minibuffer-setup-hook #'live-completions--setup)
        (advice-add 'completion--insert-strings :after
                    #'live-completions--delete-first-line)
        (dolist (cmd live-completions--postpone-update)
          (advice-add cmd :around #'live-completions--postpone-update))
        (dolist (cmd live-completions--cancel-update)
          (advice-add cmd :around #'live-completions--cancel-update)))
    (remove-hook 'minibuffer-setup-hook #'live-completions--setup)
    (advice-remove 'completion--insert-strings
                   #'live-completions--delete-first-line)
    (dolist (cmd live-completions--postpone-update)
      (advice-remove cmd #'live-completions--postpone-update))
    (dolist (cmd live-completions--cancel-update)
      (advice-remove cmd #'live-completions--cancel-update))
    (dolist (buffer (buffer-list))
      (when (minibufferp buffer)
        (setf (buffer-local-value 'after-change-functions buffer)
              (remove #'live-completions--update
                      (buffer-local-value 'after-change-functions buffer)))))))

(defun live-completions-toggle-columns ()
  "Toggle between single and multi column completion views."
  (interactive)
  (if (advice-member-p #'live-completions--single-column
                       'completion--insert-strings)
      (advice-remove 'completion--insert-strings
                     #'live-completions--single-column)
    (advice-add 'completion--insert-strings :override
                #'live-completions--single-column))
  (live-completions--update))

(provide 'live-completions)
