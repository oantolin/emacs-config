;;; -*- lexical-binding: t; -*-

(defun epithet-for-eww-title ()
  "Suggest a name for a `eww-mode' buffer."
  (when (derived-mode-p 'eww-mode)
    (format "*eww: %s*" (plist-get eww-data :title))))

(defun epithet-for-eww-url ()
  "Suggest a name for a `eww-mode' buffer."
  (when (derived-mode-p 'eww-mode)
    (format "*eww: %s*" (plist-get eww-data :url))))

(defun epithet-for-Info ()
  "Suggest a name for an `Info-mode' buffer."
  (when (derived-mode-p 'Info-mode)
    (format "*info (%s)%s*"
            (file-name-sans-extension
             (file-name-nondirectory Info-current-file))
            Info-current-node)))

(defun epithet-for-help ()
  "Suggest a name for a `help-mode' buffer."
  (when (derived-mode-p 'help-mode)
    (format "*Help: %s*" (car (last help-xref-stack-item 2)))))

(defgroup eipthet nil
  "Rename buffers with descriptive names."
  :group 'convenience)

(defcustom epithet-suggesters
  '(epithet-for-eww-title epithet-for-Info epithet-for-help)
  "List of functions to suggest a name for the current buffer.
Each function should either return a string suggestion or nil."
  :type 'hook
  :group 'epithet)

(defun epithet-suggestion ()
  "Suggest a descriptive name for current buffer.
Runs down the `epithet-suggesters' list and picks the first
non-nil suggestion."
  (run-hook-with-args-until-success 'epithet-suggesters))

(defun epithet-rename-buffer (&optional new-name)
  "Automatically give current buffer a descriptive name.
Called interactively with a universal prefix argument, prompt for
NEW-NAME (using the suggestion as default value)."
  (interactive
   (list
    (let ((suggestion (epithet-suggestion)))
      (if (equal current-prefix-arg '(4))
          (read-string
           (if suggestion
               (format "Rename buffer (default %s): " suggestion)
             "Rename buffer: ")
           nil nil suggestion)
        suggestion))))
  (rename-buffer (or new-name (epithet-suggestion) (buffer-name)) t))

(provide 'epithet)
