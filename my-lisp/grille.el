;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defcustom grille-limit 50
  "Maximum number of candidates"
  :type 'integer
  :group 'grille)

(defvar grille--columns nil)

(defvar grille-button-keymap
  `(keymap (9 . grille--insert) ,@button-map))

(define-button-type 'grille-button
  'face 'default
  'action 'grille--choose
  'keymap grille-button-keymap)

(defun grille--boundaries ()
  (let ((contents (minibuffer-contents))
        (pt (- (point) (minibuffer-prompt-end))))
    (completion-boundaries
     (substring contents 0 pt)
     minibuffer-completion-table
     minibuffer-completion-predicate
     (substring contents pt))))

(defun grille--insert (completion)
  (interactive)
  (when-let ((label (button-label completion))
             (mini (active-minibuffer-window)))
    (select-window mini)
    (pcase-let ((origin (minibuffer-prompt-end))
                (`(,beg . ,end) (grille--boundaries)))
      (delete-region (+ origin beg) (+ (point) end))
      (goto-char (+ origin beg))
      (insert label))))

(defun grille--choose (completion)
  (interactive)
  (grille--insert completion)
  (unless (= (car (grille--boundaries)) ; new boundaries?
             (- (point) (minibuffer-prompt-end)))
    (exit-minibuffer)))

(defun grille--revert (&rest _)
  "Revert the grid completions buffer."
  (while-no-input
   (with-current-buffer "*Grille*"
    (revert-buffer))))

(defun grille--list ()
  "Get list of completions."
  (when-let ((mini (active-minibuffer-window)))
    (with-selected-window mini
      (let* ((all (completion-all-completions
                   (minibuffer-contents)
                   minibuffer-completion-table
                   minibuffer-completion-predicate
                   (- (point) (minibuffer-prompt-end))))
             (last (last all)))
        (when last (setcdr last nil))
        all))))

(defun grille--select-columns ()
  "Compute reasonable number of columns."
  (let ((all (grille--list))
        (width (frame-width)))
    (/ width
       (or (cl-loop repeat grille-limit
                    for entry in all
                    maximize (min (+ (length entry) 3) width))
           width))))
  
(defun grille--set-format ()
  (setq tabulated-list-format
        (apply #'vector
               (cl-loop with width = (/ (frame-width)
                                        grille--columns)
                        repeat grille--columns
                        collect `("Completions" ,width nil)))))

(defun grille--entries ()
  (let ((all (grille--list)))
    (cl-loop repeat grille-limit
             while all
             collect
             (list
              nil
              (apply #'vector
                     (cl-loop repeat grille--columns
                              collect `(,(or (pop all) "")
                                        type grille-button)))))))

(define-derived-mode grille-mode
  tabulated-list-mode "Grid"
  "Grid of completions.")

(let ((map grille-mode-map))
  (define-key map (kbd "<right>") #'forward-button)
  (define-key map (kbd "<left>") #'backward-button)
  (define-key map "s" #'isearch-forward))

(defun grille (&optional columns)
  "Pop up a live-updating grid with current completions."
  (interactive "P")
  (when (minibufferp)
    (with-current-buffer (get-buffer-create "*Grille*")
      (grille-mode)
      (setq-local grille--columns
                  (if columns
                      (prefix-numeric-value columns)
                    (grille--select-columns)))
      (grille--set-format)
      (setq tabulated-list-entries #'grille--entries)
      (setq minibuffer-scroll-window
            (display-buffer (current-buffer)
                            '((display-buffer-reuse-window
                               display-buffer-at-bottom))))
      (while-no-input (tabulated-list-print)))
    (add-hook 'after-change-functions #'grille--revert nil t)))

(defun grille-completing-read (&rest args)
  (run-with-idle-timer 0.05 nil #'grille)
  (apply #'completing-read-default args))

(defun grille-switch-to ()
  "Switch to completions grid."
  (interactive)
  (let ((window (or (get-buffer-window "*Grille*" 0)
                    (progn (grille)
                           (get-buffer-window "*Grille*" 0)))))
    (when window (select-window window))))

(provide 'grille)
