;;; -*- lexical-binding: t; -*-

(require 'avy)
(require 'cl-lib)

(defun avy-completion--candidate (pt)
  "Return the completion candidate at PT."
  (goto-char pt)
  (let ((end (next-single-property-change pt 'mouse-face)))
    (buffer-substring-no-properties pt end)))

(defun avy-action-complete (pt)
  "Enter completion candidate at PT into the minibuffer."
  (let ((where completion-base-position)
        (candidate (avy-completion--candidate pt)))
    (select-window (active-minibuffer-window))
    (apply #'delete-region where)
    (insert candidate)))

(defun avy-action-choose (pt)
  "Choose completion at PT."
  (goto-char pt)
  (choose-completion))

(defun avy-action-save (pt)
  "Save completion candidate at PT in the kill-ring."
  (kill-new (avy-completion--candidate pt))
  (select-window (active-minibuffer-window)))

(defun avy-action-insert (pt)
  "Insert completion candidate at PT into the previous buffer."
  (let ((candidate (avy-completion--candidate pt)))
    (select-window (active-minibuffer-window))
    (with-minibuffer-selected-window
      (when (use-region-p)
        (delete-region (region-beginning) (region-end)))
      (insert candidate))))

(defun avy-completion ()
  "Jump to a completion candidate."
  (interactive)
  (let ((wnd (get-buffer-window "*Completions*" 0)))
    (if wnd
        (with-current-buffer "*Completions*"
          (avy-with avy-completion
            (let ((avy-action #'avy-action-choose)
                  (avy-dispatch-alist '((?c . avy-action-complete)
                                        (?i . avy-action-insert)
                                        (?w . avy-action-save)
                                        (?m . avy-action-goto))))
              (avy-process
               (cl-loop initially (goto-char (point-min))
                        do (next-completion 1) until (eobp)
                        collect (cons (point) wnd))))))
      (user-error "No *Completions* windows"))))

(provide 'avy-completion)
