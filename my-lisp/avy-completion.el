;;; -*- lexical-binding: t; -*-

(defun avy-completion ()
  "Jump to a completion candidate."
  (interactive)
  (let ((wnd (get-buffer-window "*Completions*" 0)))
    (if wnd
        (with-current-buffer "*Completions*"
          (let ((pt (avy-with avy-completion
                      (avy-process
                       (cl-loop initially (goto-char (point-min))
                                until (eobp) do (next-completion 1)
                                collect (cons (point) wnd))))))
            (when (number-or-marker-p pt)
              (goto-char pt)
              (choose-completion))))
      (user-error "No *Completions* windows"))))

(provide 'avy-completion)
