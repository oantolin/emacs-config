;;; eval-region-advice.el --- Advise eval commands to use region if active    -*- lexical-binding: t; -*-

(defun eval-region-if-active (arg)
  "Advice for evaluation commands, to have them call
`eval-region' when the region is activate."
  (when (use-region-p)
    (eval-region (region-beginning) (region-end) arg)
    (deactivate-mark)
    t))

(dolist (fn '(eval-print-last-sexp eval-last-sexp eval-defun))
  (advice-add fn :before-until #'eval-region-if-active))

(provide 'eval-region-advice)
