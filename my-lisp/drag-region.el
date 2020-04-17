;;; -*- lexical-binding: t; -*-

;; TODO: make it work with rectangular regions

(let (contents reversed-p)
  (defun drag-region--kill ()
    (when (and (not (eq this-command 'drag-region-mode))
               (region-active-p))
      (let ((beg (region-beginning))
            (end (region-end)))
        (setq contents (buffer-substring beg end)
              reversed-p (< beg end))
        (delete-region beg end))))
  (defun drag-region--yank ()
    (when contents
      (set-mark (point))
      (insert contents)
      (setq contents nil)
      (when reversed-p
        (exchange-point-and-mark))
      (setq deactivate-mark nil))))

(define-minor-mode drag-region-mode
  "Drag the region around as you move."
  :lighter " drag"
  (if drag-region-mode
      (progn
        (add-hook 'pre-command-hook #'drag-region--kill)
        (add-hook 'post-command-hook #'drag-region--yank))
    (remove-hook 'pre-command-hook #'drag-region--kill)
    (remove-hook 'post-command-hook #'drag-region--yank)
    (deactivate-mark)))

(provide 'drag-region)
