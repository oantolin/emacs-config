;;; -*- lexical-binding: t; -*-

(defun gobble-whitespace (skip-chars)
  (lambda (old-fun n &optional killflag)
    (let ((beg (point)))
      (if (or (and (use-region-p) delete-active-region)
              (/= n 1)
              (progn (funcall skip-chars " \t\r\n\v")
                     (= beg (point))))
          (funcall old-fun n killflag)
        (when (eq skip-chars #'skip-chars-backward)
          (goto-char (or (text-property-any (point) beg 'read-only nil) beg)))
        (delete-region beg (point))))))

(defalias 'gobble-whitespace-forward (gobble-whitespace #'skip-chars-forward))
(defalias 'gobble-whitespace-backward (gobble-whitespace #'skip-chars-backward))

(define-minor-mode global-gobble-whitespace-mode
  "Make chacter deletion commands gobble whitespace"
  :global t
  (if global-gobble-whitespace-mode
      (progn
        (advice-add 'delete-forward-char :around #'gobble-whitespace-forward)
        (advice-add 'delete-backward-char :around #'gobble-whitespace-backward))
    (advice-remove 'delete-forward-char #'gobble-whitespace-forward)
    (advice-remove 'delete-backward-char #'gobble-whitespace-backward)))

(provide 'gobble-whitespace)
