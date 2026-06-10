;; embark-elpher --- elpher link target for embark   -*- lexical-binding: t; -*-

(require 'elpher)

(defun embark-target-elpher-links ()
  (when (derived-mode-p 'elpher-mode)
    (when-let* ((url (get-text-property (point) 'elpher-page)))
      `(url ,(url-recreate-url (cadr url))
            ,(previous-single-property-change
              (min (1+ (point)) (point-max)) 'elpher-page nil (point-min))
            . ,(next-single-property-change
                (point) 'elpher-page nil (point-max))))))

(cl-pushnew #'embark-target-elpher-links embark-target-finders)

(provide 'embark-elpher)
