;;  embark-imenu-collector.el --- Have Embark collect imenu items   -*- lexical-binding: t; -*-

(require 'embark)
(require 'consult-imenu)

(defun collect-imenu-items ()
  (cons 'imenu
        (mapcar (pcase-lambda (item)
                  (add-text-properties 0 1 (list 'data item) (car item))
                  (car item))
                (consult-imenu--items))))

(defun jump-to-imenu-item (item)
  (consult-imenu--jump (get-text-property 0 'data item)))

(add-to-list 'embark-candidate-collectors #'collect-imenu-items t)

(add-to-list 'embark-default-action-overrides '(imenu . jump-to-imenu-item))

(provide 'embark-imenu-collector)
