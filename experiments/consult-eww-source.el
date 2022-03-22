;; consult-eww-source.el --- eww bookmark source for consult-buffer   -*- lexical-binding: t; -*-

(require 'consult)
(require 'eww)

;;; consult-buffer source
;; Taken with very minor modifications from the Consult wiki
(defvar consult--source-eww
  (list
   :name     "Eww"
   :narrow   ?e
   :category 'eww-bookmark
   :action   (lambda (bm)
               (eww-browse-url (get-text-property 0 'url bm)))
   :items    (lambda ()
               (eww-read-bookmarks)
               (mapcar (lambda (bm)
                         (propertize
                          (plist-get bm :title)
                          'url (plist-get bm :url)))
                       eww-bookmarks))))

;;; annotate with URL
(add-to-list 'consult-buffer-sources 'consult--source-eww 'append)

(defun annotate-eww-bookmark (bm)
  (concat
   (propertize " " 'display `(space :align-to (- right 50)))
   (propertize (get-text-property 0 'url bm) 'face 'completions-annotations)))

(defvar marginalia-annotator-registry)
(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotator-registry
               '(eww-bookmark annotate-eww-bookmark builtin none)))

;;; Have Embark treat them as just URLs
(defun transform-eww-bookmark-to-url (target)
  (if (eq (car target) 'eww-bookmark)
      (cons 'url (get-text-property 0 'url (cdr target)))
    target))

(with-eval-after-load 'embark
  (advice-add 'embark--refine-multi-category
              :filter-return #'transform-eww-bookmark-to-url))

(provide 'consult-eww-source)
