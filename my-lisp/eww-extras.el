;;; -*- lexical-binding: t -*-

(require 'eww)

;; This function is inspired by Prot's prot-eww-visit-bookmark. The
;; main difference is that this completes bookmark titles, not URLS.
;; The URLs are added as annotations.
(defun eww-bookmark-jump (&optional arg)
  "Prompt for and visit EWW bookmark.

With optional prefix ARG (\\[universal-argument]) open bookmark
in a new EWW buffer."
  (interactive "p")
  (eww-read-bookmarks)
  (let ((bookmarks (mapcar
                    (lambda (bm) (cons (plist-get bm :title)
                                       (plist-get bm :url)))
                    eww-bookmarks)))
    (eww (cdr
          (assoc
           (completing-read
            "Visit EWW bookmark: "
            (lambda (string predicate action)
              (if (eq action 'metadata)
                  `(metadata
                    (category . eww-bookmark-title)
                    (annotation-function
                     . ,(lambda (title) (cdr (assoc title bookmarks)))))
                (complete-with-action action bookmarks string predicate))))
           bookmarks))
         arg)))
