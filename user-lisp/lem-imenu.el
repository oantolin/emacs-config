;; lem-imenu --- imenu support for lem   -*- lexical-binding: t; -*-

(require 'lem)

(defun lem-imenu--line-at-point ()      ; also defined in shr-heading, dedup?
  "Return the current line."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun lem-imenu--previous-item ()
  "Move point to previous item, return nil if there isn't one."
  (let ((pt (point)))
    (lem-prev-item)
    (/= pt (point))))

;;;###autoload
(defun lem-imenu-setup ()
  "Setup imenu in lem buffers.
Add this function to `lem-mode-hook'."
  (setq-local
   imenu-prev-index-position-function #'lem-imenu--previous-item
   imenu-extract-index-name-function  #'lem-imenu--line-at-point))

(provide 'lem-imenu)
