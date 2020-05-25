;;; -*- lexical-binding: t; -*-

(require 'avy)

(defun avy-action-complete (pt)
  "Enter completion candidate at PT into the minibuffer."
  (grille--insert (button-at pt)))

(defun avy-action-choose (pt)
  "Choose completion at PT."
  (grille--choose (button-at pt)))

(declare-function embark-act "embark")

(defun avy-action-embark-act (pt)
  "Act on the completion at PT."
  (goto-char pt)
  (embark-act))

(defun avy-grille ()
  "Jump to a completion candidate."
  (interactive)
  (let ((wnd (get-buffer-window "*Grille*" 0)))
    (if wnd
        (with-current-buffer "*Grille*"
          (avy-with avy-completion
            (let ((avy-action #'avy-action-choose)
                  (avy-dispatch-alist '((?c . avy-action-complete)
                                        (?x . avy-action-embark-act)
                                        (?m . avy-action-goto))))
              (avy-process
               (save-excursion
                 (goto-char (point-min))
                 (let ((btns `((,(point) . ,wnd))))
                   (forward-button 1 t)
                   (while (not (bobp))
                     (push (cons (point) wnd) btns)
                     (forward-button 1 t))
                   (nreverse btns)))))))
      (user-error "No *Grille* windows"))))

(provide 'avy-grille)
