;;; -*- lexical-binding: t; -*-

(require 'avy)
(require 'grille)

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

(defun avy-grille-command (action dispatch-alist)
  "Jump to a completion candidate."
  (let ((wnd (or (get-buffer-window "*Grille*" 0)
                 (get-buffer-window "*Embark Live Occur*" 0))))
    (if wnd
        (with-current-buffer (window-buffer wnd)
          (avy-with avy-completion
            (let ((avy-action action)
                  (avy-dispatch-alist dispatch-alist))
              (avy-process
               (save-excursion
                 (goto-char (point-min))
                 (let ((btns `((,(point) . ,wnd))))
                   (forward-button 1 t)
                   (while (not (bobp))
                     (push (cons (point) wnd) btns)
                     (forward-button 1 t))
                   (nreverse btns)))))))
      (user-error "No *Grille* or *Embark Live Occur* windows"))))

(defun avy-grille-choose ()
  "Choose a completion candidate."
  (interactive)
  (avy-grille-command #'avy-action-choose
                      '((?c . avy-action-complete)
                        (?x . avy-action-embark-act)
                        (?m . avy-action-goto))))

(defun avy-grille-embark-act ()
  "Act on a completion candidate."
  (interactive)
  (avy-grille-command #'avy-action-embark-act
                      '((?c . avy-action-complete)
                        (?x . avy-action-choose)
                        (?m . avy-action-goto))))

(provide 'avy-grille)
