;;; -*- lexical-binding: t; -*-

(defun toggle-wrapping ()
  "Toggle both auto fill and visual line modes."
  (interactive)
  (auto-fill-mode 'toggle)
  (visual-line-mode 'toggle))

(defvar ispell-current-dictionary)

(with-eval-after-load 'ispell
  (defun toggle-ispell-lang ()
    "Toggle ispell dictionary between English and Spanish."
    (interactive)
    (ispell-change-dictionary
     (pcase ispell-current-dictionary
       ('nil
        (if (string= (system-name) "penguin") "español" "english"))
       ("english" "español")
       ("español" "english")))))

(defun toggle-completion-ui ()
  "Toggle between embark and icomplete for completion."
  (interactive)
  (if (eq completing-read-function 'embark-completing-read)
      (progn (setq completing-read-function #'completing-read-default)
             (icomplete-mode))
    (setq completing-read-function #'embark-completing-read)
    (icomplete-mode -1)))

(provide 'various-toggles)
