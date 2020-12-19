;;; -*- lexical-binding: t; -*-

(defun toggle-wrapping ()
  "Toggle both auto fill and visual line modes."
  (interactive)
  (auto-fill-mode 'toggle)
  (visual-line-mode 'toggle))

(defvar ispell-current-dictionary)

(defun toggle-ispell-lang ()
  "Toggle ispell dictionary between English and Spanish."
  (interactive)
  (require 'ispell)
  (ispell-change-dictionary
   (pcase ispell-current-dictionary
     ('nil
      (if (string= (system-name) "penguin") "español" "english"))
     ("english" "español")
     ("español" "english"))))

(defun toggle-completion-ui ()
  "Toggle between embark and icomplete for completion."
  (interactive)
  (if (bound-and-true-p icomplete-mode)
      (progn (icomplete-mode -1)
             (add-hook 'minibuffer-setup-hook #'embark-live-occur-after-input))
    (remove-hook 'minibuffer-setup-hook #'embark-live-occur-after-input)
    (icomplete-mode)))

(provide 'various-toggles)
