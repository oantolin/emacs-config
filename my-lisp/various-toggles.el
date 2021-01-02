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

(defun change-completion-ui ()
  "Choose between Embark, Icomplete and Selectrum for completion."
  (interactive)
  (icomplete-mode -1)
  (remove-hook 'minibuffer-setup-hook #'embark-live-occur-after-input)
  (selectrum-mode -1)
  (pcase (read-char-choice "Default, Embark, Icomplete or Selectrum? "
                           '(?d ?i ?e ?s))
    (?e (add-hook 'minibuffer-setup-hook #'embark-live-occur-after-input))
    (?i (icomplete-mode))
    (?s (selectrum-mode))))

(provide 'various-toggles)
