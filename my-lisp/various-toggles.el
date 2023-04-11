;;; various-toggles.el --- Commands to toggle wrapping or ispell language    -*- lexical-binding: t; -*-

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

(defun toggle-tab-bar-visibility ()
  "Toggle tab-bar visibility."
  (interactive)
  (customize-set-variable 'tab-bar-show (not tab-bar-show)))

(provide 'various-toggles)
