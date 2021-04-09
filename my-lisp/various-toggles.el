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
  "Choose between Embark, Icomplete, Vertico and Selectrum for completion."
  (interactive)
  (icomplete-mode -1)
  (selectrum-mode -1)
  (ivy-mode -1)
  (vertico-mode -1)
  (remove-hook 'minibuffer-setup-hook #'embark-collect-completions-after-input)
  (remove-hook 'minibuffer-setup-hook #'embark-collect-completions-after-delay)
  (let ((ui (read-char-choice
             (replace-regexp-in-string
              "_."
              (lambda (x)
                (propertize (substring x 1) 'face '(:foreground "purple")))
              (concat "_default, embark (after _Input, _Delay), "
                      "_icomplete, _vertico, _selectrum or iv_y? "))
             '(?d ?D ?i ?I ?s ?v ?y))))
    (pcase ui
      ((or ?I ?D)
       (add-hook 'minibuffer-setup-hook
                 (if (= ui ?I)
                     #'embark-collect-completions-after-input
                   #'embark-collect-completions-after-delay)))
      (?i (icomplete-mode))
      (?s (selectrum-mode))
      (?v (vertico-mode))
      (?y (ivy-mode)))))

(provide 'various-toggles)
