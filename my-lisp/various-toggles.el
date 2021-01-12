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

(defun embark-collect-completions-1 (_start _end)
  (embark-collect-completions))

(defun change-completion-ui ()
  "Choose between Embark, Icomplete and Selectrum for completion."
  (interactive)
  (icomplete-mode -1)
  (remove-hook 'minibuffer-setup-hook #'embark-collect-completions-after-input)
  (remove-hook 'minibuffer-setup-hook #'embark-collect-completions-after-delay)
  (defalias 'minibuffer-completion-help 'original-minibuffer-completion-help)
  (defalias 'switch-to-completions 'original-switch-to-completions)
  (setq completion-auto-help nil)
  (selectrum-mode -1)
  (let ((ui (read-char-choice
             "[d]efault, Embark ([e]asy, [I]nput, [D]elay), [i]complete or [s]electrum? "
             '(?d ?D ?i ?I ?e ?s))))
    (pcase ui
      ((or ?I ?D)
       (add-hook 'minibuffer-setup-hook
                 (if (= ui ?I)
                     #'embark-collect-completions-after-input
                   #'embark-collect-completions-after-delay)))
      (?e
       (setq completion-auto-help t)
       (advice-add 'minibuffer-completion-help
                   :override 'embark-collect-completions-1)
       (advice-add 'switch-to-completions
                   :override 'embark-switch-to-collect-completions))
      (?i (icomplete-mode))
      (?s (selectrum-mode)))))

(provide 'various-toggles)
