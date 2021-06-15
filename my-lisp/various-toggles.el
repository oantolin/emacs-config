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

(defun embark-collect-completions-1 (&optional _start _end)
  (unless (bound-and-true-p embark-collect-linked-buffer)
    (embark-collect-completions)))

(defun change-completion-ui (ui)
  "Choose between Embark, Icomplete, Vertico and Selectrum for completion."
  (interactive
   (list (read-char-choice
          (replace-regexp-in-string
           "_."
           (lambda (x)
             (propertize (substring x 1) 'face '(:foreground "purple")))
           (concat "_default, _embark (after _Input, _Delay), "
                   "_icomplete, _vertico, _selectrum or iv_y? "))
          '(?d ?e ?D ?i ?I ?s ?v ?y))))
  ;; turn off all completion UIs
  (icomplete-mode -1)
  (selectrum-mode -1)
  (ivy-mode -1)
  (vertico-mode -1)
  (remove-hook 'minibuffer-setup-hook #'embark-collect-completions-after-input)
  (remove-hook 'minibuffer-setup-hook #'embark-collect-completions-after-delay)
  (advice-remove 'minibuffer-completion-help #'embark-collect-completions-1)
  (advice-remove 'switch-to-completions #'embark-switch-to-collect-completions)
  (setq completion-auto-help nil completion-cycle-threshold 3)
  ;; activate chosen one
  (pcase ui
    (?d (setq completion-auto-help t))
    (?e
     (setq completion-auto-help t completion-cycle-threshold nil)
     (advice-add 'minibuffer-completion-help
                 :override #'embark-collect-completions-1)
     (advice-add 'switch-to-completions
                 :override #'embark-switch-to-collect-completions))
    ((or ?I ?D)
     (advice-add 'switch-to-completions
                 :override #'embark-switch-to-collect-completions)
     (add-hook 'minibuffer-setup-hook
               (if (= ui ?I)
                   #'embark-collect-completions-after-input
                 #'embark-collect-completions-after-delay)))
    (?i (icomplete-mode))
    (?s (selectrum-mode))
    (?v (vertico-mode))
    (?y (ivy-mode))))

(provide 'various-toggles)
