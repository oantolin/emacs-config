;;  completion-ui.el --- Switch between vaious completion UIs   -*- lexical-binding: t; -*-

(defun automatic-completions (fn &rest args)
  "Apply FN to ARGS automatically using Embark to display any completions."
  (minibuffer-with-setup-hook #'minibuffer-completion-help (apply fn args)))

(defun change-completion-ui (ui)
  "Choose between Embark, Icomplete, Vertico and Selectrum for completion."
  (interactive
   (list (read-char-choice
          (replace-regexp-in-string
           "_."
           (lambda (x)
             (propertize (substring x 1) 'face '(:foreground "purple")))
           "_default, _mct, _icomplete, _vertico, _selectrum or iv_y? ")
          '(?d ?m ?i ?s ?v ?y))))
  ;; turn off all completion UIs
  (mct-minibuffer-mode -1)
  (icomplete-mode -1)
  (selectrum-mode -1)
  (ivy-mode -1)
  (vertico-mode -1)
  (setq completion-auto-help nil completion-cycle-threshold 3)
  (advice-remove #'embark-completing-read-prompter #'automatic-completions)
  (advice-remove #'consult-completion-in-region #'automatic-completions)
  ;; activate chosen one
  (pcase ui
    (?d
     (setq completion-auto-help t)
     (advice-add #'consult-completion-in-region
                 :around #'automatic-completions)
     (advice-add #'embark-completing-read-prompter
                 :around #'automatic-completions))
    (?m (mct-minibuffer-mode))
    (?i (icomplete-mode))
    (?s (selectrum-mode))
    (?v (vertico-mode))
    (?y (ivy-mode))))

(provide 'completion-ui)
