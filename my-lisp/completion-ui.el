;;  completion-ui.el --- Switch between vaious completion UIs   -*- lexical-binding: t; -*-

(defun embark-collect-completions-1 (&optional _start _end)
  (unless (bound-and-true-p embark-collect-linked-buffer)
    (embark-collect-completions)))

(defun automatic-embark-completions (fn &rest args)
  "Apply FN to ARGS automatically using Embark to display any completions."
  (minibuffer-with-setup-hook #'embark-collect-completions (apply fn args)))

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
  (advice-remove #'embark-completing-read-prompter
                 #'automatic-embark-completions)
  (advice-remove #'consult-completion-in-region
                 #'automatic-embark-completions)
  ;; activate chosen one
  (pcase ui
    (?d (setq completion-auto-help t))
    ((or ?e ?I ?D)
     (advice-add #'consult-completion-in-region :around
                 #'automatic-embark-completions)
     (advice-add #'embark-completing-read-prompter
                 :around #'automatic-embark-completions)
     (advice-add 'minibuffer-completion-help
                 :override #'embark-collect-completions-1)
     (advice-add 'switch-to-completions
                 :override #'embark-switch-to-collect-completions)
     (if (eq ui ?e)
         (setq completion-auto-help t completion-cycle-threshold nil)
       (add-hook 'minibuffer-setup-hook
                 (if (= ui ?I)
                     #'embark-collect-completions-after-input
                   #'embark-collect-completions-after-delay))))
    (?i (icomplete-mode))
    (?s (selectrum-mode))
    (?v (vertico-mode))
    (?y (ivy-mode))))

(provide 'completion-ui)
