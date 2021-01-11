;;; -*- lexical-binding: t; -*-

(defun mc-prompt-once-advice (fn &rest args)
  (setq mc--this-command (lambda () (interactive) (apply fn args)))
  (apply fn args))

(defun mc-prompt-once (&rest fns)
  (dolist (fn fns)
    (advice-add fn :around #'mc-prompt-once-advice)))

(defun mc--reset-read-prompts ()
  (setq mc--read-char nil
        mc--read-quoted-char nil
        mc--ivy-completing-read nil))

(defvar mc--ivy-completing-read nil)

(defun mc-support-ivy (fn &rest args)
  (if (not multiple-cursors-mode)
      (apply fn args)
    (unless mc--ivy-completing-read
      (setq mc--ivy-completing-read (apply fn args)))
    mc--ivy-completing-read))

(with-eval-after-load 'ivy
  (advice-add 'ivy-completing-read :around #'mc-support-ivy))

(with-eval-after-load 'misc (mc-prompt-once #'zap-up-to-char))

(with-eval-after-load 'smartparens (mc-prompt-once #'sp-rewrap-sexp))

(defun prompt-insert-string (s)
  "Prompt for a string to insert in the buffer (meant to be used with multiple-cursors)."
  (interactive "MInsert: ")
  (insert s))

(provide 'mc-prompt-once)
