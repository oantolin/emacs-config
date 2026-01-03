;; comint-extras -- Convenience functions for comint    -*- lexical-binding: t; -*-

(require 'comint)

(defun send-to-comint ()
  "Send active region or current line to a process in some window."
  (interactive)
  (if-let* ((process (cl-loop for w in (window-list)
                              thereis (get-buffer-process (window-buffer w))))
            (input (buffer-substring-no-properties
                    (if (use-region-p) (region-beginning) (pos-bol))
                    (if (use-region-p) (region-end) (pos-eol)))))
      (progn
        (setq deactivate-mark t)
        (funcall
         (buffer-local-value 'comint-input-sender (process-buffer process))
         process input))
    (user-error "No process found")))

(defmacro newline-substitution-comint-sender (fmt subst)
  "Return a `comint-input-sender' that replaces newlines with SUBST.
The function also applies format string FMT to the modified input."
  (lambda (process input)
    (thread-last
      input
      (substring-no-properties)
      (format fmt)
      (replace-regexp-in-string "\n" subst)
      (comint-simple-send process))))
                                                     
(cl-defmacro define-run-command (name cmd &key args &key sender &key setup)
  "Define run-NAME as a command that pops to a comint buffer running CMD."
  (declare (indent 2))
  `(defun ,(intern (format "run-%s" name)) ()
     ,(format "Run an inferior %s process." cmd)
     (interactive)
     (pop-to-buffer (apply #'make-comint ,name ,cmd nil ,args))
     ,@(when sender
         `((setq-local comint-input-sender
                       (newline-substitution-comint-sender ,@sender))))
     ,setup))

(provide 'comint-extras)
