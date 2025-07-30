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
      (funcall
       (buffer-local-value 'comint-input-sender (process-buffer process))
       process input)
    (user-error "No process found")))

(defmacro define-run-command (name cmd &rest args)
  "Define run-NAME as a command that pops to a comint buffer running CMD."
  `(defun ,(intern (format "run-%s" name)) ()
     (interactive)
     ,(format "Run an inferior %s process." cmd)
     (pop-to-buffer (make-comint ,name ,cmd nil ,@args))))

(provide 'comint-extras)
