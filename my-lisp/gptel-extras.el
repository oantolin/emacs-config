;;; gptel-extras.el --- Other commands to use LLMs   -*- lexical-binding: t; -*-

(require 'gptel-context)
(require 'gptel-transient)

(defun gptel-extras-mini ()
  "Display the LLM's response to PROMPT in echo area."
  (interactive)
  (gptel--suffix-send '("m" "e")))

(defun gptel-extras-define (term)
  "Use an LLM to define a TERM."
  (interactive "sLookup: ")
  (when (string= term "") (user-error "A term to define is required."))
  (gptel-request (format "Explain this very briefly: %S" term)
    :callback
    (lambda (response info &optional _raw)
      (pcase response
        ((pred stringp) (message "%s" response))
        (`(tool-call . ,calls) (gptel--display-tool-calls calls info t))
        (`(tool-result . ,results) (gptel--display-tool-results results info))
        (`(reasoning . ,step) (gptel--display-reasoning-stream step info))
        (_ (when (and (null response) (plist-get info :error))
             (message "response error: %s" (plist-get info :status))))))))

(gptel-make-tool
 :name "run_python_code"
 :confirm t
 :function (lambda (code)
             (let ((command (format "python3 -c %S" code)))
               (shell-command-to-string command)))
 :description "Run some Python code and capture its standard output"
 :args (list '(:name "code"
               :type string
               :description "the Python program to run"))
 :category "computation")

(gptel-make-preset "translate"
  :system (concat
           "Please translate the text; include the name of the "
           "original language in the format "
           "'[ORIGINAL_LANGUAGE] TRANSLATION'."))

(gptel-make-preset "python"
  :system "If necessary, please write and run a Python script to answer this."
  :tools '("run_python_code"))

(provide 'gptel-extras)
