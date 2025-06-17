;;; gptel-extras.el --- Other commands to use LLMs   -*- lexical-binding: t; -*-

(require 'gptel-context)

(defun gptel-extras--show-response (response info)
  "Display RESPONSE in the echo area or a buffer, depending on length.

For use as a `:callback' with `gptel-request'."
  (cond
   ((not response)
    (message "gptel failed with status %s and error message:\n%S"
             (plist-get info :status)
             (plist-get (plist-get info :error) :message)))
   ((< (length response) 250)
    (message "%s" response))
   (t (pop-to-buffer (get-buffer-create "*gptel-mini*"))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert response))
      (goto-char (point-min))
      (when (fboundp 'markdown-mode) (markdown-mode))
      (view-mode))))

(defun gptel-extras-mini (prompt)
  "Display the LLM's response to PROMPT.
If the region is active, it is included as context. If the response is
short, it is shown in the echo area; otherwise, it is displayed in a
buffer."
  (interactive "sAsk LLM: ")
  (when (string= prompt "") (user-error "A prompt is required."))
  (let (gptel-include-reasoning)
    (gptel-request
        (if (use-region-p)
            (concat prompt "\n\n" (buffer-substring-no-properties
                                   (region-beginning) (region-end)))
          prompt)
      :transforms '(gptel--transform-add-context)
      :callback #'gptel-extras--show-response)))

(defun gptel-extras-define (term)
  "Use an LLM to define a TERM."
  (interactive "sLookup: ")
  (when (string= term "") (user-error "A term to define is required."))
  (let (gptel-include-reasoning)
    (gptel-request (format "Explain this very briefly: %S" term)
      :callback #'gptel-extras--show-response)))

(gptel-make-tool
 :name "run_python_code"
 :function (lambda (code)
             (let ((command (format "python3 -c %S" code)))
               (shell-command-to-string command)))
 :description "Run some Python code and capture its standard output"
 :args (list '(:name "code"
               :type string
               :description "the Python program to run"))
 :category "computation")

(provide 'gptel-extras)
