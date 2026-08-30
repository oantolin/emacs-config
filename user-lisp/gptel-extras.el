;;; gptel-extras.el --- Other commands to use LLMs   -*- lexical-binding: t; -*-

(require 'gptel-context)
(require 'gptel-transient)
(require 'markdown-ts-mode)

(defun gptel-extras-mini ()
  "Query an LLM from the minibuffer with output to a new buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "*gptel*")))
    (with-current-buffer buffer (markdown-ts-mode))
    (gptel--suffix-send `("m" ,(concat "b" (buffer-name buffer))))))

(defun gptel-extras-define (term)
  "Use an LLM to define a TERM."
  (interactive "sLookup: ")
  (when (and (string= term "") (null gptel-context))
    (if (use-region-p)
        (setq term (buffer-substring-no-properties
                    (region-beginning) (region-end)))
      (user-error "A term to define is required.")))
  (gptel-request (format "Explain this very briefly: %S" term)
    :transforms gptel-prompt-transform-functions
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
 :name "run_python"
 :confirm t
 :function (lambda (code)
             (let ((command (format "python3 -c %S" code)))
               (shell-command-to-string command)))
 :description "Run some Python code and capture its standard output"
 :args (list '(:name "code"
               :type string
               :description "the Python program to run"))
 :category "computation")

(gptel-make-tool
 :name "eval_elisp"
 :confirm t
 :function (lambda (code) (format "%S" (eval (read code))))
 :description "Evaluate some Emacs Lisp code and get its return value"
 :args (list '(:name "code"
               :type string
               :description "the Emacs Lisp code to evaluate"))
 :category "computation")

(gptel-make-preset 'en
  :system
  "Please translate the text to English; include the name of the original
language in the format '[ORIGINAL_LANGUAGE] TRANSLATED_TEXT'.")

(gptel-make-preset 'es
  :system
  "Por favor, traduzca el texto al español; incluya el nombre del idioma
original en el formato '[IDIOMA_ORIGINAL] TEXTO_TRADUCIDO'.")

(gptel-make-preset 'msc
  :system "What MSC2020 (Mathematics Subject Classifications) would you use for a
talk with the following abstract?")
           
(gptel-make-preset 'py
  :system "Please write and run a Python script to answer this."
  :tools '("run_python"))

(gptel-make-preset 'el
  :system "Please write and evaluate some Emacs Lisp code to do this."
  :tools '("eval_elisp"))

(provide 'gptel-extras)
