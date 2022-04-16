;;; cicio-mode.el --- Major mode for cicio programs    -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.ci\\'" . cicio-mode))

(require 'clojure-mode)

(defconst cicio-font-lock-keywords
  (let ((keywords (copy-tree clojure-font-lock-keywords)))
    (setf (elt keywords 2)
          `(,(concat "(\\(?:[a-z\.-]+/\\)?\\(def\[a-z\-*!?\]*-?\\)"
                     ;; Function declarations
                     "\\>"
                     ;; Any whitespace
                     "[ \r\n\t]*"
                     ;; Possibly type or metadata
                     "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                     "\\(\\sw+\\)?")
            (1 font-lock-keyword-face)
            (2 font-lock-function-name-face nil t)))
    keywords))

(defvar cicio-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'cicio-to-lua)
    (define-key map (kbd "C-c C-c") #'run-cicio)
    (define-key map (kbd "C-c C-r") #'cicio-runfile)
    (define-key map (kbd "C-M-x") #'cicio-eval-defun)
    (define-key map (kbd "C-M-q") #'indent-pp-sexp)
    map)
  "Keymap for Cicio mode.")

(defvar inferior-cicio-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") #'inferior-cicio-runfile)
    (define-key map (kbd "C-c C-s") #'inferior-cicio-switch-to-source)
    (define-key map (kbd "C-M-q") #'indent-pp-sexp)
    map)
  "Keymap for Inferior Cicio mode.")

(define-derived-mode cicio-mode clojure-mode "Cicio"
  "Major mode for editing Cicio programs.

\\{cicio-mode-map}"
  (dolist (ind-forms '((nil def)
                       (1 unless repeat-until using)
                       (2 fold loop)))
    (let ((ind (car ind-forms)))
      (dolist (form (cdr ind-forms))
        (put-clojure-indent form ind))))
  (setf (car font-lock-defaults) 'cicio-font-lock-keywords))

(define-derived-mode inferior-cicio-mode comint-mode "Inferior Cicio"
  "Major mode for interacting with an inferior Cicio interpreter."
  (set-syntax-table clojure-mode-syntax-table)
  (clojure-font-lock-setup)
  (setf (car font-lock-defaults) 'cicio-font-lock-keywords))

(defun cicio-to-lua (arg)
  "Convert current buffer to Lua. With prefix arg, skip indenting."
  (interactive "P")
  (save-buffer)
  (shell-command (format "cicio -d %s" buffer-file-name) "*cicio-to-lua*")
  (switch-to-buffer-other-window "*cicio-to-lua*")
  (lua-mode)
  (unless arg (indent-region (point-min) (point-max))))

(defun cicio-runfile ()
  "Switch to REPL and reload file"
  (interactive)
  (save-buffer)
  (let ((buffer (current-buffer)))
    (run-cicio)
    (setq-local cicio-source-buffer buffer)
    (inferior-cicio-runfile)))

(defun inferior-cicio-runfile ()
  "(Re)load associated source file"
  (interactive)
  (goto-char (point-max))
  (if (boundp 'cicio-source-buffer)
      (let ((file (buffer-file-name cicio-source-buffer)))
        (comint-send-string "*cicio*"
         (format "(runfile \"%s\")(io.write \";; loaded %s\")nil\n"
                 file (file-name-nondirectory file))))
    (message "No associated source buffer")))

(defun inferior-cicio-switch-to-source ()
  "Switch to associated source buffer"
  (interactive)
  (if (boundp 'cicio-source-buffer)
      (switch-to-buffer cicio-source-buffer)
    (message "No associated source buffer")))

(defun cicio-eval-defun ()
  "Send the top-level form containing point to cicio."
  (interactive)
  (save-excursion
    (when (beginning-of-defun)
      (let ((beg (point)))
	(end-of-defun)
	(comint-send-region "*cicio*" beg (point))))))

(defun run-cicio ()
  "Run an inferior cicio process, input and output via buffer `*cicio*'."
  (interactive)
  (switch-to-buffer (make-comint "cicio" "cicio"))
  (inferior-cicio-mode))

(when (featurep 'smartparens)
  (sp-with-modes '(cicio-mode inferior-cicio-mode)
    (sp-local-pair "`" "`" :actions nil)))

(provide 'cicio-mode)
