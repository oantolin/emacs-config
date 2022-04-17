;;; j-mode.el --- Communicate with an inferior J   -*- lexical-binding: t; -*-

(defgroup j-mode nil "Major mode for J programs"
  :group 'languages)

(defcustom j-mode-interpreter
  (or (executable-find "jconsole") (executable-find "ijconsole"))
  "Name of J interpreter"
  :type 'string)

(defvar-local j-mode-source-buffer nil
  "Buffer from which this inferior J buffer was started.")

(defvar j-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'j-mode-load-file)
    (define-key map (kbd "C-c C-c") #'j-mode-eval)
    (define-key map (kbd "C-c C-s") #'run-j)
    map)
  "Keymap for J mode")

(defvar inferior-j-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'j-mode-load-file)
    (define-key map (kbd "C-c C-s") #'j-mode-switch-to-source)
    map)
  "Keymap for J mode")

(define-derived-mode j-mode prog-mode "J"
  "Major mode for editing J programs

\\{j-mode-map}"
  ;; LOTS of unbalanced delimiters
  (setq-local comment-start "NB. ")
  (electric-pair-local-mode -1))

(define-derived-mode inferior-j-mode comint-mode "Inferior J"
  "Major mode for interacting with an inferior J interpreter."
  ;; LOTS of unbalanced delimiters
  (electric-pair-local-mode -1))

(defun j-mode-load-file ()
  "(Re)load J source file."
  (interactive)
  (save-excursion (run-j))
  (if-let ((file (buffer-file-name
                  (or j-mode-source-buffer (current-buffer)))))
      (comint-send-string "*J*" (format "load '%s'\n" file))
    (user-error "No associated source buffer")))

(defun j-mode-eval (arg)
  "Evaluate some portion of the buffer in inferior J process.
If the region is active, evaluate that.  With no active region
evaluate the current paragraph by default, or the current line
with one universal argument, or the entire buffer with two."
  (interactive "P")
  (save-excursion (run-j))
  (apply #'comint-send-region "*J*"
         (cond
          ((use-region-p) (list (region-beginning) (region-end)))
          ((equal arg '(4))
           (list (line-beginning-position) (line-end-position)))
          ((equal arg '(16)) (list (point-min) (point-max)))
          (t (save-excursion
               (list (progn (backward-paragraph) (point))
                     (progn (forward-paragraph)  (point)))))))
  (with-current-buffer "*J*" (comint-send-input)))

(defun run-j ()
  "Run an inferior J process in buffer *J*."
  (interactive)
  (let ((buffer (and (derived-mode-p 'j-mode) (current-buffer))))
    (pop-to-buffer (make-comint "J" j-mode-interpreter))
    (unless (derived-mode-p 'inferior-j-mode)
      (inferior-j-mode))
    (when buffer (setq j-mode-source-buffer buffer))))

(defun j-mode-switch-to-source ()
  "Switch to associated J mode buffer."
  (interactive)
  (if j-mode-source-buffer
      (pop-to-buffer j-mode-source-buffer)
    (user-error "No associated source buffer.")))

(provide 'j-mode)
