;;  embark-this-buffer.el --- Embark whole buffer target & actions   -*- lexical-binding: t; -*-

(require 'embark)

(declare-function 'transpose-windows "ext:window-extras")

(defvar-keymap this-buffer-map
  :doc "Commands to act on current file or buffer."
  :parent embark-general-map
  "RET" #'eval-buffer
  "l" #'load-file
  "b" #'byte-compile-file
  "e" #'eval-buffer
  "r" #'rename-file
  "d" #'delete-file
  "W" #'write-file
  "v r" #'vc-rename-file
  "v d" #'vc-delete-file
  "n" #'diff-buffer-with-file           ; n for new
  "C-=" #'ediff-buffers
  "=" #'ediff-files
  "$" #'ispell
  "!" #'shell-command
  "&" #'async-shell-command
  "x" #'embark-open-externally         ; useful for PDFs
  "c" #'copy-file
  "k" #'kill-current-buffer
  "z" #'bury-buffer
  "q" #'quit-window
  "|" #'embark-shell-command-on-buffer
  "g" #'revert-buffer
  "p" #'pwd
  "h" #'mark-whole-buffer
  "<" #'previous-buffer
  ">" #'next-buffer
  "t" #'transpose-windows)

(cl-pushnew 'embark--allow-edit (alist-get 'write-file embark-target-injection-hooks))

(dolist (cmd '(previous-buffer next-buffer transpose-windows))
  (cl-pushnew cmd embark-repeat-actions))

(defun embark-on-this-buffer ()
  "Run `embark-act' on the current buffer."
  (interactive)
  (let ((embark-target-finders
         (list (lambda () (cons 'this-buffer (buffer-name)))))
        (embark-keymap-alist '((this-buffer . this-buffer-map))))
    (embark-act)))

(provide 'embark-this-buffer)
