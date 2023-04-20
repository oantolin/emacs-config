;;  embark-this-buffer.el --- Embark whole buffer target & actions   -*- lexical-binding: t; -*-

(require 'embark)

(defun embark-target-this-buffer ()
  (cons 'this-buffer (buffer-name)))

(add-to-list 'embark-target-finders #'embark-target-this-buffer t)

(add-to-list 'embark-keymap-alist '(this-buffer . this-buffer-map))

(push 'embark--allow-edit (alist-get 'write-file embark-target-injection-hooks))

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
  "k" #'kill-buffer
  "z" #'bury-buffer
  "|" #'embark-shell-command-on-buffer
  "g" #'revert-buffer
  "p" #'pwd
  "h" #'mark-whole-buffer
  "<" #'previous-buffer
  ">" #'next-buffer
  "t" #'transpose-windows)

(add-to-list 'embark-repeat-actions #'previous-buffer)
(add-to-list 'embark-repeat-actions #'next-buffer)
(add-to-list 'embark-repeat-actions #'transpose-windows)

(provide 'embark-this-buffer)
