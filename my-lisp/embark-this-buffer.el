;; -*- lexical-binding: t; -*-

(require 'embark)

(defun embark-target-this-buffer ()
  (cons 'this-buffer (buffer-name)))

(add-to-list 'embark-target-finders #'embark-target-this-buffer 'append)

(add-to-list 'embark-keymap-alist '(this-buffer . this-buffer-map))

(add-to-list 'embark-allow-edit-commands #'write-file)

(defun revert-buffer-no-question (_) (revert-buffer nil t))

(embark-define-keymap this-buffer-map
  "Commands to act on current file or buffer."
  ("RET" eval-buffer)
  ("l" load-file)
  ("b" byte-compile-file)
  ("e" eval-buffer)
  ("r" rename-file)
  ("d" delete-file)
  ("w" write-file)
  ("R" vc-rename-file)
  ("D" vc-delete-file)
  ("n" diff-buffer-with-file)           ; n for new
  ("C-=" ediff-buffers)
  ("=" ediff-files)
  ("!" shell-command)
  ("&" async-shell-command)
  ("x" consult-file-externally)         ; useful for PDFs
  ("c" copy-file)
  ("k" kill-buffer)
  ("z" bury-buffer)
  ("|" embark-shell-command-on-buffer)
  ("g" revert-buffer-no-question)
  ("p" pwd))

(provide 'embark-this-buffer)
