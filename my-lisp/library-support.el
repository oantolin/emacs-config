;;  library-support.el --- Embark & Marginalia support for elisp libraries   -*- lexical-binding: t; -*-

(require 'marginalia)
(require 'embark)
(require 'lisp-mnt)

(add-to-list 'marginalia-prompt-categories '("\\<[lL]ibrary\\>" . library))

(defun library-summary (library) (lm-summary (ffap-el-mode library)))

(add-to-list 'marginalia-annotator-registry
             '(library library-summary builtin none))

;; override Embark's target finder
(defun embark-target-library-at-point ()
  "Target the Emacs Lisp library name at point."
  (when-let (filename (thing-at-point 'filename))
    (when (ffap-el-mode filename)
      `(library ,filename . ,(bounds-of-thing-at-point 'filename)))))

(embark-define-keymap embark-library-map
  "Keymap for operations on Emacs Lisp libraries."
  ("RET" find-library)
  ("l" load-library)
  ("f" find-library)
  ("h" finder-commentary)
  ("a" apropos-library)
  ("w" locate-library))

(add-to-list 'embark-keymap-alist '(library . embark-library-map))

(provide 'library-support)
