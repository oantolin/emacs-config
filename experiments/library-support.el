;;  library-support.el --- Embark & Marginalia support for elisp libraries   -*- lexical-binding: t; -*-

(require 'marginalia)
(require 'embark)
(require 'lisp-mnt)

(add-to-list 'marginalia-prompt-categories '("\\<[lL]ibrary\\>" . library))

(defvar library-summary-cache (make-hash-table :test #'equal))

(defun library-summary (library)
  (setq library (string-remove-suffix ".elc" library))
  (or (gethash library library-summary-cache)
      (setf (gethash library library-summary-cache)
            (when-let ((path (ffap-el-mode library))
                       (summary (let ((inhibit-message t)) (lm-summary path))))
              (concat (propertize " " 'display '(space :align-to (- right 60)))
                      (propertize summary 'face 'completions-annotations))))))

(add-to-list 'marginalia-annotator-registry
             '(library library-summary builtin none))

;; override Embark's target finder
(defun embark-target-library-at-point ()
  "Target the Emacs Lisp library name at point."
  (when-let (filename (thing-at-point 'filename))
    (when (ffap-el-mode filename)
      `(library ,filename . ,(bounds-of-thing-at-point 'filename)))))

(defvar-keymap embark-library-map
  :doc "Keymap for operations on Emacs Lisp libraries."
  "RET" #'find-library
  "l" #'load-library
  "f" #'find-library
  "h" #'finder-commentary
  "a" #'apropos-library
  "w" #'locate-library)

(add-to-list 'embark-keymap-alist '(library . embark-library-map))

(provide 'library-support)
