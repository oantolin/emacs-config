;;; -*- lexical-binding: t; -*-

(defgroup completing-history nil
  "Insert history items chosen with completion."
  :group 'completion)

(defcustom completing-history-input-rings
  '((eshell-mode . eshell-history-ring)
    (comint-mode . comint-input-ring)
    (term-mode   . term-input-ring))
  "Alist of (mode . ring) pairs of input rings."
  :type '(list (cons symbol symbol))
  :group 'completing-history)

(defun completing-history--items-for-buffer ()
  "Get history relevant for current buffer."
  (if (minibufferp)
      (if (version< emacs-version "27")
          (symbol-value minibuffer-history-variable)
        (minibuffer-history-value))
    (cl-loop
     for (mode . ring) in completing-history-input-rings
     when (and (boundp ring) (derived-mode-p mode))
     return (ring-elements (symbol-value ring)))))

(defun completing-history-insert-item ()
  "Insert an item from history, selected with completion."
  (interactive)
  (let ((item (let ((enable-recursive-minibuffers t))
                (completing-read
                 "Item: " (completing-history--items-for-buffer) nil t))))
    (when (minibufferp)
      (delete-minibuffer-contents))
    (when item
      (let ((inhibit-read-only t))
        (insert item)))))

(defcustom completing-history-keymaps
  `((minibuffer . minibuffer-local-map)
    ,(if (version< emacs-version "27")
         '(esh-mode . eshell-mode-map)
       '(em-hist . eshell-hist-mode-map))
    (shell . shell-mode-map)
    (term . term-mode-map)
    (term . term-raw-map)
    (comint . comint-mode-map)
    (sly . sly-mrepl-mode-map))
  "Alist of (mode . keymap) pairs where M-r should insert history items."
  :type '(list (cons symbol symbol))
  :group 'completing-history)

(defcustom completing-history-binding "M-r"
  "Keybinding to use for `completing-history-insert-item'."
  :type 'string
  :group 'completing-history)

(defcustom completing-history-unbind-M-s t
  "Unbind M-s in the keymaps where we bind `completing-history-insert-item'?"
  :type 'boolean
  :group 'completing-history)

(defun completing-history-setup-keybinding ()
  "Free M-s and bind M-r to do history completion in various modes."
  (cl-loop   
   for (feature . keymap) in completing-history-keymaps
   for body = `((define-key ,keymap (kbd ,completing-history-binding)
                  #'completing-history-insert-item)
                ,@(when completing-history-unbind-M-s
                    `((define-key ,keymap (kbd "M-s") nil))))
   do (eval-after-load feature
        (if (and (version< emacs-version "27") (eq feature 'esh-mode))
            `(add-hook 'eshell-mode-hook (lambda () ,@body))
          `(progn ,@body)))))

(provide 'completing-history)
