;;; Packages that I am not currently using, but whose configuration I
;;; still sometimes want to load

(use-package grille
  :bind
  (:map minibuffer-local-completion-map
        ("M-q" . grille)
        ("<right>" . grille-forward-char-or-switch-to)
        ("<down>" . grille-switch-to))
  (:map grille-mode-map
        (";" . embark-act)
        ("'" . avy-grille))
  :commands grille-completing-read
  :custom
  (completing-read-function #'grille-completing-read)
  :hook
  (grille-mode
   . (lambda ()
       (let ((buffer (current-buffer))
             (mini (active-minibuffer-window)))
         (when mini
           (with-selected-window mini
             (embark--cache-info buffer)))
         (add-hook 'tabulated-list-revert-hook
                   (lambda ()
                     (setq default-directory
                           (with-selected-window mini
                             (embark--default-directory))))
                   nil t)))))

(use-package live-completions
  :demand t
  :load-path "~/my-elisp-packages/live-completions"
  :bind (:map minibuffer-local-completion-map
              ("C-v" . live-completions-set-columns))
  :config (live-completions-mode))

(use-package icomplete
  :demand t
  :config (icomplete-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
	      ("<up>" . icomplete-backward-completions)
	      ("C-p" . icomplete-backward-completions)
              ("TAB" . minibuffer-force-complete)
              ("C-M-i" . minibuffer-complete)
              ("M-RET" . exit-minibuffer))
  :hook
  (icomplete-minibuffer-setup . visual-line-mode)
  :custom
  (icomplete-show-matches-on-no-input t)
  (icomplete-prospects-height 5)
  (icomplete-separator " ⋮ ")
  (icomplete-hide-common-prefix nil)
  :config
  (advice-add 'icomplete-vertical-minibuffer-teardown
              :after #'visual-line-mode))

(use-package icomplete-vertical
  :ensure t
  :demand t
  ;; :load-path "~/my-elisp-packages/icomplete-vertical"
  :bind (:map icomplete-minibuffer-map
              ("C-v" . icomplete-vertical-toggle))
  :config (icomplete-vertical-mode))

(use-package orderless
  ;; :ensure t
  :load-path "~/my-elisp-packages/orderless"
  :demand t
  :bind (:map minibuffer-local-completion-map ("SPC"))
  :config
  (defun flex-if-star (pattern _i _t)
    (when (string-prefix-p "*" pattern)
      (cons 'orderless-flex (substring pattern 1))))
  (defun regexp-if-equals (pattern _i _t)
    (cond
     ((string-prefix-p "=" pattern)
      (cons 'orderless-regexp (substring pattern 1)))
     ((string-suffix-p "=" pattern)
      (cons 'orderless-regexp (substring pattern 0 -1)))))
  (defun not-containing (literal _i _t)
    (when (string-prefix-p "!" literal)
      (cons
       'orderless-regexp
       (rx-to-string
        `(seq
          (group string-start)
          (zero-or-more
           (or ,@(cl-loop for i from 1 below (length literal)
                          collect `(seq ,(substring literal 1 i)
                                        (or (not ,(aref literal i))
                                            string-end)))))
          string-end)))))
  (defun first-for-symbol (pattern index _t)
    (when (and (zerop index)
               (or (eq minibuffer-completion-table
                       #'help--symbol-completion-table)
                   completion-in-region-mode-predicate
                   (let ((prompt (minibuffer-prompt)))
                     (and prompt (string-match-p "M-x" prompt)))))
      (if (string-match-p "^[[:alnum:]]*$" pattern)
          'orderless-initialism
        'orderless-prefixes)))
  (use-package em-glob :commands eshell-glob-regexp)
  (defun glob-for-files (pattern _i _t)
    (when minibuffer-completing-file-name
      (cons
       'orderless-regexp
       (let ((startp (string-prefix-p "`" pattern))
             (endp (string-suffix-p "'" pattern)))
         (substring (eshell-glob-regexp
                     (substring (replace-regexp-in-string " " "*" pattern)
                                (if startp 1 0)
                                (if endp -1 nil)))
                    (if startp 0 2) (if endp nil -2))))))
  :custom
  (orderless-matching-styles 'orderless-literal)
  (orderless-style-dispatchers '(regexp-if-equals
                                 first-for-symbol
                                 glob-for-files
                                 not-containing
                                 flex-if-star)))

(use-package simple ; defines completion-list-mode
  :bind
  (:map completion-list-mode-map
        ("TAB" . insert-completion))
  :config
  (defun insert-completion (&optional event)
  "Insert current completion into the minibuffer.
If EVENT, use EVENT’s position to determine the starting position."
  (interactive (list last-nonmenu-event))
  (let ((completion-no-auto-exit t))
    (choose-completion event)))
  :hook
  (completion-list-mode . force-truncate-lines))
