;;; Packages that I am not currently using, but whose configuration I
;;; still sometimes want to load

(use-package icomplete
  :demand t
  :config (icomplete-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
	      ("<up>" . icomplete-backward-completions)
	      ("C-p" . icomplete-backward-completions)
              ("TAB" . icomplete-force-complete)
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
              :after #'visual-line-mode)
  (unless (fboundp 'icomplete-force-complete)
    (defun icomplete-force-complete ()
      "Complete the icomplete minibuffer."
      (interactive)
      ;; We're not at all interested in cycling here (bug#34077).
      (minibuffer-force-complete nil nil 'dont-cycle))))

(use-package orderless
  :ensure t
  :defer t
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

(use-package icomplete-vertical
  ;; :ensure t
  :demand t
  :load-path "~/my-elisp-packages/icomplete-vertical"
  :bind (:map icomplete-minibuffer-map
              ("C-v" . icomplete-vertical-toggle))
  :config (icomplete-vertical-mode))
