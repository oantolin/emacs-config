;;; Packages or keybindings that I am not currently using, but whose
;;; configuration I still sometimes want to load

(bind-keys
 :prefix "C-c f"
 :prefix-map file-ops-map
 :prefix-docstring "Keymap for file operations"
 ("c" . copy-file)
 ("d" . delete-file)
 ("x" . embark-open-externally)
 ("r" . rename-file)
 ("m" . make-directory)
 ("D" . delete-directory)
 ("/" . cd)
 ("." . pwd)
 ("=" . ediff)
 ("b" . byte-compile-file)
 ("B" . byte-recompile-directory))

(bind-keys
 :prefix "C-c f e"
 :prefix-map ediff-ops-map
 :prefix-docstring "Keymap for launching ediff"
 ("f" . ediff-files)
 ("3" . ediff-files3)
 ("d" . ediff-directories)
 ("c" . ediff-current-file)
 ("b" . ediff-buffers))

(bind-keys
 :prefix "C-x p"
 :prefix-map pkg-ops-map
 :prefix-docstring "Keymap for package operations"
 ("l" . list-packages)
 ("n" . package-list-packages-no-fetch)
 ("i" . package-install)
 ("d" . package-delete)
 ("a" . package-autoremove)
 ("g" . package-refresh-contents)
 ("r" . package-reinstall)
 ("h" . describe-package))

(use-package avy-grille
  :bind
  (:map minibuffer-local-completion-map
        ("'" . avy-grille-choose)
        ("\"" . avy-grille-embark-act)))

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

(use-package restricto
  :demand t
  :load-path "~/my-elisp-packages/restricto"
  :bind (:map minibuffer-local-completion-map
              ("SPC" . restricto-narrow)
              ("S-SPC" . restricto-widen))
  :config (restricto-mode))

(use-package regexpect
  :demand t
  :config
  (cl-flet ((string-fix-p (u v)
              (or (string-prefix-p u v) (string-suffix-p u v)))
            (remfix (u v)
              (let ((pre (string-prefix-p u v)))
                (substring v (if pre 1 0) (if pre nil -1))))
            (rx-seq (fmt seq)
              (mapconcat
               (if (stringp fmt) (lambda (x) (format fmt x)) fmt)
               seq ".?*"))
            (format-unless (fmt pat)
              (lambda (x)
                (if (string-match-p pat x) x (format fmt x)))))
    (defun my-regexp-converter (pattern)
      (cond
       ((string-match-p " " pattern)
        (rx-seq (format-unless "\\(%s\\)" "\\\\(")
                (mapcar #'my-regexp-converter (split-string pattern))))
       ((string-fix-p "=" pattern) (regexp-quote (remfix "=" pattern)))
       ((string-fix-p "," pattern) (remfix "," pattern))
       ((string-match-p "^!." pattern)
        (rx-to-string
         `(seq
           (group string-start)         ; highlight nothing!
           (zero-or-more
            (or ,@(cl-loop for i from 1 below (length pattern)
                           collect `(seq ,(substring pattern 1 i)
                                         (or (not (any ,(aref pattern i)))
                                             string-end)))))
           string-end)))
       ((string-match-p "^{.*}$" pattern)
        (rx-seq "\\(%c\\)" (substring pattern 1 -1)))
       ((and minibuffer-completing-file-name
             (string-match-p "[-.]" pattern))
        (rx-seq "\\<\\(%s\\)" (split-string pattern "[-.]" t)))
       ((string-match-p "[/-]" pattern)
        (rx-seq "\\<\\(%s\\)" (split-string pattern "[/-]" t)))
       ((string-fix-p "." pattern)
        (rx-seq "\\<\\(%c\\)" (remfix "." pattern)))
       (t pattern))))
  :custom (regexpect-converter #'my-regexp-converter))
