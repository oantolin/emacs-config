;;; -*- lexical-binding: t -*-

;;; customize thinks it knows better than me

(setq custom-file (make-temp-file "emacs-custom-"))

;;; GUI

(custom-set-variables
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(ring-bell-function #'ignore))

(when (string= (system-name) "penguin") ; Chromebook
  (set-face-attribute 'default nil :height 110)
  (define-key key-translation-map (kbd "<next>") (kbd "<M-down>"))
  (define-key key-translation-map (kbd "<prior>") (kbd "<M-up>")))

(custom-set-faces
 `(variable-pitch ((t :family "DejaVu Serif")))
 '(Info-quoted ((t (:inherit fixed-pitch))))
 `(fixed-pitch ((t :family ,(face-attribute 'default :family))))
 '(fringe ((t :background nil))))

;;; package.el & use-package setup

(custom-set-variables
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-enable-at-startup nil))
(require 'package)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(custom-set-variables
 '(use-package-enable-imenu-support t))

(eval-when-compile (require 'use-package))

(use-package diminish :ensure t)

(use-package gcmh :ensure t :diminish :config (gcmh-mode))

(use-package bind-key
  :bind ("C-h y" . describe-personal-keybindings))

(add-to-list 'load-path "~/.emacs.d/my-lisp/")
(add-to-list 'load-path "~/.private/")

;;; misc

(let ((home-bin (expand-file-name "bin" (getenv "HOME")))
      (path (getenv "PATH")))
  (unless (string-prefix-p home-bin path)
    (setenv "PATH" (concat home-bin ":" path))
    (add-to-list 'exec-path home-bin)))

(dolist (cmd '(narrow-to-region
               dired-find-alternate-file
               LaTeX-narrow-to-environment
               TeX-narrow-to-group
               narrow-to-page
               set-goal-column
               scroll-left
               scroll-right))
  (put cmd 'disabled nil))
(put 'suspend-frame 'disabled t)

(custom-set-variables
 '(set-mark-command-repeat-pop t)
 '(current-language-environment "UTF-8")
 '(after-save-hook '(executable-make-buffer-file-executable-if-script-p))
 '(column-number-indicator-zero-based nil)
 '(scroll-preserve-screen-position t)
 '(make-backup-files nil)
 '(sentence-end-double-space nil)
 '(words-include-escapes t)
 '(indent-tabs-mode nil)
 '(standard-indent 2)
 '(track-eol t)
 '(text-mode-hook '(turn-on-auto-fill text-mode-hook-identify))
 '(view-read-only t)
 '(kill-read-only-ok t)
 '(history-delete-duplicates t)
 '(kill-do-not-save-duplicates t)
 '(save-interprogram-paste-before-kill t)
 '(password-cache-expiry 300)
 '(debugger-stack-frame-as-list t)
 '(split-width-threshold 140)
 '(bookmark-default-file "~/.private/bookmarks"))

(defalias 'yes-or-no-p #'y-or-n-p)

(bind-keys
 ("C-:" . eval-print-last-sexp)
 ("C-d" . delete-forward-char)
 ("M-K" . kill-paragraph)
 ("M-Z" . zap-to-char)
 ("M-o" . other-window)
 ("C-x C-p" . proced)
 ("C-x c" . set-goal-column)
 ("C-x k" . kill-current-buffer)
 ("C-x K" . kill-buffer)
 ("C-x C-d" . dired)
 ("C-x d" . list-directory)
 ([remap list-buffers] . electric-buffer-list)
 ([remap upcase-word] . upcase-dwim)
 ([remap downcase-word] . downcase-dwim)
 ([remap capitalize-word] . capitalize-dwim)
 ([remap just-one-space] . cycle-spacing)
 ([remap count-words-region] . count-words)
 ("C-M-o" . up-list)
 ((if (string= (system-name) "penguin") "<C-delete>" "<C-M-backspace>") .
  ;; Alt+backspace sends <delete> on the Chromebook...  
  kill-backward-up-list)
 ("M-R" . raise-sexp)
 ("M-E" . mark-end-of-sentence)
 ("M-T" . transpose-sentences)
 ("C-x M-t" . transpose-paragraphs)
 ("C-S-s" . forward-same-syntax)
 ("M-p" . previous-error)
 ("M-n" . next-error)
 ([remap apropos-command] . apropos)
 ;; The Chromebook has a pretty reload key!
 ("<XF86Reload>" . revert-buffer))

(when (string= (system-name)  "penguin")
  ;; Alt+backspace sends <delete> on the Chromebook...
  (bind-key "<delete>" #'backward-kill-word))

(bind-keys
 :prefix "C-c f"
 :prefix-map file-ops-map
 :prefix-docstring "Keymap for file operations"
 ("c" . copy-file)
 ("d" . delete-file)
 ("x" . open-externally)
 ("r" . rename-file)
 ("m" . make-directory)
 ("D" . delete-directory)
 ("/" . cd)
 ("." . pwd)
 ("b" . byte-recompile-file)
 ("B" . byte-recompile-directory)
 ("e" . ediff)
 ("E" . ediff-directories)
 ("g" . ediff-current-file)
 ("C-e" . ediff-buffers))

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

(bind-keys
 :prefix "C-c t"
 :prefix-map toggle-map
 :prefix-docstring "Keymap for commands that toggle various settings."
 ("c" . column-number-mode)
 ("d" . toggle-debug-on-error)
 ("t" . toggle-truncate-lines)
 ("s" . whitespace-mode)
 ("v" . variable-pitch-mode)
 ("o" . org-toggle-link-display)
 ("b" . toggle-my-theme))

(bind-keys
 :prefix "C-c l"
 :prefix-map lib-ops-map
 :prefix-docstring "Keymap for operations on Emacs Lisp libraries."
 ("l" . load-library)
 ("f" . find-library)
 ("b" . eval-buffer)
 ("c" . byte-compile-file)
 ("r" . byte-recompile-file)
 ("a" . apropos-library)
 ("w" . locate-library))

;;; packages

(use-package modus-operandi-theme
  :ensure t
  :defer t
  :custom
  (modus-operandi-theme-slanted-constructs t)
  (modus-operandi-theme-bold-constructs t))

(use-package modus-vivendi-theme
  :ensure t
  :defer t
  :custom
  (modus-vivendi-theme-slanted-constructs t)
  (modus-vivendi-theme-bold-constructs t))

(load-theme 'modus-operandi t (not (display-graphic-p)))
(load-theme 'modus-vivendi t (display-graphic-p))

(use-package imenu
  :bind ("C-c i" . imenu)
  :custom (imenu-space-replacement nil))

(use-package flimenu
  :ensure t
  :after imenu
  :config (flimenu-global-mode))

(use-package misc
  :bind
  ("M-z" . zap-up-to-char)
  ("M-F" . forward-to-word)
  ("M-B" . backward-to-word))

(use-package text-extras
  :bind
  ("M-Q" . unfill-paragraph)
  ("C-\"" . copy-word-from-above)
  ("M-L" . mark-line)
  ("M-C" . mark-char)
  ("M-@" . mark-my-word)
  ("C-c A" . align-matches)
  ("M-g r" . goto-random-line)
  ("M-g M-r" . goto-random-line)
  ("M-g s" . goto-matching-line)
  ("M-g M-s" . goto-matching-line)
  ("C-M-y" . completing-yank)
  ("C-M--" . kill-inside-sexp)
  ("C-M-=" . mark-inside-sexp)
  ("M-U" . unwrap-sexp)
  ("M-S" . unwrap-mark-sexp)
  ("C-|" . pipe-region)
  ("C-S-w" . forward-to-whitespace)
  ("C-S-r" . backward-to-whitespace)
  ("M-W" . mark-non-whitespace)
  :commands force-truncate-lines)

(use-package placeholder
  :load-path "~/my-elisp-packages/placeholder"
  :bind
  ("M-_" . placeholder-insert)
  ("C-S-n" . placeholder-forward)
  ("C-S-p" . placeholder-backward))

(use-package isearch-extras
  :bind
  (:map isearch-mode-map
        ("<S-return>" . isearch-exit-at-start)
        ("<C-return>" . isearch-kill-and-exit)
        ("<M-return>" . isearch-save-and-exit)))

(use-package math-delimiters
  :load-path "~/my-elisp-packages/math-delimiters"
  :bind
  (:map toggle-map
        ("m" . math-delimiters-toggle))
  :commands
  math-delimters-no-dollars
  math-delimiters-insert)

(use-package block-undo)

(use-package help-extras
  :bind ("C-h M" . describe-keymap)
  :commands cotd)

(use-package epithet
  :load-path "~/my-elisp-packages/epithet"
  :bind ("C-x B" . epithet-rename-buffer))

(use-package various-toggles
  :bind
  (:map toggle-map
        ("w" . toggle-wrapping)
        ("l" . toggle-ispell-lang)
        ("b" . toggle-my-theme)))

(use-package window-extras
  :bind
  (:map ctl-x-4-map
        ("s" . toggle-window-split)
        ("t" . transpose-windows)))

(use-package completing-history
  :demand t
  :config (completing-history-setup-keybinding))

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

(use-package minibuffer
  :bind
  (:map minibuffer-local-completion-map
        ("TAB" . minibuffer-force-complete)
        ("<backtab>" . minibuffer-complete)
        ("M-RET" . exit-minibuffer)
        ("C-j" . minibuffer-force-complete-and-exit)
        ("SPC") ("?"))
  (:map minibuffer-local-must-match-map
        ("C-j" . minibuffer-force-complete-and-exit))
  :custom
  (completion-auto-help nil)
  (completion-show-help nil)
  (completion-styles '(regexpect))
  (completion-category-defaults nil)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (completions-format 'vertical)
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-electric-default-mode t)
  (minibuffer-eldef-shorten-default t)
  (resize-mini-windows t))

(use-package minibuffer-extras
  :bind
  (:map minibuffer-local-filename-completion-map
        ("<C-backspace>" . up-directory)
        ("C-c C-d" . cd-bookmark))
  (:map minibuffer-local-map
        :prefix "M-m"
        :prefix-map minibuffer-ops-map
        :prefix-docstring "Keymap for common minibuffer operations" 
        ("i" . insert-minibuffer-contents)
        ("w" . exit-minibuffer-save-contents)
        ("r" . insert-region-in-minibuffer)
        ("s" . schedule-for-next-minibuffer))
  :commands completing-read-in-region
  :custom
  (completion-in-region-function #'completing-read-in-region))

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
  (use-package em-glob :commands eshell-glob-regexp)
  (cl-flet ((string-fix-p (u v)
              (or (string-prefix-p u v) (string-suffix-p u v)))
            (remfix (u v)
              (let ((pre (string-prefix-p u v)))
                (substring v (if pre 1 0) (if pre nil -1)))))
    (defun my-regexp-converter (pattern)
      (cond
       ((string-prefix-p "`" pattern)
        (concat "\\`" (my-regexp-converter (substring pattern 1))))
       ((string-suffix-p "'" pattern)
        (concat
         (my-regexp-converter (substring pattern 0 -1))
         (if minibuffer-completing-file-name "\\(?:\\'\\|/\\)" "\\'")))
       ((string-fix-p "=" pattern) (regexp-quote (remfix "=" pattern)))
       ((string-fix-p ";" pattern) (remfix ";" pattern))
       ((string-prefix-p "!" pattern)
        (rx-to-string
         `(seq
           (group string-start)         ; highlight nothing!
           (zero-or-more
            (or ,@(cl-loop for i from 1 below (length pattern)
                           collect `(seq ,(substring pattern 1 i)
                                         (or (not ,(aref pattern i))
                                             string-end)))))
           string-end)))
       ((string-match-p "^{.*}$" pattern)
        (mapconcat
         (lambda (ch) (rx (group (literal (string ch)))))
         (substring pattern 1 -1)
         ".*?"))
       ((string-match-p ".[/-]\\|[/-]." pattern)
        (mapconcat
         (lambda (str) (rx  (group (literal str))))
         (split-string pattern "\\>" t) ".*"))
       ((or minibuffer-completing-file-name
            (eq major-mode 'eshell-mode))
        (mapconcat (lambda (str) (rx (group (regexp str))))
                   (split-string (substring (eshell-glob-regexp pattern) 2 -2))
         ".*?"))
       ((string-fix-p "." pattern)
        (mapconcat
         (lambda (ch) (rx bow (group (literal (string ch)))))
         (remfix "." pattern)
         ".*?"))
       (t pattern))))
  :custom (regexpect-converter #'my-regexp-converter))

(use-package live-completions
  :demand t
  :load-path "~/my-elisp-packages/live-completions"
  :bind (:map minibuffer-local-completion-map
              ("C-v" . live-completions-set-columns))
  :config (live-completions-mode))

(use-package avy-completion
  :bind (:map minibuffer-local-completion-map
              ("M-'" . avy-completion)))

(use-package gobble-whitespace
  :config (global-gobble-whitespace-mode))

(use-package tmp-buffer
  :bind ("C-c n" . tmp-buffer))

(use-package eval-region-advice)

(use-package narrow-extras
  :bind
  (:map ctl-x-map
        ("C-n" . narrow-or-widen-dwim))
  (:map narrow-map
        ("s" . narrow-to-sexp)
        ("l" . narrow-to-sexp) ; alias for Org mode
        ("r" . narrow-to-region)
        ("." . narrow-to-point)))

(use-package open-externally
  :bind ("C-c x" . open-externally)
  :commands dired-open-externally)

(use-package dot-mode
  :ensure t
  :diminish
  :init (global-dot-mode)
  :config
  (defvar dot-mode-map (assoc 'dot-mode minor-mode-map-alist))
  (unbind-key "C-M-." dot-mode-map)
  (unbind-key "C-c ." dot-mode-map)
  :bind
  (:map dot-mode-map
        ("C->" . dot-mode-override)
        ("C-x C-." . dot-mode-copy-to-last-kbd-macro)))

(use-package beginend
  :ensure t
  :demand t
  :diminish beginend-global-mode
  :config
  (dolist (mode beginend-modes) (diminish (cdr mode)))
  (beginend-global-mode))

(use-package avy
  :ensure t
  :bind
  (("M-j" . avy-goto-word-or-subword-1)
   ("M-i" . avy-goto-char-timer)
   ([remap goto-line] . avy-goto-line))
  (:map isearch-mode-map
        ("M-'" . avy-isearch)))

(use-package ace-link
  :ensure t
  :config (ace-link-setup-default)
  :custom (avy-styles-alist nil))

(use-package goto-addr
  :bind ("C-c o" . ace-link-addr)
  :hook
  (text-mode . goto-address-mode)
  (prog-mode . goto-address-prog-mode)
  (eshell-mode . goto-address-mode))

(use-package elec-pair :init (electric-pair-mode))

(use-package paren :init (show-paren-mode))

(use-package text-mode
  :defer t
  :config
  (modify-syntax-entry ?\" "\"" text-mode-syntax-table))

(use-package eldoc :defer t :diminish)

(use-package ediff
  :defer t
  :custom
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package occur
  :defer t
  :hook (occur-mode . force-truncate-lines))

(use-package package-lint :ensure t :defer t)

(use-package restart-emacs :ensure t :defer t)

(use-package shr
  :defer t
  :custom
  (shr-use-colors nil))

(use-package eww
  :bind
  (("C-x w" . eww)
   ("C-x W" . eww-list-bookmarks))
  :custom
  (eww-bookmarks-directory "~/.private/")
  (eww-search-prefix "http://google.com/search?q="))

(use-package latex
  :ensure auctex
  :bind (:map LaTeX-mode-map
              ("$" . math-delimiters-insert)
              ("C-'" . TeX-font))
  :custom
  (TeX-save-query nil)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  :hook
  (LaTeX-mode . fix-LaTeX-minor-annoyances)
  (LaTeX-mode . turn-on-cdlatex)
  :config
  (defun LaTeX-outline-name ()
    "Guess a name for the current header line."
    (save-excursion
      (search-forward "{" nil t)
      (let ((beg (point)))
        (forward-char -1)
        (condition-case nil
            (progn
              (forward-sexp 1)
              (forward-char -1))
          (error (forward-sentence 1)))
        (buffer-substring beg (point)))))
  (defun fix-LaTeX-minor-annoyances ()
    (modify-syntax-entry ?\\ "'" LaTeX-mode-syntax-table))
  (dolist (p `((,(executable-find "SumatraPDF") "SumatraPDF")
               (,(executable-find "zathura") "Zathura")
               (,(featurep 'pdf-tools) "PDF Tools")))
    (when (car p)
      (setcdr (assq 'output-pdf TeX-view-program-selection)
              (cdr p)))))

(use-package cdlatex
  :ensure t
  :defer t
  :bind (:map cdlatex-mode-map ("$") ("(") ("[") ("{"))
  :custom
  (cdlatex-math-modify-alist '((?B "\\mathbb" nil t nil nil)
                               (?k "\\mathfrak" nil t nil nil)))
  (cdlatex-math-symbol-alist '((?+ "\\cup" "\\oplus" "\\bigoplus")
                               (?* "\\times" "\\otimes")
                               (?o "\\omega" "\\circ")
                               (?x "\\chi" "\\xrightarrow"))))

(use-package reftex
  :ensure t
  :after latex
  :hook (LaTeX-mode . reftex-mode)
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-ref-macro-prompt nil)
  (reftex-label-alist
   '(("theorem"     ?T "thm:"  "~\\ref{%s}" t ("theorem")     -3)
     ("lemma"       ?L "lem:"  "~\\ref{%s}" t ("lemma")       -3)
     ("proposition" ?P "prop:" "~\\ref{%s}" t ("proposition") -3)
     ("corollary"   ?C "cor:"  "~\\ref{%s}" t ("corollary")   -3)
     ("remark"      ?R "rem:"  "~\\ref{%s}" t ("remark")      -3)
     ("definition"  ?D "defn:" "~\\ref{%s}" t ("definition")  -3))))

(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-midnight-colors '("#ffffff" . "#000000"))
  :bind
  (:map pdf-view-mode-map
        ("d" . pdf-view-midnight-minor-mode))
  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package pdf-annot
  :defer t
  :custom
  (pdf-annot-minor-mode-map-prefix "a")
  (pdf-annot-list-format '((page . 3) (type . 7) (contents . 200))))

(use-package pdf-loader
  :init (pdf-loader-install))

(use-package dired
  :bind (:map dired-mode-map
              ("e" . dired-open-externally))
  :custom
  (dired-dwim-target t)
  :hook
  (dired-mode . force-truncate-lines)
  (dired-mode . dired-hide-details-mode))

(use-package eshell-extras
  :commands
  eshell/in-term eshell/for-each interactive-cd)

(use-package eshell
  :bind ("C-!" . eshell)
  :config (setenv "PAGER" "cat"))

(use-package esh-mode
  :bind (:map eshell-mode-map
              ("<home>" . eshell-bol)
              ("C-c d" . interactive-cd)
              ("M-q" . quit-window)))

(use-package em-hist
  :defer t
  :custom (eshell-hist-ignoredups t))

(use-package shell
  :bind (:map shell-mode-map
              ("C-c d" . interactive-cd)))

(use-package term
  :bind
  (:map term-mode-map
        ("C-c d" . interactive-cd))
  (:map term-raw-map
        ("C-c d" . interactive-cd)))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch))

(use-package markdown-mode
  :ensure t
  :hook 
  (markdown-mode . turn-off-auto-fill)
  (markdown-mode . turn-on-visual-line-mode)
  :config
  (modify-syntax-entry ?\" "\"" markdown-mode-syntax-table))

(use-package org
  :ensure org-plus-contrib
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c s" . org-store-link))
  (:map org-mode-map
        ("C-c o" . ace-link-org)
        ("$" . math-delimiters-insert)
        ("C-$" . ispell-complete-word)
        ("C-'" . org-emphasize)
        ("C-x n s" . org-narrow-to-subtree)
        ("C-x n b" . org-narrow-to-block)  
        ("C-x n e" . org-narrow-to-element))
  :custom
  (org-refile-use-outline-path 'file)
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-support-shift-select t)
  (org-capture-bookmark nil)
  (org-highlight-latex-and-related '(latex script entities))
  (org-export-with-smart-quotes t)
  (org-confirm-babel-evaluate nil)
  (org-export-async-init-file "~/.emacs.d/my-lisp/org-async-init.el")
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  (org-preview-latex-image-directory "~/.cache/ltximg/")
  :hook
  (org-mode . turn-on-org-cdlatex)
  (org-mode . ediff-with-org-show-all)
  :config
  (defun ediff-with-org-show-all ()
    (add-hook 'ediff-prepare-buffer-hook #'org-show-all nil t))
  (customize-set-variable
   'org-structure-template-alist
   (append org-structure-template-alist
           '(("thm"  . "theorem")
             ("pf"   . "proof")
             ("lem"  . "lemma")
             ("cor"  . "corollary")
             ("def"  . "definition")
             ("rem"  . "remark")
             ("exer" . "exercise")
             ("prop" . "proposition")
             ("el"   . "src emacs-lisp"))))
  (customize-set-variable
   'org-latex-default-packages-alist
   (seq-filter
    (lambda (x)
      ;; Won't install these packages on the space limited Chromebook
      (not (member (cadr x) '("fontenc" "textcomp"))))
    org-latex-default-packages-alist))
  (customize-set-variable
   'org-latex-packages-alist
   (cons '("AUTO" "babel" t ("pdflatex")) org-latex-packages-alist))
  (when (executable-find "latexmk")
    (customize-set-variable 'org-latex-pdf-process '("latexmk -pdf %f")))
  (modify-syntax-entry ?< "_" org-mode-syntax-table)
  (modify-syntax-entry ?> "_" org-mode-syntax-table)
  (bind-keys :map narrow-map ("s" . narrow-to-sexp) ("b") ("e")))

(use-package org-config :after org) ; private package

(use-package org-variable-pitch
  :ensure t
  :after org
  :diminish
  org-variable-pitch-minor-mode
  buffer-face-mode
  :bind (:map org-mode-map
              ("C-c t v" . org-variable-pitch-minor-mode)))

(use-package ispell
  :defer t
  :config
  (defconst ispell-org-skip-alists
    '(("\\\\\\[" . "\\\\\\]")
      ("\\\\(" . "\\\\)")
      ("\\begin{\\(align\\|equation\\)}" . "\\end{\\(align\\|equation\\)}" )
      ("\\[fn:" . "\\]")
      ("#\\+BEGIN_SRC". "#\\+END_SRC")))
  (dolist (reg ispell-org-skip-alists)
    (add-to-list 'ispell-skip-region-alist reg))
  (add-to-list 'ispell-tex-skip-alists '(("\\$" . "\\$"))))

(use-package try :ensure t :bind ("C-x p t" . try))

;;; email packages

(use-package email-config) ; private package

(use-package gnus
  :bind
  ("C-c g" . gnus)
  (:map gnus-summary-mode-map ("C-c o" . ace-link-gnus))
  (:map gnus-article-mode-map ("C-c o" . ace-link-gnus))
  :config
  :custom
  (gnus-ignored-newsgroups
   "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  ;; all-user-mail-addresses-regexp is defined in email-config
  (gnus-ignored-from-addresses all-user-mail-addresses-regexp))

(use-package bbdb
  :ensure t
  :after message
  :hook
  (message-mode . bbdb-mail-aliases)
  :custom
  (bbdb-file "~/.private/bbdb")
  (bbdb-mua-pop-up nil)
  (bbdb-completion-display-record nil)
  (bbdb-update-records-p 'query)
  :config
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'message))

(use-package message
  :bind (:map message-mode-map
              ("<C-tab>" . expand-mail-aliases))
  :custom
  (message-signature nil)
  (message-from-style 'angles)
  ;; all-user-mail-addresses-regexp is defined in email-config
  (message-alternative-emails all-user-mail-addresses-regexp)
  :hook
  (message-mode . turn-off-auto-fill)
  (message-mode . turn-on-visual-line-mode))

(use-package message-extras
  ;; private package
  :after message
  :bind
  (:map message-mode-map
        ([remap message-insert-signature] . choose-signature)
        ("C-c t f" . toggle-from-address))
  :commands set-smtp-server
  :hook
  (message-send . set-smtp-server))

(use-package sx
  :ensure t
  :defer t
  :init
  (defalias 'sx #'sx-tab-all-questions)
  :custom
  (sx-cache-directory "~/.private/sx")
  :custom-face
  (sx-question-mode-content-face ((t (:inherit default)))))

;;; major modes

(use-package python
  :defer t
  :custom
  (python-shell-interpreter "python3"))

(use-package slime
  :ensure t
  :defer t
  :custom
  (slime-lisp-implementations '((sbcl ("sbcl" "--no-inform")))))

(use-package slime-repl
  :after slime
  :bind (:map slime-repl-mode-map ("DEL")))

(use-package clojure-mode :ensure t :defer t)

(use-package cicio-mode
  :mode ("\\.ci\\'" . cicio-mode)
  :commands run-cicio)

(use-package lua-mode
  :ensure t
  :defer t
  :custom
  (lua-indent-level 2)
  (lua-default-application "luajit"))

(use-package julia-mode
  :ensure t
  :defer t
  :config
  (defun run-julia ()
    "Just run julia in a term buffer."
    (interactive)
    (switch-to-buffer (make-term "julia" "julia"))
    (term-mode)
    (term-char-mode)))

(use-package haskell-mode
  :ensure t
  :defer t
  :hook
  capitalized-words-mode
  turn-on-haskell-indentation
  interactive-haskell-mode)

(when (executable-find "sage")
  (defun sage-notebook ()
    "Start a Sage notebook. This makes a buffer to communicate with
the Sage kernel, useful to shut it down, for example."
    (interactive)
    (bury-buffer
     (process-buffer
      (start-process "sage-notebook" "*sage*" "sage" "--notebook=jupyter")))))
(put 'scroll-left 'disabled nil)
