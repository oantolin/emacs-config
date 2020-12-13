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
 '(use-dialog-box nil)
 '(ring-bell-function #'ignore))

(when (string= (system-name) "penguin") ; Chromebook
  (set-face-attribute 'default nil :height 110)
  (define-key key-translation-map (kbd "<next>") (kbd "<M-down>"))
  (define-key key-translation-map (kbd "<S-next>") (kbd "<S-M-down>"))
  (define-key key-translation-map (kbd "<prior>") (kbd "<M-up>"))
  (define-key key-translation-map (kbd "<S-prior>") (kbd "<S-M-up>")))

(custom-set-faces
 '(default ((((type w32)) :family "Consolas"))))

(custom-set-faces
 `(variable-pitch ((((type w32)) :family "Georgia")
                   (t :family "DejaVu Serif")))
 '(Info-quoted ((t :inherit fixed-pitch)))
 `(fixed-pitch ((t :family ,(face-attribute 'default :family))))
 '(fringe ((t :background nil))))

;;; package.el & use-package setup

(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

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

(use-package gcmh
  :ensure t
  :diminish
  :custom
  (gcmh-high-cons-threshold 80000000)
  (gcmh-idle-delay 5)
  :config (gcmh-mode))

(use-package bind-key
  :bind ("C-h y" . describe-personal-keybindings))

(add-to-list 'load-path "~/.emacs.d/my-lisp/")
(add-to-list 'load-path "~/.private/")

(when (eq system-type 'windows-nt)
  (cd "~/")
  (setenv "LANG" "en_US"))

;;; misc

(let ((home-bin (expand-file-name "bin" (getenv "HOME")))
      (path (getenv "PATH")))
  (unless (or (string-prefix-p home-bin path)
              (not (file-exists-p home-bin)))
    (setenv "PATH" (concat home-bin ":" path))
    (add-to-list 'exec-path home-bin)))

(dolist (cmd '(narrow-to-region
               upcase-region
               downcase-region
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
 ("C-h M" . describe-keymap)
 ("C-x C-p" . proced)
 ("C-x c" . set-goal-column)
 ("C-x k" . kill-current-buffer)
 ("C-x p" . list-packages)
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
 ([remap apropos-command] . apropos)
 ;; The Chromebook has a pretty reload key!
 ("<XF86Reload>" . revert-buffer))

(global-set-key (kbd "M-r") ctl-x-r-map)

(when (string= (system-name)  "penguin")
  ;; Alt+backspace sends <delete> on the Chromebook...
  (bind-key "<delete>" #'backward-kill-word))

(bind-keys
 :prefix "C-c t"
 :prefix-map toggle-map
 :prefix-docstring "Keymap for commands that toggle various settings."
 ("c" . column-number-mode)
 ("d" . toggle-debug-on-error)
 ("t" . toggle-truncate-lines)
 ("s" . whitespace-mode)
 ("v" . variable-pitch-mode)
 ("o" . org-toggle-link-display))

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

(use-package modus-themes
  :ensure t
  :bind
  ("C-c t b" . modus-themes-toggle)
  :custom
  (modus-themes-slanted-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-scale-headings t)
  :init
  (load-theme 'modus-operandi t (not (display-graphic-p)))
  (load-theme 'modus-vivendi t (display-graphic-p)))

(use-package imenu
  :custom (imenu-space-replacement nil))

(use-package misc
  :bind
  ("M-z" . zap-up-to-char)
  ("M-F" . forward-to-word)
  ("M-B" . backward-to-word)
  ("C-M-\"". copy-from-above-command))

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
  ("C-M--" . kill-inside-sexp)
  ("C-M-=" . mark-inside-sexp)
  ("M-U" . unwrap-sexp)
  ("M-S" . unwrap-mark-sexp)
  ("C-|" . pipe-region)
  ("C-S-w" . forward-to-whitespace)
  ("C-S-r" . backward-to-whitespace)
  ("M-W" . mark-non-whitespace)
  ("M-'" . dabbrev-next)
  ("C-`" . store-register-dwim)
  ("M-`" . use-register-dwim)
  :commands force-truncate-lines)

(use-package placeholder
  :load-path "~/my-elisp-packages/placeholder"
  :bind
  ("M-_" . placeholder-insert)
  ("C-S-n" . placeholder-forward)
  ("C-S-p" . placeholder-backward))

(use-package isearch-extras 
  :custom
  (search-whitespace-regexp ".*?")
  (isearch-allow-scroll t)
  :bind
  (:map isearch-mode-map
        ("<S-return>" . isearch-exit-at-end)
        ([remap isearch-abort] . isearch-cancel)
        ("<C-backspace>" . isearch-delete-wrong)
        ("C-M-w" . isearch-yank-region))
  :hook
  (isearch-mode-end . isearch-exit-at-start))

(use-package math-delimiters
  :load-path "~/my-elisp-packages/math-delimiters"
  :bind
  (:map toggle-map
        ("m" . math-delimiters-toggle))
  :commands
  math-delimiters-no-dollars
  math-delimiters-insert)

(use-package block-undo)

(use-package help-extras
  :commands cotd describe-keymap)

(use-package epithet
  :load-path "~/my-elisp-packages/epithet"
  :bind ("C-x B" . epithet-rename-buffer))

(use-package various-toggles
  :bind
  (:map toggle-map
        ("w" . toggle-wrapping)
        ("l" . toggle-ispell-lang)
        ("SPC" . toggle-completion-ui)))

(use-package window-extras
  :bind
  (:map ctl-x-4-map
        ("s" . toggle-window-split)
        ("t" . transpose-windows)))

(use-package minibuffer
  :bind
  (:map minibuffer-local-completion-map
        ("<backtab>" . minibuffer-force-complete)
        ("M-RET" . exit-minibuffer)
        ("M-?" . minibuffer-completion-help)
        ("SPC") ("?"))
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-auto-help nil)
  (completion-cycle-threshold 5)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (resize-mini-windows t)
  (minibuffer-eldef-shorten-default t)
  :init
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode)
  :hook
  (minibuffer-setup . use-default-completion-in-region)
  :config
  (defun use-default-completion-in-region ()
    (unless (string= "Eval: " (minibuffer-prompt))
      (setq-local completion-in-region-function #'completion--in-region))))

(use-package minibuffer-extras
  :bind
  (:map minibuffer-local-filename-completion-map
        ("<C-backspace>" . up-directory)
        ("C-c C-d" . cd-bookmark)))

(use-package orderless
  :load-path "~/my-elisp-packages/orderless"
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
               seq ".*?"))
            (format-unless (fmt pat)
              (lambda (x)
                (if (string-match-p pat x) x (format fmt x)))))
    (defun my-regexp-converter (pattern)
      (cond
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
  :custom (orderless-matching-styles #'my-regexp-converter))

(use-package avy-embark-occur
  :bind
  (:map minibuffer-local-completion-map
        ("C-'" . avy-embark-occur-choose)
        ("C-\"" . avy-embark-occur-act)))

(use-package icomplete
  :demand t
  ;; :config (icomplete-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
	      ("<up>" . icomplete-backward-completions)
	      ("C-p" . icomplete-backward-completions)
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
  :demand t
  :load-path "~/my-elisp-packages/icomplete-vertical"
  :bind (:map icomplete-minibuffer-map
              ("C-v" . icomplete-vertical-toggle))
  :config (icomplete-vertical-mode))

(use-package embark
  :demand t
  :load-path "~/my-elisp-packages/embark"
  :bind
  ("C-;" . embark-act)
  ("C-c e" . embark-export)
  (:map minibuffer-local-map
        ("C->" . embark-become))
  (:map minibuffer-local-completion-map
        ("C-;" . embark-act-noexit)
        ("C-:" . embark-act)
        ("C-o" . embark-occur)
        ("C-l" . embark-live-occur) ; only here for crm, really
        ("M-e" . embark-export)
        ("<down>" . embark-switch-to-live-occur)
        ("M-q" . embark-occur-toggle-view))
  (:map completion-list-mode-map
        (";" . embark-act))
  (:map embark-meta-map
        ("?" . embark-keymap-help)
        ("C-h"))
  (:map embark-occur-mode-map
        ("a") ; I don't like my own default :)
        (";" . embark-act)
        ("'" . avy-embark-occur-choose)
        ("\"" . avy-embark-occur-act))
  (:map embark-package-map
        ("g" . package-refresh-contents)
        ("a" . package-autoremove)
        ("U" . package-update-all)
        ("I" . embark-insert)
        ("t" . try))
  (:map embark-file-map
        ("x" . consult-file-externally))
  :custom
  (embark-occur-initial-view-alist '((line . list)
                                     (kill-ring . list)
                                     (t . grid)))
  (embark-occur-minibuffer-completion t)
  (embark-annotator-alist '((t . embark-annotation-function-metadatum)))
  (completing-read-function 'embark-completing-read)
  :config
  (setf (alist-get 'variable embark-keymap-alist) 'embark-symbol-map)
  (defun package-update-all (&optional no-fetch)
    "Upgrade all packages.
When optional argument NO-FETCH is non-nil (interactively with
prefix argument), do not fetch packages."
    (interactive "P")
    (save-window-excursion
      (let (package-menu-async)
        (package-list-packages no-fetch))
      (package-menu-mark-upgrades)
      (condition-case nil (package-menu-execute) (user-error))))
  (defun embark-ignore-target (&rest _) (ignore (embark-target)))
  (dolist (fn '(package-autoremove
                package-refresh-contents
                package-update-all))
    (advice-add fn :before #'embark-ignore-target))
  (advice-add 'tabulated-list-revert :after
              (defun resize-embark-live-occur-window (&rest _)
                (when (and (eq major-mode 'embark-occur-mode)
                           (string-match-p "Live" (buffer-name)))
                  (fit-window-to-buffer (get-buffer-window)
                                        (floor (frame-height) 2) 1)))))

(use-package marginalia
  :load-path "~/my-elisp-packages/marginalia"
  :demand t
  :bind
  (:map toggle-map
        ("a" . marginalia-cycle-annotators))
  :config
  (marginalia-mode)
  (marginalia-cycle-annotators))

(use-package consult
  :load-path "~/my-elisp-packages/consult"
  :bind
  ("C-M-y" . consult-yank-replace)
  ("M-g l" . consult-line)
  ("M-g M-l" . consult-line)
  ("M-g i" . consult-imenu)
  ("M-g M-i" . consult-imenu)
  ("M-g o" . consult-outline)
  ("M-g M-o" . consult-outline)
  (:map minibuffer-local-map
        ("M-r" . consult-history)
        ("M-s"))
  :commands
  consult-completion-in-region
  consult-file-externally
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (consult-preview-mode)
  (setf (alist-get 'slime-repl-mode consult-mode-histories)
        'slime-repl-input-history))

(use-package tmp-buffer
  :bind ("C-c n" . tmp-buffer))

(use-package narrow-extras
  :bind
  (:map ctl-x-map
        ("C-n" . narrow-or-widen-dwim))
  (:map narrow-map
        ("s" . narrow-to-sexp)
        ("l" . narrow-to-sexp) ; alias for Org mode
        ("r" . narrow-to-region)
        ("." . narrow-to-point)))

(use-package dot-mode
  :ensure t
  :diminish
  :demand t
  :config
  (global-dot-mode)
  (defvar dot-mode-map (assoc 'dot-mode minor-mode-map-alist))
  (unbind-key "C-M-." dot-mode-map)
  (unbind-key "C-c ." dot-mode-map)
  :bind
  (:map dot-mode-map
        ("C->" . dot-mode-override)
        ("C-x C-." . dot-mode-copy-to-last-kbd-macro)))

(use-package beginend
  :ensure t
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
  :config
  (ace-link-setup-default)
  (setq avy-styles-alist nil))

(use-package elec-pair :init (electric-pair-mode))

(use-package paren :init (show-paren-mode))

(use-package text-mode
  :hook 
  (text-mode . turn-on-visual-line-mode)
  :config
  (remove-hook 'text-mode-hook 'turn-on-auto-fill)
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

(use-package restart-emacs
  :ensure t
  :bind ("C-x M-c" . restart-emacs))

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
  :bind
  (:map LaTeX-mode-map
        ("$" . math-delimiters-insert)
        ("C-'" . TeX-font)
        ([remap next-error])
        ([remap previous-error])
        ("M-g M-n" . TeX-next-error)
        ("M-g M-p" . TeX-previous-error)
        ("M-n" . next-error)
        ("M-p" . previous-error))
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
  (setcdr (assq 'output-pdf TeX-view-program-selection)
          '("PDF Tools")))

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
  (pdf-annot-list-format '((page . 3) (type . 7) (contents . 200)))
  (pdf-annot-activate-created-annotations t))

(use-package pdf-loader
  :init (pdf-loader-install))

(use-package dired
  :bind (:map dired-mode-map
              ("e" . dired-toggle-read-only)
              ("E" . dired-open-externally))
  :custom
  (dired-dwim-target t)
  :hook
  (dired-mode . force-truncate-lines)
  (dired-mode . dired-hide-details-mode)
  :config
  (defun dired-open-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks
     (embark-open-externally (dired-get-filename))
     arg)))

(use-package eshell-extras
  :commands
  eshell/in-term
  eshell/for-each
  interactive-cd)

(use-package eshell
  :bind
  ("C-!" . eshell)
  :config (setenv "PAGER" "cat"))

(use-package esh-mode
  :bind
  (:map eshell-mode-map
        ("<home>" . eshell-bol)
        ("C-c d" . interactive-cd)
        ("M-q" . quit-window)))

(use-package comint
  :bind ())

(use-package em-hist
  :defer t
  :bind
  (:map eshell-hist-mode-map
        ("M-r" . consult-history)
        ("M-s"))
  :custom (eshell-hist-ignoredups t))

(use-package shell
  :bind (:map shell-mode-map
              ("C-c d" . interactive-cd)
              ("M-r" . consult-history)
              ("M-s")))

(use-package term
  :bind
  (:map term-mode-map
        ("C-c d" . interactive-cd)
        ("M-r" . consult-history)
        ("M-s"))
  (:map term-raw-map
        ("C-c d" . interactive-cd)
        ("M-r" . consult-history)
        ("M-s")))

(use-package magit :ensure t :defer t)

(use-package markdown-mode
  :ensure t
  :config
  (modify-syntax-entry ?\" "\"" markdown-mode-syntax-table))

(use-package org
  :ensure org-plus-contrib
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c s" . org-store-link)
   ("C-c C" . org-clock-goto))
  (:map org-mode-map
        ("C-c o" . ace-link-org)
        ("$" . math-delimiters-insert)
        ("C-$" . ispell-complete-word)
        ("C-'" . org-emphasize)
        ("C-x n s" . org-narrow-to-subtree)
        ("C-x n b" . org-narrow-to-block)  
        ("C-x n e" . org-narrow-to-element))
  :custom
  (org-ellipsis "…")
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
  (bind-keys :map narrow-map ("s" . narrow-to-sexp) ("b") ("e"))
  (org-link-set-parameters
   "org-title"
   :store (defun store-org-title-link ()
            "Store a link to the org file visited in the current buffer.
Use the #+TITLE as the link description. The link is only stored
if `org-store-link' is called from the #+TITLE line."
            (when (and (derived-mode-p 'org-mode)
                       (save-excursion
                         (beginning-of-line)
                         (looking-at "#\\+TITLE:")))
              (org-link-store-props
               :type "file"
               :link (concat "file:" (buffer-file-name))
               :description (cadar (org-collect-keywords '("TITLE"))))))))

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
  (add-to-list 'ispell-dicts-name2locale-equivs-alist
               '("español" "es_MX"))
  (defconst ispell-org-skip-alists
    '(("\\\\\\[" . "\\\\\\]")
      ("\\\\(" . "\\\\)")
      ("\\begin{\\(align\\|equation\\)}" . "\\end{\\(align\\|equation\\)}" )
      ("\\[fn:" . "\\]")
      ("#\\+BEGIN_SRC". "#\\+END_SRC")))
  (dolist (reg ispell-org-skip-alists)
    (add-to-list 'ispell-skip-region-alist reg))
  (add-to-list 'ispell-tex-skip-alists '(("\\$" . "\\$"))))

(use-package try :ensure t :defer t)

;;; email packages

(use-package email-config) ; private package

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
  :bind (:map slime-repl-mode-map
              ("DEL")
              ("M-r" . consult-history)
              ("M-s")))

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

(when (executable-find "sage")
  (defun sage-notebook ()
    "Start a Sage notebook. This makes a buffer to communicate with
the Sage kernel, useful to shut it down, for example."
    (interactive)
    (bury-buffer
     (process-buffer
      (start-process "sage-notebook" "*sage*" "sage" "--notebook=jupyter")))))
