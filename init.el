;;; -*- lexical-binding: t -*-

;;; Customize thinks it knows better than me

(setq custom-file (make-temp-file "emacs-custom-"))

;;; GUI

(custom-set-variables
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(use-dialog-box nil)
 '(ring-bell-function #'ignore)
 '(cursor-type 'bar)
 '(tab-bar-show 1))

(when (string= (system-name) "penguin") ; Chromebook
  (set-face-attribute 'default nil :height 110)
  (define-key key-translation-map (kbd "<next>") (kbd "<M-down>"))
  (define-key key-translation-map (kbd "<S-next>") (kbd "<S-M-down>"))
  (define-key key-translation-map (kbd "<prior>") (kbd "<M-up>"))
  (define-key key-translation-map (kbd "<S-prior>") (kbd "<S-M-up>")))

(custom-set-faces
 '(default ((((type w32)) :family "Consolas"))))

(custom-set-faces
 `(variable-pitch ((((type w32)) :family "Verdana")
                   (t :family "Noto Sans")))
 '(Info-quoted ((t :inherit fixed-pitch)))
 `(fixed-pitch ((t :family ,(face-attribute 'default :family))))
 '(fringe ((t :background nil))))

(set-fontset-font "fontset-default" 'emoji
                  (font-spec :family (if (eq system-type 'windows-nt)
                                         "Segoe UI Emoji"
                                       "Noto Emoji")))

(set-fontset-font "fontset-default" 'symbol
                  (font-spec :family (if (eq system-type 'windows-nt)
                                         "Segoe UI Symbol"
                                       "Noto Emoji")))

;;; package.el & use-package setup

(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(require 'package)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(custom-set-variables
 '(use-package-enable-imenu-support t))

(eval-when-compile (require 'use-package))

(use-package diminish :ensure t :defer t)

(use-package bind-key
  :bind ("C-h y" . describe-personal-keybindings))

(add-to-list 'load-path "~/.emacs.d/my-lisp/")
(dolist (dir '("placeholder" "math-delimiters"))
  (add-to-list 'load-path (format "~/my-elisp-packages/%s/" dir)))
(add-to-list 'load-path "~/.private/")

(when (eq system-type 'windows-nt)
  (cd "~/")
  (setenv "LANG" "en_US"))

;;; misc

(when (string= (system-name) "penguin") ; Chromebook
  (dolist (dir '("~/texlive/2022/bin/x86_64-linux" "~/bin"))
    (let ((full (expand-file-name dir)))
      (setenv "PATH" (concat full ":" (getenv "PATH")))))
  (setq exec-path (split-string (getenv "PATH") ":")))

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
 '(tab-always-indent 'complete)
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
 '(view-read-only t)
 '(kill-read-only-ok t)
 '(history-delete-duplicates t)
 '(kill-do-not-save-duplicates t)
 '(password-cache-expiry 300)
 '(debugger-stack-frame-as-list t)
 '(split-width-threshold 140)
 '(y-or-n-p-use-read-key t)
 '(use-short-answers t)
 '(async-shell-command-display-buffer nil)
 '(revert-without-query '(""))
 '(recenter-positions '(top middle bottom))
 '(display-time-default-load-average nil)
 '(dictionary-server "dict.org"))

(autoload 'viper-ex "viper")

(bind-keys
 ("C-d" . delete-forward-char)
 ("M-K" . kill-paragraph)
 ("M-Z" . zap-to-char)
 ("C-x c" . set-goal-column)
 ("C-x k" . kill-current-buffer)
 ("C-x C-p" . list-packages)
 ("M-s k" . keep-lines)
 ("M-s f" . flush-lines)
 ("M-s c" . count-matches)
 ("M-s d" . dictionary-search)
 ("C-h p" . describe-package)  ; swap these two
 ("C-h P" . finder-by-keyword)
 ("C-c l" . find-library)
 ("C-x M-c" . restart-emacs)
 ([remap list-buffers] . electric-buffer-list)
 ([remap count-words-region] . count-words)
 ([remap dabbrev-expand] . hippie-expand)
 ("C-M-o" . up-list)
 ("C-o" . split-line)
 ("M-o" . other-window)
 ((if (string= (system-name) "penguin") "<C-delete>" "<C-M-backspace>") .
  ;; Alt+backspace sends <delete> on the Chromebook...
  kill-backward-up-list)
 ("M-R" . raise-sexp)
 ("M-E" . mark-end-of-sentence)
 ("M-T" . transpose-sentences)
 ("C-x M-t" . transpose-paragraphs)
 ([remap apropos-command] . apropos)
 ("C-:" . viper-ex) ; the g command is worth having!
 ;; The Chromebook has a pretty reload key!
 ("<XF86Reload>" . revert-buffer))

(bind-keys :prefix-map insert-pair-map
           :prefix "C-S-w"
           ([t] . insert-pair))
           
(when (string= (system-name)  "penguin")
  ;; Alt+backspace sends <delete> on the Chromebook...
  (bind-key "<delete>" #'backward-kill-word))

(bind-keys :prefix-map toggle-map
           :prefix "C-c x"
           :prefix-docstring "Keymap for commands that toggle settings."
           ("c" . column-number-mode)
           ("d" . toggle-debug-on-error)
           ("t" . toggle-truncate-lines)
           ("s" . whitespace-mode)
           ("v" . variable-pitch-mode)
           ("i" . visible-mode))

(bind-keys :prefix-map time-map
           :prefix "C-c t"
           :prefix-docstring "Keymap for commands that deal with time."
           ("w" . world-clock)
           ("t" . display-time-mode)
           ("c" . calendar)
           ("o" . org-timer-set-timer)
           ("p" . org-timer-pause-or-continue)
           ("s" . org-timer-stop))

;;; packages

(use-package modus-themes
  :ensure t
  :bind
  ("C-c x b" . modus-themes-toggle)
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-headings '((1 1.3) (2 1.2) (3 1.1)))
  :init
  (modus-themes-load-themes)
  :config
  (if (display-graphic-p)
      (modus-themes-load-operandi)
    (modus-themes-load-vivendi)))

(use-package imenu
  :defer t
  :custom (imenu-space-replacement nil))

(use-package custom
  :hook
  (Custom-mode . configure-imenu-Custom)
  :config
  (defun configure-imenu-Custom ()
    (setq imenu-generic-expression
          '(("Faces" "^\\(?:Show\\|Hide\\) \\(.*\\) face: \\[sample\\]" 1)
            ("Variables" "^\\(?:Show Value\\|Hide\\) \\([^:\n]*\\)" 1)))))

(use-package recentf
  :custom
  (recentf-max-saved-items 50)
  :init
  (recentf-mode))

(use-package bookmark
  :defer t
  :custom
  (bookmark-fontify nil))

(use-package repeat
  :init (repeat-mode))

(use-package misc
  :bind
  ("M-z" . zap-up-to-char)
  ("M-F" . forward-to-word)
  ("M-B" . backward-to-word)
  ("M-\"". copy-from-above-command))

(use-package rect
  :bind
  (:map rectangle-mark-mode-map
        ("t" . string-rectangle)
        ("o" . open-rectangle)
        ("c" . clear-rectangle)
        ("n" . rectangle-number-lines)
        ("x" . rectangle-exchange-point-and-mark)
        ("*" . calc-grab-rectangle)
        (":" . calc-grab-sum-down)
        ("_" . calc-grab-sum-across))
  :init
  (autoload 'calc-grab-sum-down "calc" nil t)
  (autoload 'calc-grab-sum-across "calc" nil t))

(use-package visiting-buffer)

(use-package text-extras
  :bind
  ("M-Q" . unfill-paragraph)
  ("C-\"" . copy-word-from-above)
  ("M-L" . mark-line)
  ("M-C" . mark-char)
  ("M-@" . mark-my-word)
  ("M-g r" . goto-random-line)
  ("M-g M-r" . goto-random-line)
  ("C-M--" . kill-inside-sexp)
  ("C-M-=" . mark-inside-sexp)
  ("M-U" . unwrap-sexp)
  ("M-S" . unwrap-mark-sexp)
  ("C-|" . pipe-region)
  ("C-S-s" . forward-to-whitespace)
  ("C-S-r" . backward-to-whitespace)
  ("M-W" . mark-non-whitespace)
  ("M-'" . dabbrev-next)
  ("C-M-'" . dabbrev-complete-next)
  ("C-c e" . text-to-clipboard)
  ("M-s i" . focus-lines)
  ([remap upcase-word] . upcase-dwiw)
  ([remap downcase-word] . downcase-dwiw)
  ([remap capitalize-word] . capitalize-dwiw)
  :commands
  force-truncate-lines
  turn-off-visual-line-mode)

(use-package placeholder
  :bind
  ("M-_" . placeholder-insert)
  ("C-S-n" . placeholder-forward)
  ("C-S-p" . placeholder-backward))

(use-package topaz-paste
  :commands topaz-paste-region topaz-paste-buffer)

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
  :bind
  (:map toggle-map
        ("$" . math-delimiters-toggle))
  :commands
  math-delimiters-no-dollars
  math-delimiters-insert)

(use-package block-undo)

(use-package help-extras
  :bind
  ("C-h h" . show-help)
  :commands cotd)

(use-package various-toggles
  :bind
  (:map toggle-map
        ("w" . toggle-wrapping)
        ("l" . toggle-ispell-lang)))

(use-package window-extras
  :bind
  (:map ctl-x-4-map
        ("s" . toggle-window-split)
        ("t" . transpose-windows)))

(use-package minibuffer
  :bind
  (:map minibuffer-local-completion-map
        ("<backtab>" . minibuffer-force-complete)
        ("SPC") ("?"))
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (minibuffer-eldef-shorten-default t)
  (resize-mini-windows t)
  :init
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode)
  :hook
  (completion-list-mode . force-truncate-lines)
  :config
  (defun stealthily (fn &rest args)
    "Apply FN to ARGS while inhibiting modification hooks."
    (let ((inhibit-modification-hooks t))
      (apply fn args)))
  (advice-add 'minibuf-eldef-setup-minibuffer :around #'stealthily))

(use-package orderless
  :ensure t
  :demand t
  :config
  (defmacro dispatch: (regexp style)
    (cl-flet ((symcat (a b) (intern (concat a (symbol-name b)))))
      `(defun ,(symcat "dispatch:" style) (pattern _index _total)
         (when (string-match ,regexp pattern)
           (cons ',(symcat "orderless-" style) (match-string 1 pattern))))))
  (cl-flet ((pre/post (str) (format "^%s\\(.*\\)$\\|^\\(?1:.*\\)%s$" str str)))
    (dispatch: (pre/post "=") literal)
    (dispatch: (pre/post "`") regexp)
    (dispatch: (pre/post (if (or minibuffer-completing-file-name
                                 (derived-mode-p 'eshell-mode))
                             "%" "[%.]"))
               initialism))
  (dispatch: "^{\\(.*\\)}$" flex)
  (dispatch: "^\\([^][^\\+*]*[./-][^][\\+*$]*\\)$" prefixes)
  (dispatch: "^!\\(.+\\)$" without-literal)
  :custom
  (orderless-matching-styles 'orderless-regexp)
  (orderless-style-dispatchers
   '(dispatch:literal dispatch:regexp dispatch:without-literal
     dispatch:initialism dispatch:flex dispatch:prefixes))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package icomplete
  :bind (:map icomplete-minibuffer-map
              ("RET" . icomplete-force-complete-and-exit)
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-M-i" . minibuffer-complete))
  :custom
  (icomplete-show-matches-on-no-input t)
  (icomplete-prospects-height 1)
  (icomplete-separator " ⋮ ")
  (icomplete-hide-common-prefix nil))

(use-package vertico
  :ensure t
  :bind
  (:map vertico-map
        ("DEL" . vertico-directory-delete-char)
        ("M-." . consult-dir)
        ("M-j" . consult-dir-jump-file)
        ("M-q" . vertico-multiform-grid)
        ("M-a" . vertico-multiform-unobtrusive)
        ("'" . vertico-quick-jump))
  :custom
  (vertico-multiform-categories
   '((embark-keybinding grid)))
  :init
  (defvar minibuffer--require-match nil)
  (vertico-mode)
  :config
  (vertico-multiform-mode))

(use-package marginalia
  :ensure t
  :bind
  ("M-A" . marginalia-cycle)
  :init
  (marginalia-mode))

(use-package ecomplete-extras
  :commands
  add-email-to-ecomplete
  remove-email-from-ecomplete)

(autoload 'pocket-lib-add-urls "pocket-lib")

(use-package embark
  :ensure t
  :bind
  ("C-;" . embark-act)
  ("C-," . embark-dwim)
  ("C-h b" . embark-bindings)
  ("C-h B" . embark-bindings-at-point)
  ("C-h M" . embark-bindings-in-keymap)
  ("C-h E" . embark-on-last-message)
  ("M-n" . embark-next-symbol)
  ("M-s n" . embark-next-symbol) ; for when M-n is taken
  ("M-p" . embark-previous-symbol)
  ("M-s p" . embark-previous-symbol) ; for when M-p is taken
  (:map completion-list-mode-map
        (";" . embark-act))
  (:map embark-collect-mode-map
        ("a") ; I don't like my own default :)
        (";" . embark-act)
        ("F" . consult-focus-lines))
  (:map embark-package-map
        ("t" . try))
  (:map embark-identifier-map
        ("(" . insert-parentheses))
  (:map embark-expression-map
        ("(" . insert-parentheses))
  (:map embark-email-map
        ("+" . add-email-to-ecomplete)
        ("\\" . remove-email-from-ecomplete))
  (:map embark-encode-map
        ("p" . topaz-paste-region))
  (:map embark-url-map
        ("x" . browse-url-default-browser)
        ("p" . pocket-lib-add-urls))
  (:map embark-identifier-map
        ("D" . dictionary-lookup-definition))
  :custom
  (embark-quit-after-action nil)
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key ";")
  (embark-help-key "?")
  (embark-confirm-act-all nil)
  :config
  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (dolist (cmd '(comment-dwim
                 insert-parentheses
                 markdown-insert-code
                 markdown-insert-italic
                 markdown-insert-bold
                 org-emphasize
                 cdlatex-math-modify
                 TeX-font))
    (push #'embark--mark-target (alist-get cmd embark-pre-action-hooks)))
  (push #'embark--xref-push-marker
        (alist-get 'find-file embark-pre-action-hooks))
  (defun embark-on-last-message (arg)
    "Act on the last message displayed in the echo area."
    (interactive "P")
    (with-current-buffer "*Messages*"
      (goto-char (1- (point-max)))
      (embark-act arg))))

(use-package embark-consult :ensure t)

(use-package consult-dir
  :ensure t
  :bind
  (:map minibuffer-local-filename-completion-map
        ("M-." . consult-dir)
        ("M-j" . consult-dir-jump-file)))

(use-package embark-this-buffer
  :after embark
  :demand t
  :bind
  (:map this-buffer-map
        ("P" . topaz-paste-buffer)))

(use-package embark-extra-indicators
  :commands
  embark-which-key-indicator
  embark-minibuffer-indicator)

(use-package consult
  :ensure t
  :bind
  ("M-y" . consult-yank-pop)
  ("M-g l" . consult-line)
  ("M-g i" . consult-imenu)
  ("M-g o" . consult-outline)
  ("M-g I" . consult-imenu-multi)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-s m" . consult-multi-occur)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-g f" . consult-find)
  ("M-X" . consult-mode-command)
  ("C-c b" . consult-buffer)
  ("C-c 4 b" . consult-buffer-other-window)
  ("C-c k" . consult-keep-lines)
  ("C-c f" . consult-focus-lines)
  ("M-`" . consult-register-store)
  ("C-`" . consult-register-load)
  ("C-M-`" . consult-register)
  (:map minibuffer-local-map
        ("M-r" . consult-history)
        ("M-s"))
  (:map consult-narrow-map
        ("C-<" . consult-narrow-help))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (register-preview-function #'consult-register-format)
  (consult-narrow-key "<")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :hook
  ((embark-collect-mode completion-list-mode)
   . consult-preview-at-point-mode)
  (minibuffer-setup . choose-completion-in-region)
  :config
  (defun choose-completion-in-region ()
    "Use default `completion--in-region' unless we are not completing."
    (when minibuffer-completion-table
      (setq-local completion-in-region-function #'completion--in-region)))
  (when (eq (window-system) 'w32)
    (setq consult-find-args
          (replace-regexp-in-string "\\*" "\\\\*" consult-find-args)))
  (advice-add #'register-preview :override #'consult-register-window)
  (setf (alist-get 'log-edit-mode consult-mode-histories)
        'log-edit-comment-ring))

(use-package consult-imenu
  :config
  (setf
   (alist-get
    ?k (plist-get (alist-get 'emacs-lisp-mode consult-imenu-config) :types))
   '("Embark Keymaps" font-lock-variable-name-face)))

(use-package webjump
  :bind
  ("C-x w" . webjump))

(use-package webjump-extras
  :after webjump
  :demand t
  :bind
  ("C-x W" . webjump-reload)
  :config
  (webjump-reload))

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
  :hook
  (minibuffer-setup . dot-mode)
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
  (("M-j" . avy-goto-char-timer)
   ([remap goto-line] . avy-goto-line))
  (:map isearch-mode-map
        ("C-'" . avy-isearch))
  :config
  (add-to-list 'avy-dispatch-alist '(?\. . avy-action-goto))
  (defun avy-embark-act (pt)
    "Use Embark to act on the completion at PT."
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0)))
      t))
  (add-to-list 'avy-dispatch-alist '(?\; . avy-embark-act))
  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))
  (add-to-list 'avy-dispatch-alist '(?e . avy-action-exchange)))

(use-package link-hint
  :ensure t
  :bind
  ("C-^" . link-hint-open-link))

(use-package paren :init (show-paren-mode))

(use-package text-mode
  :hook
  (text-mode . turn-on-visual-line-mode)
  (text-mode . variable-pitch-mode)
  :config
  (modify-syntax-entry ?\" "\"" text-mode-syntax-table))

(use-package outline
  :defer t
  :config
  ;; NEWS files use single quotes around elisp symbols. I think those
  ;; are the only files I view in outline-mode, but if I find others
  ;; then I might modify the syntax only locally in NEWS files.
  (modify-syntax-entry ?' "\"" outline-mode-syntax-table)
  :diminish outline-minor-mode
  :hook (prog-mode . outline-minor-mode))

(use-package eldoc :defer t :diminish)

(use-package diff-mode
  :bind (:map diff-mode-map ("M-o")))

(use-package ediff
  :defer t
  :custom
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package occur
  :defer t
  :hook (occur-mode . force-truncate-lines))

(use-package olivetti
  :ensure t
  :bind
  ("C-c x o" . olivetti-mode))

(use-package browse-url
  :defer t
  :custom
  (browse-url-browser-function #'eww-browse-url)
  (browse-url-handlers
   '(("\\`https?://\\(?:youtu\\.be\\|\\(?:www\\.\\)youtube\\.com\\)"
      . browse-url-default-browser)
     ("\\`https?://[^/]+zoom\\.us" . browse-url-default-browser)
     ("https?://meet\\.google\\.com" . browse-url-default-browser)
     ("https?://bluejeans\\.com" . browse-url-default-browser)
     ("https?://doodle\\.com" . browse-url-default-browser)))
  :config
  (advice-add 'browse-url-generic :override 'browse-url-default-browser))

(use-package shr
  :bind
  (:map shr-map
        ("P" . pocket-reader-shr-add-link)
        ("v")) ; don't override view-source with a useless synonym for RET
  :custom
  (shr-use-colors nil)
  (shr-image-animate nil))

(use-package shr-heading
  :commands
  shr-heading-setup-imenu
  shr-heading-next
  shr-heading-previous)

(autoload 'pocket-reader-eww-add-page "pocket-reader-extras")

(use-package eww
  :bind
  (:map eww-mode-map
        ("P" . pocket-reader-eww-add-page)
        ("{" . backward-paragraph)
        ("}" . forward-paragraph)
        ("C-c C-p" . shr-heading-previous)
        ("C-c C-n" . shr-heading-next))
  :custom
  (eww-bookmarks-directory "~/.private/")
  :hook
  (eww-mode . shr-heading-setup-imenu)
  :config
  (modify-syntax-entry ?\“ "(”" eww-mode-syntax-table)
  (modify-syntax-entry ?\” ")“" eww-mode-syntax-table)
  (defun pdfs-are-binary (fn &rest args)
    (let ((buffer-file-coding-system 'binary))
      (apply fn args)))
  (advice-add 'eww-display-pdf :around #'pdfs-are-binary))

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
  (LaTeX-mode . make-backslash-a-prefix-in-LaTeX)
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
  (defun make-backslash-a-prefix-in-LaTeX ()
    "Set the syntax class of \\ to ' in LaTeX buffers."
    (modify-syntax-entry ?\\ "'" LaTeX-mode-syntax-table))
  (setcdr (assq 'output-pdf TeX-view-program-selection)
          '("PDF Tools")))

(use-package cdlatex
  :ensure t
  :diminish
  :diminish org-cdlatex-mode
  :bind (:map cdlatex-mode-map ("$") ("(") ("[") ("{"))
  :custom
  (cdlatex-math-modify-alist '((?B "\\mathbb" nil t nil nil)
                               (?k "\\mathfrak" nil t nil nil)))
  (cdlatex-math-symbol-alist '((?+ "\\cup" "\\oplus" "\\bigoplus")
                               (?& "\\wedge" "\\cap")
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
        ("d" . pdf-view-midnight-minor-mode)
        ("i" . consult-imenu))
  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (defun default-1-page (fn prop &optional winprops)
    (or (funcall fn prop winprops)
        (and (eq prop 'page) 1)))
  (advice-add 'image-mode-window-get :around #'default-1-page))

(use-package pdf-annot
  :defer t
  :custom
  (pdf-annot-minor-mode-map-prefix "a")
  (pdf-annot-list-format '((page . 3) (type . 7) (contents . 200)))
  (pdf-annot-activate-created-annotations t))

(use-package pdf-loader
  :init (pdf-loader-install))

(use-package pdf-outline
  :defer t
  :custom
  (pdf-outline-imenu-use-flat-menus t)
  :config
  (defun pdf-outline-indent (fn link &optional labels)
    (let ((item (funcall fn link labels))
          (indent pdf-outline-buffer-indent))
      (cons (concat (make-string (* indent (1- (alist-get 'depth link))) ?\s)
                    (car item))
            (cdr item))))
  (advice-add 'pdf-outline-imenu-create-item :around #'pdf-outline-indent))

(use-package tramp
  :defer t
  :when (executable-find "plink.exe")
  :custom (tramp-default-method "plink"))

(use-package dired
  :bind
  (:map dired-mode-map
        ("e" . dired-open-externally))
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alGh")
  :hook
  (dired-mode . force-truncate-lines)
  (dired-mode . dired-hide-details-mode)
  :config
  (defun dired-open-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks
     (consult-file-externally (dired-get-filename))
     arg)))

(use-package comint
  :bind
  (:map comint-mode-map
        ("M-r" . consult-history)
        ("M-s")))

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
        ("M-q" . quit-window)
        ([remap display-local-help] . man)))

(use-package em-hist
  :bind
  (:map eshell-hist-mode-map
        ("M-r" . consult-history)
        ("M-s"))
  :custom (eshell-hist-ignoredups t))

(use-package ffap-eshell :after (eshell ffap))

(use-package shell
  :bind (:map shell-mode-map
              ([remap display-local-help] . man)
              ("C-c d" . interactive-cd)))

(use-package sh-script
  :bind (:map sh-mode-map
              ([remap display-local-help] . man)))

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

(use-package vc-extras
  :commands
  clear-log-edit-buffer
  log-view-save-commit-hash
  vc-git-commit)

(use-package log-edit
  :bind
  (:map log-edit-mode-map
        ("M-r" . consult-history)
        ("M-s"))
  :hook
  (log-edit-mode . turn-off-visual-line-mode)
  (log-edit-mode . turn-on-auto-fill)
  :config
  (advice-add 'consult-history :before #'clear-log-edit-buffer)
  (remove-hook 'log-edit-hook #'log-edit-show-files))

(use-package log-view
  :bind
  (:map log-view-mode-map
        ("w" . log-view-save-commit-hash)))

(use-package vc
  :bind
  (:map vc-prefix-map
        ("R" . vc-rename-file)
        ("d" . vc-dir-root)
        ("c" . vc-git-commit)))

(use-package vc-dir
  :bind
  (:map vc-dir-mode-map
        ("r" . vc-revert)
        ("c" . vc-git-commit)))

(use-package smerge-mode
  :config
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map 'smerge-basic-map)))
   smerge-basic-map))

(use-package magit :ensure t :defer t)

(use-package markdown-mode
  :ensure t
  :bind
  (:map markdown-mode-map
        ("C-'" . markdown-mode-style-map))
  :custom-face
  (markdown-metadata-key-face ((t (:foreground nil))))
  (markdown-metadata-value-face ((t (:foreground nil))))
  :config
  (fset 'markdown-mode-style-map markdown-mode-style-map)
  (modify-syntax-entry ?\" "\"" markdown-mode-syntax-table))

(use-package org
  :ensure t
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c s" . org-store-link)
   ("C-c C" . org-clock-goto)
   ("C-c o" . org-open-at-point-global))
  (:map org-mode-map
        ("C-,") ; I use this for embark-dwim
        ("C-c C-'" . org-cycle-agenda-files)
        ("$" . math-delimiters-insert)
        ("C-$" . ispell-complete-word)
        ("C-'" . org-emphasize))
  :custom
  (org-ellipsis "…")
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-highlight-latex-and-related '(latex script entities))
  (org-export-with-smart-quotes t)
  (org-confirm-babel-evaluate nil)
  (org-export-async-init-file "~/.emacs.d/my-lisp/org-async-init.el")
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  (org-preview-latex-image-directory "~/.cache/ltximg/")
  (org-tags-column -55)
  (org-use-speed-commands t)
  (org-cycle-emulate-tab 'whitestart)
  (org-return-follows-link t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-start-on-weekday nil)
  (org-log-into-drawer t)
  :hook
  (org-mode . turn-on-org-cdlatex)
  (org-mode . ediff-with-org-show-all)
  (org-mode . turn-on-auto-fill)
  (org-mode . turn-off-visual-line-mode)
  (org-mode . org-tweak-syntax-table)
  (org-mode . add-pretty-entities-hook)
  :config
  (defun ediff-with-org-show-all ()
    "Expand all headings prior to ediffing org buffers."
    (add-hook 'ediff-prepare-buffer-hook #'org-show-all nil t))
  (defun add-pretty-entities-hook ()
    "Add `org-toggle-pretty-entities' to local value of `visible-mode-hook'."
    (add-hook 'visible-mode-hook 'org-toggle-pretty-entities nil t))
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
   (cl-set-difference org-latex-default-packages-alist
                      '("fontenc" "textcomp")
                      :test #'equal))
  (customize-set-variable
   'org-latex-packages-alist
   (cons '("AUTO" "babel" t ("pdflatex")) org-latex-packages-alist))
  (when (executable-find "latexmk")
    (customize-set-variable 'org-latex-pdf-process '("latexmk -pdf %f")))
  (defun org-tweak-syntax-table ()
    (cl-loop for (ch cl) in '((?< ".") (?> ".") (?\\ "'") (?' "'"))
             do (modify-syntax-entry ch cl org-mode-syntax-table)))
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

(use-package org-modern
  :ensure t
  :bind
  (:map toggle-map
        ("m" . org-modern-mode))
  :config
  (setf (alist-get ?X org-modern-checkbox) "✓"))

(use-package citeproc :ensure t :defer t)

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

(use-package logos
  :ensure t
  :custom
  (logos-outlines-are-pages t)
  :bind
  ([remap forward-page] . logos-forward-page-dwim)
  ([remap backward-page] . logos-backward-page-dwim))

;;; email packages

(use-package email-config) ; private package

(use-package gnus
  :bind
  ("C-c g" . gnus))

(use-package gnus-art
  :bind
  (:map gnus-article-mode-map
        ("{" . backward-paragraph)
        ("}" . forward-paragraph)))

(use-package gnus-sum
  :bind
  (:map gnus-summary-mode-map
        ("M-a" . gnus-symbolic-argument)))

(use-package ecomplete
  :defer t
  :custom
  (ecomplete-database-file "~/.private/ecompleterc")
  :config
  (setq completion-category-defaults nil))

(use-package message
  :bind (:map message-mode-map
              ("C-<tab>" . expand-mail-aliases))
  :custom
  (message-signature nil)
  (message-mail-alias-type 'ecomplete)
  (message-self-insert-commands nil)
  (message-expand-name-standard-ui t)
  ;; all-user-mail-addresses-regexp is defined in email-config
  (message-alternative-emails all-user-mail-addresses-regexp)
  :hook
  (message-mode . turn-off-auto-fill)
  (message-mode . turn-on-visual-line-mode))

(use-package message-extras
  :after message
  :bind
  (:map message-mode-map
        ("C-c x f" . cycle-from-address))
  :commands set-smtp-server
  :hook
  (message-send . set-smtp-server)
  (message-send . message-lint)
  :config
  (defvar cycle-from-address-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "f" #'cycle-from-address)
      map))
  (put 'cycle-from-address 'repeat-map 'cycle-from-address-repeat-map))

;;; applications

(use-package sx
  :ensure t
  :defer t
  :init
  (defalias 'sx #'sx-tab-all-questions)
  :custom
  (sx-cache-directory "~/.private/sx")
  :custom-face
  (sx-question-mode-content-face ((t (:background nil)))))

(use-package nov :ensure t :mode ("\\.epub\\'" . nov-mode))

(use-package pocket-reader
  :ensure t
  :bind
  ("C-c p" . pocket-reader)
  (:map pocket-reader-mode-map
        ("c") ; the default binding of c is "unemacsy"
        ("w" . pocket-reader-copy-url))
  :custom
  (pocket-reader-open-url-default-function #'eww)
  (pocket-reader-pop-to-url-default-function #'eww))

(use-package pocket-reader-extras :after pocket-reader)

(use-package embark-pocket-reader :after (pocket-reader embark))

(use-package elfeed
  :ensure t
  :bind
  ("C-c w" . elfeed)
  (:map elfeed-search-mode-map
        ("y") ; Wellons is brilliant but he confused yank & save
        ("w" . elfeed-search-yank)
        ("P" . pocket-reader-elfeed-search-add-link)
        ("SPC" . scroll-up-command)
        ("S-SPC" . scroll-down-command))
  (:map elfeed-show-mode-map
        ("y") ; Again...
        ("P" . pocket-reader-elfeed-entry-add-link)
        ("E" . elfeed-show-play-enclosure)
        ("w" . elfeed-show-yank)
        ("S-SPC" . scroll-down-command)
        ("{" . backward-paragraph)
        ("}" . forward-paragraph)
        ("C-c C-p" . shr-heading-previous)
        ("C-c C-n" . shr-heading-next))
  :hook
  (elfeed-show-mode . visual-line-mode)
  (elfeed-show-mode . shr-heading-setup-imenu))

(use-package elfeed-extras
  :after elfeed
  :bind
  (:map elfeed-search-mode-map
        ("c" . elfeed-extras-comments)
        ("l" . elfeed-extras-link)
        ("v" . elfeed-extras-arxiv-pdf)
        ("y" . elfeed-extras-youtube))
  (:map elfeed-show-mode-map
        ("c" . elfeed-extras-comments)
        ("l" . elfeed-extras-link)
        ("v" . elfeed-extras-arxiv-pdf)
        ("y" . elfeed-extras-youtube)))

(use-package embark-elfeed :after (elfeed embark))

(use-package elfeed-config :after elfeed) ; private package

(use-package osm
  :ensure t
  :defer t
  :custom
  (osm-tile-directory "~/.cache/osm")
  :config
  (unless (fboundp 'json-parse-string)
    (cl-defun json-parse-string (string &key
                                        (object-type 'hash-table)
                                        (array-type 'array)
                                        (null-object :null)
                                        (false-object :false))
      (let ((json-object-type object-type)
            (json-array-type (if (eq array-type 'array) 'vector array-type))
            (json-null null-object)
            (json-false false-object)
            (json-key-type nil))
        (json-read-from-string string)))))

(use-package osm-ol :after org :demand t)

;;; major modes for programming languages

(use-package elisp-mode
  :custom
  (lisp-indent-function #'hybrid-lisp-indent-function)
  :config
  (add-to-list 'lisp-imenu-generic-expression
               '("Embark Keymaps"
                 "^\\s-*(embark-define-keymap\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
                 1)
               t)
  (defun hybrid-lisp-indent-function (indent-point state)
    "An indent function for Emacs Lisp that handles some cl constructs.
It handles `cl-labels', `cl-flet' and `cl-macrolet'.
For those constructs, it uses `common-lisp-indent-function', for
everything else, it uses `lisp-indent-function'."
    (if (save-excursion
          (cl-loop for pt in (nth 9 state)
                   do (goto-char pt)
                   thereis (looking-at "(cl-\\(?:label\\|flet\\|macrolet\\)")))
        (common-lisp-indent-function indent-point state)
      (lisp-indent-function indent-point state))))

(use-package python
  :defer t
  :custom
  (python-shell-interpreter "python3"))

(use-package cperl-mode
  :mode "\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'"
  :interpreter "\\(mini\\)?perl5?"
  :bind
  (:map cperl-mode-map
        ([remap display-local-help] . cperl-perldoc)))

(use-package sly
  :ensure t
  :bind
  (:map sly-editing-mode-map
        ([remap display-local-help] . sly-describe-symbol)
        ([remap embark-pp-eval-defun] . sly-compile-defun)
        ([remap pp-macroexpand-expression] . sly-expand-1)
        ([remap pp-eval-expression] . sly-interactive-eval)
        ([remap xref-find-definitions] . sly-edit-definition))
  (:map sly-prefix-map
        ("I") ("E") ; C-c LETTER are reserved!
        ("C-v i" . sly-inspect)
        ("C-v e" . sly-edit-value))
  :custom
  (inferior-lisp-program "sbcl")
  :hook
  (sly-mode . turn-off-sly-symbol-completion-mode)
  :config
  (defun turn-off-sly-symbol-completion-mode ()
    (sly-symbol-completion-mode -1)))

(use-package sly-mrepl
  :bind
  (:map sly-mrepl-mode-map
        ([remap display-local-help] . sly-describe-symbol)
        ([remap embark-pp-eval-defun] . sly-compile-defun)
        ([remap pp-macroexpand-expression] . sly-expand-1)
        ([remap pp-eval-expression] . sly-interactive-eval)
        ([remap xref-find-definitions] . sly-edit-definition)))

(use-package sly-package-fu
  :bind
  (:map sly-prefix-map
        ("C-s i" . sly-import-symbol-at-point)
        ("C-s x" . sly-export-symbol-at-point))
  :config
  (defun sly-package-fu-fix-keys () ; C-c LETTER are reserved!
    (define-key sly-mode-map (kbd "C-c i") nil)
    (define-key sly-mode-map (kbd "C-c x") nil))
  (advice-add 'sly-package-fu-init :after #'sly-package-fu-fix-keys))

(use-package sly-trace-dialog
  :bind
  (:map sly-trace-dialog-shortcut-mode-map
        ("C-c T") ; C-c LETTER are reserved!
        ("C-c M-t" . sly-trace-dialog)
        ("C-c C-M-t" . sly-toggle-fancy-trace)))

(use-package clojure-mode :ensure t :defer t)

(use-package cicio-mode :mode "\\.ci\\'" :commands run-cicio)

(when (or (executable-find "lua") (executable-find "luajit"))
  (use-package lua-mode
    :ensure t
    :defer t
    :custom
    (lua-indent-level 2)
    (lua-default-application "luajit")))

(when (executable-find "julia")
  (use-package julia-mode
    :ensure t
    :defer t
    :config
    (defun run-julia ()
      "Just run julia in a term buffer."
      (interactive)
      (switch-to-buffer (make-term "julia" "julia"))
      (term-mode)
      (term-char-mode))))

(when (executable-find "sage")
  (defun sage-notebook ()
    "Start a Sage notebook.
This makes a buffer to communicate with the Sage kernel, useful
to shut it down, for example."
    (interactive)
    (bury-buffer
     (process-buffer
      (start-process "sage-notebook" "*sage*" "sage" "--notebook=jupyter")))))

(use-package j-mode
  :bind ("C-c j" . run-j)
  :mode "\\.ijs\\'"
  :commands j-mode)

(use-package gap-mode :ensure t)

(use-package gap-config :after gap-mode) ; private package that just sets paths
