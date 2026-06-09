;;; -*- lexical-binding: t -*-

;;; Customize thinks it knows better than me

(setq custom-file (make-temp-file "emacs-custom-"))

;;; GUI

(setopt
 inhibit-startup-screen t
 initial-scratch-message nil
 menu-bar-mode nil
 tool-bar-mode nil
 scroll-bar-mode nil
 use-dialog-box nil
 ring-bell-function #'ignore
 cursor-type 'bar
 tab-bar-show nil
 tab-bar-close-button-show nil)

(set-face-attribute 'variable-pitch nil :family "Noto Sans")

(set-fontset-font "fontset-default" 'emoji
                  (font-spec :family "Noto Color Emoji"))

;;; package.el & use-package setup

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(use-package bind-key
  :bind (:map help-map ("y" . describe-personal-keybindings)))

(setopt use-package-vc-prefer-newest t
        package-install-upgrade-built-in t)

(add-to-list 'load-path "~/.private/")

;;; misc

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

(setopt
 mode-line-collapse-minor-modes t
 use-package-enable-imenu-support t
 set-mark-command-repeat-pop t
 tab-always-indent 'complete
 current-language-environment "UTF-8"
 after-save-hook '(executable-make-buffer-file-executable-if-script-p)
 column-number-indicator-zero-based nil
 scroll-preserve-screen-position t
 make-backup-files nil
 save-interprogram-paste-before-kill t
 sentence-end-double-space nil
 words-include-escapes t
 indent-tabs-mode nil
 standard-indent 2
 view-read-only t
 kill-read-only-ok t
 kill-whole-line t
 truncate-lines t
 history-delete-duplicates t
 kill-do-not-save-duplicates t
 help-window-select t
 default-input-method "TeX"
 default-transient-input-method "TeX"
 password-cache-expiry 300
 debugger-stack-frame-as-list t
 split-width-threshold 140
 y-or-n-p-use-read-key t
 use-short-answers t
 async-shell-command-display-buffer nil
 revert-without-query '("")
 recenter-positions '(top middle bottom)
 display-time-default-load-average nil
 display-time-mail-string ""
 native-comp-async-report-warnings-errors 'silent
 grep-use-headings t)

(bind-keys
 ("C-d" . delete-forward-char)
 ("M-K" . kill-paragraph)
 ("M-H" . mark-paragraph) ; for REPLs where I use M-h for consult-history  
 ("M-Q" . unfill-paragraph)
 ("M-Z" . zap-to-char)
 ("C-x k" . kill-current-buffer)
 ("C-x /" . pwd)
 ("C-x C-/" . cd)
 ("C-x C-p" . list-packages)
 ("M-r" . kmacro-start-macro-or-insert-counter)
 ("M-m" . kmacro-end-or-call-macro)
 ("C-x C-k i" . insert-kbd-macro)
 ("M-i" . back-to-indentation)
 ("M-'" . delete-indentation)
 ("M-s k" . keep-lines)
 ("M-s f" . flush-lines)
 ("M-s c" . count-matches)
 ("C-;" . comment-dwim)
 ("C-z" . repeat)
 ("C-<tab>" . tab-next)
 ("C-<iso-lefttab>" . tab-previous)
 ("C-h p" . describe-package)  ; swap these two
 ("C-h P" . finder-by-keyword)
 ("C-\\" . activate-transient-input-method) ; swap these two, too
 ("C-x \\" . toggle-input-method)
 ("C-x M-c" . restart-emacs)
 ([remap count-words-region] . count-words)
 ("C-M-o" . up-list)
 ("C-o" . split-line)
 ("M-o" . other-window)
 ("M-R" . kill-backward-up-list)
 ("M-T" . transpose-sentences)
 ("C-x M-t" . transpose-paragraphs)
 ([remap apropos-command] . apropos)
 ("C-x t s" . toggle-frame-tab-bar)
 ("C-x x #" . recover-this-file))

(bind-keys :prefix-map insert-pair-map
           :prefix "M-["
           ([t] . insert-pair))

(define-advice insert-pair (:filter-args (args) numeric-prefix)
  (cons (prefix-numeric-value (car args)) (cdr args)))

(bind-keys :prefix-map toggle-map
           :prefix "C-c x"
           :prefix-docstring "Keymap for commands that toggle settings."
           ("c" . column-number-mode)
           ("d" . toggle-debug-on-error)
           ("f" . follow-mode)
           ("w" . whitespace-mode)
           ("v" . variable-pitch-mode)
           ("l" . visual-line-mode)
           ("r" . visible-mode)
           ("s" . mode-line-invisible-mode))

(bind-keys :prefix-map time-map
           :prefix "C-c t"
           :prefix-docstring "Keymap for commands that deal with time."
           ("w" . world-clock)
           ("t" . display-time-mode)
           ("c" . calendar)
           ("o" . org-timer-set-timer)
           ("p" . org-timer-pause-or-continue)
           ("s" . org-timer-stop))

(use-package package
  :defer t
  :config
  (define-advice package-menu--list-to-prompt
      (:around (fn &rest args) just-package-names)
    (cl-letf (((symbol-function 'package-desc-full-name)
               (lambda (pkg) (symbol-name (package-desc-name pkg)))))
      (apply fn args))))

;;; gui packages

(use-package modus-themes
  :init
  (require-theme 'modus-themes)
  (load-theme (car modus-themes-to-toggle))
  :bind
  ("C-c x b" . modus-themes-toggle)
  :custom
  (modus-themes-common-palette-overrides '((fringe unspecified)))
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-bold-constructs t)
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)))

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((modus-vivendi-tinted) (modus-operandi-tinted)))
  :init
  (auto-dark-mode))

(use-package spacious-padding
  :ensure t
  :custom
  (spacious-padding-widths
   '( :internal-border-width 15
      :header-line-width 4
      :mode-line-width 3                ; half the default
      :tab-width 4
      :right-divider-width 15           ; half the default
      :scroll-bar-width 8))
  :init (spacious-padding-mode))

(use-package kkp
  :ensure t
  :hook (tty-setup . global-kkp-mode))

;;; packages

(use-package imenu
  :defer t
  :custom (imenu-space-replacement nil))

(use-package info
  :defer t
  :bind
  (:map Info-mode-map
        ("{" . backward-paragraph)
        ("}" . forward-paragraph))
  :custom-face
  (Info-quoted ((t :inherit fixed-pitch))))

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

(use-package repeat
  :init (repeat-mode)
  :config
  (put 'other-window 'repeat-map nil))

(use-package misc
  :bind
  ("M-z" . zap-up-to-char)
  ("M-F" . forward-to-word)
  ("M-B" . backward-to-word)
  ("C-M-<" . mark-beginning-of-buffer)
  ("C-M->" . mark-end-of-buffer)
  ("C-S-l" . copy-from-above-command)
  ("C-S-d" . duplicate-dwim))

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
        ("_" . calc-grab-sum-across)
        (" " . delete-whitespace-rectangle)))

(use-package outline
  :bind
  ("<backtab>" . outline-cycle-buffer))

(use-package visiting-buffer)

(use-package text-extras
  :bind
  ("C-S-o" . copy-word-from-above)
  ("C-S-k" . duplicate-line-kill-word)
  ("M-L" . mark-line)
  ("M-C" . mark-char)
  ("M-@" . mark-my-word)
  ("M-g r" . goto-random-line)
  ("C-M--" . kill-inside-sexp)
  ("C-M-=" . mark-inside-sexp)
  ("M-U" . unwrap-sexp)
  ("M-S" . unwrap-mark-sexp)
  ("C-|" . pipe-region)
  ("C-S-f" . forward-to-whitespace)
  ("C-S-b" . backward-to-whitespace)
  ("M-W" . mark-non-whitespace)
  ("M-M" . apply-macro-to-lines-of-paragraph)
  ("M-;" . dabbrev-next)
  ("C-M-;" . dabbrev-complete-next)
  ("C-c u" . text-to-clipboard)
  ([remap upcase-word] . upcase-dwiw)
  ([remap downcase-word] . downcase-dwiw)
  ([remap capitalize-word] . capitalize-dwiw))
  
(use-package placeholder
  :vc (:url "https://github.com/oantolin/placeholder.git")
  :bind
  ("M-_" . placeholder-insert)
  ("C-S-n" . placeholder-forward)
  ("C-S-p" . placeholder-backward))

(use-package gptel
  :ensure t
  :bind
  (:prefix-map global-gptel-map :prefix "C-c i"
               ("c" . gptel)
               ("s" . gptel-send)
               ("r" . gptel-rewrite)
               ("a" . gptel-add)
               ("m" . gptel-mode)
               ("p" . gptel-menu)
               ("x" . gptel-context-remove-all))
  :custom
  (gptel-org-branching-context t)
  (gptel-prompt-prefix-alist
   '((markdown-mode . "> ") (org-mode . "> ") (text-mode . "> ")))
  (gptel-include-reasoning nil)
  :config
  (gptel-make-gemini "Gemini" :key gptel-api-key :stream t)
  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key gptel-api-key
    :models '(llama-3.3-70b-versatile
              llama-3.1-8b-instant
              openai/gpt-oss-120b
              openai/gpt-oss-20b
              qwen/qwen3-32b
              groq/compound
              groq/compound-mini
              deepseek-r1-distill-llama-70b
              meta-llama/llama-4-scout-17b-16e-instruct
              meta-llama/llama-4-maverick-17b-128e-instruct
              gemma2-9b-it))
  (setq gptel-model 'qwen/qwen3-32b
        gptel-backend (cdr (assoc "Groq" gptel--known-backends))))

(use-package gptel-extras
  :bind
  (:map global-gptel-map
        ("q" . gptel-extras-mini)
        ("d" . gptel-extras-define)))

(use-package whisper
  :vc (:url "https://github.com/natrys/whisper.el" :branch "master")
  :bind
  ("<insert>" . whisper-run)
  ("C-x C-l" . whisper-select-language))

(use-package isearch-extras
  :custom
  (search-whitespace-regexp ".*?")
  (isearch-allow-scroll 'unlimited)
  (isearch-lazy-count t)
  :bind
  ("M-n" . isearch-next)
  ("M-p" . isearch-previous)
  (:map isearch-mode-map
        ("M-c") ; free up for capitalize-dwim, still bound to M-s c
        ("M-r") ; free up for recording kmacros, still bound to M-s r
        ("<S-return>" . isearch-exit-at-end)
        ([remap isearch-abort] . isearch-cancel)
        ("<C-backspace>" . isearch-delete-wrong)
        ("C-M-w" . isearch-yank-region))
  :hook
  (isearch-mode-end . isearch-exit-at-start))

(use-package math-delimiters
  :vc (:url "https://github.com/oantolin/math-delimiters.git")
  :bind
  (:map toggle-map
        ("$" . math-delimiters-toggle)))

(use-package block-undo
  :config
  (advice-add 'message-insert-signature :around #'block-undo))

(use-package help-extras
  :bind
  (:map help-map ("h" . show-help)))

(use-package window-extras
  :bind
  (:map ctl-x-5-map
        ("s" . screenshot)))

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
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :init
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode)
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  :config
  (define-advice minibuf-eldef-setup-minibuffer
      (:around (fn &rest args) stealthily)
    "Apply FN to ARGS while inhibiting modification hooks."
    (let ((inhibit-modification-hooks t))
      (apply fn args))))

(use-package orderless
  :ensure t
  :demand t
  :config
  (defun prefixes-for-separators (pattern _index _total)
    (when (string-match-p "^[^][^\\+*]*[./-][^][\\+*$]*$" pattern)
      (cons 'orderless-prefixes pattern)))
  (cl-pushnew '(?` . orderless-regexp) orderless-affix-dispatch-alist)
  :custom
  (orderless-style-dispatchers
   '(orderless-affix-dispatch prefixes-for-separators)))

(use-package vertico
  :ensure t
  :bind
  (:map vertico-map
        ("DEL" . vertico-directory-delete-char)
        ("C-M-d" . consult-dir)
        ("C-M-j" . consult-dir-jump-file)
        ("M-q" . vertico-quick-exit))
  (:map vertico-multiform-map
        ("M-F") ("M-B")                     ; I use these for WORD motions
        ("M-H" . vertico-multiform-flat)    ; H for horizontal
        ("M-P" . vertico-multiform-buffer)) ; P for panoramic
  :custom
  (vertico-multiform-categories
   '((embark-keybinding grid)
     (command flat)
     (file grid)))
  (vertico-multiform-commands
   '((org-set-tags-command grid)
     (org-agenda-set-tags grid)
     (TeX-command-master flat)))
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode))

(use-package marginalia
  :ensure t
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (cl-pushnew '(org-goto . org-heading) marginalia-command-categories)
  (cl-pushnew '(org-refile . org-heading) marginalia-command-categories)
  (defun marginalia--file-owner (attrs) ; Only display UID
    "Return file owner given ATTRS."
    (let ((uid (file-attribute-user-id attrs)))
      (when (/= (user-uid) uid)
        (or (user-login-name uid) uid)))))

(use-package cape
  :ensure t
  :bind ("C-'" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package ecomplete-extras
  :bind
  ("C-x M" . compose-mail-to))

(use-package arXiv
  :init
  (defvar-keymap arXiv-map
    "p" #'arXiv-pdf
    "s" #'arXiv-show
    "w" #'arXiv-copy-url
    "c" #'arXiv-capture)
  (fset 'arXiv-map arXiv-map))

(use-package scimago
  :vc (:url "https://github.com/oantolin/scimago.git")
  :bind
  ("C-c q" . scimago-copy-quartiles))

(use-package embark
  :ensure t
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  (:map help-map
        ("b" . embark-bindings)
        ("B" . embark-bindings-at-point)
        ("M" . embark-bindings-in-keymap)
        ("E" . embark-on-last-message))
  (:map completion-list-mode-map
        ("." . embark-act))
  (:map embark-collect-mode-map
        ("a") ; I don't like my own default :)
        ("." . embark-act)
        ("F" . consult-focus-lines))
  (:map embark-package-map
        ("t" . try))
  (:map embark-identifier-map
        ("(" . insert-parentheses)
        ("[" . insert-pair-map))
  (:map embark-expression-map
        ("(" . insert-parentheses)
        ("[" . insert-pair-map))
  (:map embark-region-map
        ("(" . insert-parentheses)
        ("[" . insert-pair-map)
        ("=" . quick-calc))
  (:map embark-heading-map
        ("SPC" . outline-mark-subtree)
        ("C-SPC" . embark-select))        
  (:map embark-email-map
        ("+" . add-email-to-ecomplete)
        ("\\" . remove-email-from-ecomplete))
  (:map embark-encode-map
        ("p" . topaz-paste-region))
  (:map embark-url-map
        ("a" . arXiv-map)
        ("m" . mastodon-url-lookup))
  (:map embark-general-map
        ("SPC" . mark)
        ("C-SPC" . embark-select)
        ("D" . gptel-extras-define))
  :custom
  (embark-quit-after-action nil)
  ;; (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key ".")
  (embark-help-key "?")
  (embark-confirm-act-all nil)
  (embark-auto-prefix-help-delay 1.0)
  :init
  (embark-auto-prefix-help-mode)
  :config
  ;; want sentence and paragraph targets in more modes
  (embark-define-thingatpt-target sentence
    text-mode help-mode Info-mode man-common mastodon-mode
    lem-mode emacs-news-view-mode)
  (embark-define-thingatpt-target paragraph
    text-mode help-mode Info-mode man-common mastodon-mode
    lem-mode emacs-news-view-mode)
  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (dolist (cmd '(markdown-insert-code
                 markdown-insert-italic
                 markdown-insert-bold
                 cdlatex-math-modify
                 TeX-font
                 send-to-comint))
    (push #'embark--mark-target (alist-get cmd embark-around-action-hooks)))
  (push #'embark--xref-push-marker
        (alist-get 'find-file embark-pre-action-hooks))
  (add-to-list 'embark-keymap-alist '(ecomplete . embark-email-map))
  (defun embark-on-last-message (arg)
    "Act on the last message displayed in the echo area."
    (interactive "P")
    (with-current-buffer "*Messages*"
      (goto-char (1- (point-max)))
      (cl-letf (((symbol-function #'embark--end-of-target) #'ignore))
        (embark-act arg))))
  (cl-defun embark--goto-location (&key target &allow-other-keys)
    "Jump to location of the TARGET."
    (goto-char (car (consult--get-location target))))
  (dolist (cmd '(kmacro-end-or-call-macro
                 kmacro-end-and-call-macro))
    (push #'embark--goto-location (alist-get cmd embark-pre-action-hooks))))

(use-package embark-consult
  :ensure t
  :defer t)

(use-package embark-org
  :bind
  (:map embark-org-link-map
        ("a" . arXiv-map))
  (:map embark-org-heading-map
        ("a" . org-archive-subtree-default)) ; skip confirmation
  (:map embark-org-src-block-map
        ("SPC" . org-babel-mark-block) ("C-SPC")))

(use-package consult-dir
  :ensure t
  :bind
  (:map minibuffer-local-filename-completion-map
        ("C-M-d" . consult-dir)
        ("C-M-j" . consult-dir-jump-file)))

(use-package consult
  :ensure t
  :bind
  ("M-y" . consult-yank-pop)
  ("M-X" . consult-mode-command)
  ("C-c b" . consult-buffer)
  ("C-c 4 b" . consult-buffer-other-window)
  ("C->" . consult-register-store)
  ("C-," . consult-register-load)
  ("C-M-," . consult-register)
  (:map goto-map
        ("l" . consult-line)
        ("L" . consult-line-multi)
        ("i" . consult-imenu)
        ("o" . consult-outline)
        ("a" . consult-org-agenda)
        ("I" . consult-imenu-multi)
        ("m" . consult-mark)
        ("k" . consult-global-mark)
        ("f" . consult-find))
  (:map search-map
        ("g" . consult-grep)
        ("G" . consult-git-grep)
        ("r" . consult-ripgrep)
        ("i" . consult-info)
        ("K" . consult-keep-lines)
        ("F" . consult-focus-lines))
  (:map minibuffer-local-map
        ("M-h" . consult-history)
        ("M-r") ("M-s"))
  (:map consult-narrow-map
        ("C-<" . consult-narrow-help))
  (:map isearch-mode-map
        ("M-g l" . consult-line))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (register-preview-function #'consult-register-format)
  (consult-narrow-key "<")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (advice-add #'register-preview :override #'consult-register-window)
  (setf (alist-get 'log-edit-mode consult-mode-histories)
        'log-edit-comment-ring))

(use-package consult-imenu
  :defer t
  :config
  (setf
   (alist-get
    ?k (plist-get (alist-get 'emacs-lisp-mode consult-imenu-config) :types))
   '("Keymaps" font-lock-variable-name-face)))

(use-package webjump
  :bind
  ("C-c w" . webjump))

(use-package webjump-extras
  :after webjump
  :demand t
  :bind
  ("C-c W" . webjump-reload)
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

(use-package beginend
  :ensure t
  :init
  (beginend-global-mode))

(use-package avy
  :ensure t
  :bind
  (("M-j" . avy-goto-char-timer)
   ([remap goto-line] . avy-goto-line))
  (:map isearch-mode-map
        ("M-q" . avy-isearch))
  :config
  (add-to-list 'avy-dispatch-alist '(?\, . avy-action-goto))
  (defun avy-embark-act (pt)
    "Use Embark to act on the item at PT."
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0)))
      t))
  (add-to-list 'avy-dispatch-alist '(?\. . avy-embark-act))
  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))
  (add-to-list 'avy-dispatch-alist '(?e . avy-action-exchange)))

(use-package link-hint
  :vc ( :url "https://github.com/pjones/link-hint.el.git"
        :rev "pjones/fix-avy-action")
  :bind
  ("C-S-j" . link-hint-avy-act))

(use-package paren
  :custom
  (show-paren-context-when-offscreen t)
  :init
  (show-paren-mode))

(use-package find-func
  :init
  (find-function-setup-keys))

(use-package text-mode
  :hook
  (text-mode . variable-pitch-mode)
  :config
  (modify-syntax-entry ?\" "\"" text-mode-syntax-table))

(use-package emacs-news-view-mode
  :mode "\\`NEWS"
  :config
  ;; NEWS files use single quotes around elisp symbols. I think those
  ;; are the only files I view in outline-mode, but if I find others
  ;; then I might modify the syntax only locally in NEWS files.
  (modify-syntax-entry ?' "\"" emacs-news-view-mode-syntax-table))

(use-package diff-mode
  :bind
  (:map diff-mode-map
        ("M-o") ; diff-goto-source is also bound to o
        ("v" . vc-next-action)
        ("/" . diff-split-hunk)))

(use-package ediff
  :defer t
  :custom
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package olivetti
  :ensure t
  :bind
  (:map toggle-map
        ("o" . olivetti-mode)))

(use-package shr
  :bind
  (:map shr-map
        ("v")) ; don't override view-source with a useless synonym for RET
  :custom
  (shr-use-colors nil)
  (shr-image-animate nil)
  :custom-face
  (shr-text ((t (:inherit variable-pitch))))
  (shr-code ((t (:inherit fixed-pitch)))))

(use-package eww
  :bind
  (:map eww-mode-map
        ("{" . backward-paragraph)
        ("}" . forward-paragraph))
  :custom
  (eww-bookmarks-directory "~/.private/")
  (eww-auto-rename-buffer 'title)
  :hook
  (eww-mode . echo-area-tooltips)
  :config
  (modify-syntax-entry ?\“ "(”" eww-mode-syntax-table)
  (modify-syntax-entry ?\” ")“" eww-mode-syntax-table)
  (define-advice eww-display-pdf (:around (fn &rest args) pdfs-are-binary)
    (let ((buffer-file-coding-system 'binary))
      (apply fn args))))

(use-package latex
  :ensure auctex
  :bind
  (:map LaTeX-mode-map
        ("$" . math-delimiters-insert)
        ("C-=" . TeX-font)
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
  (LaTeX-mode . prettify-symbols-mode)
  (LaTeX-mode . add-prettify-symbols-hook)
  :config
  (defun LaTeX-outline-name ()
    "Guess a name for the current header line."
    (save-excursion
      (search-forward "{" nil t)
      (let ((start (point)))
        (backward-char)
        (condition-case nil
            (with-syntax-table (TeX-search-syntax-table ?\{ ?\})
              (forward-sexp)
              (backward-char))
          (error (forward-sentence)))
        (replace-regexp-in-string
         "[\n\r][ ]+" " "
         (buffer-substring start (point))))))
  (defun make-backslash-a-prefix-in-LaTeX ()
    "Set the syntax class of \\ to ' in LaTeX buffers."
    (modify-syntax-entry ?\\ "'" LaTeX-mode-syntax-table))
  (defun toggle-prettify-symbols ()
    "Toggle `prettify-symbols-mode'."
    (prettify-symbols-mode 'toggle))
  (defun add-prettify-symbols-hook ()
    "Add toggling `prettify-symbols-mode' to local value of `visible-mode-hook'."
    (add-hook 'visible-mode-hook 'toggle-prettify-symbols nil t))
  (setcdr (assq 'output-pdf TeX-view-program-selection)
          '("PDF Tools")))

(use-package cdlatex
  :ensure t
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
  :bind
  (:map pdf-view-mode-map
        ("i" . consult-imenu)
        ("t" . pdf-view-themed-minor-mode)
        ("d" . pdf-view-midnight-minor-mode)
        ("c" . pdf-view-roll-minor-mode)
        ("s n" . "nsbp")     ; is this my only keyboard macro binding?
        ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page)
        ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page))
  :init
  (pdf-loader-install)
  :hook
  (pdf-view-mode . pdf-view-themed-minor-mode)
  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (define-advice image-mode-window-get
      (:around (fn prop &optional winprops) default-1-page)
    (or (funcall fn prop winprops)
        (and (eq prop 'page) 1))))

(use-package pdf-annot
  :defer t
  :custom
  (pdf-annot-minor-mode-map-prefix "a")
  (pdf-annot-list-format '((page . 3) (type . 7) (contents . 200)))
  (pdf-annot-activate-created-annotations t))

(use-package pdf-outline
  :defer t
  :custom
  (pdf-outline-imenu-use-flat-menus t)
  :config
  (define-advice pdf-outline-imenu-create-item
      (:around (fn link &optional labels) indent)
    (let ((item (funcall fn link labels))
          (indent pdf-outline-buffer-indent))
      (cons (concat (make-string (* indent (1- (alist-get 'depth link))) ?\s)
                    (car item))
            (cdr item)))))

(use-package dired
  :bind
  (:map dired-mode-map
        ("e" . dired-open-externally))
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alGh")
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (defun dired-open-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks
     (embark-open-externally (dired-get-filename))
     arg)))

(use-package comint
  :custom
  (comint-prompt-read-only t) 
  :bind
  (:map comint-mode-map
        ("M-h" . consult-history)
        ("M-r") ("M-s")))

(use-package man-help)

(use-package eshell
  :bind
  ("C-!" . eshell)
  :config
  (setenv "PAGER" "cat")
  (setq eshell-modules-list (delq 'eshell-banner eshell-modules-list)))

(use-package esh-mode
  :bind
  (:map eshell-mode-map
        ("C-c d" . interactive-cd)
        ("M-q" . quit-window)
        ([remap display-local-help] . man)))

(use-package em-hist
  :bind
  (:map eshell-hist-mode-map
        ("M-h" . consult-history)
        ("M-r") ("M-s"))
  :custom (eshell-hist-ignoredups t))

(use-package ffap-eshell :after (eshell ffap))

(use-package shell
  :bind (:map shell-mode-map
              ([remap display-local-help] . man)
              ("C-c d" . interactive-cd)))

(use-package apt-progress
  :after (:any eshell shell))

(use-package sh-script
  :bind (:map sh-mode-map
              ([remap display-local-help] . man)))

(use-package term
  :bind
  (:map term-mode-map
        ("C-c d" . interactive-cd)
        ("M-h" . consult-history)
        ("M-r") ("M-s"))
  (:map term-raw-map
        ("C-c d" . interactive-cd)
        ("M-h" . consult-history)
        ("M-r") ("M-s")))

(use-package log-edit
  :bind
  (:map log-edit-mode-map
        ("M-h" . consult-history)
        ("M-r") ("M-s"))
  :hook
  (log-edit-mode . turn-on-auto-fill)
  :config
  (define-advice consult-history (:before (&optional _) clear-log-edit-buffer)
    (when (derived-mode-p 'log-edit-mode)
      (erase-buffer)))
  (remove-hook 'log-edit-hook #'log-edit-show-files))

(use-package vc
  :bind
  (:map vc-prefix-map
        ("d" . vc-dir-root)
        ("c" . vc-git-commit)))

(use-package vc-dir
  :bind
  (:map vc-dir-mode-map
        ("c" . vc-git-commit)))

(use-package smerge-mode
  :defer t
  :custom
  (smerge-command-prefix "\C-xc"))

(use-package magit
  :ensure t
  :bind
  (:map vc-prefix-map
        ("b d" . magit-branch-delete))
  :custom (magit-diff-refine-hunk 'all))

(use-package markdown-mode
  :ensure t
  :bind
  (:map markdown-mode-map
        ("C-=" . markdown-mode-style-map))
  :hook
  (markdown-mode . turn-on-visual-line-mode)
  :custom
  (markdown-hide-markup t)
  :custom-face
  (markdown-metadata-key-face ((t (:inherit default))))
  (markdown-metadata-value-face
   ((t (:inherit default :foreground unspecified))))
  :config
  (fset 'markdown-mode-style-map markdown-mode-style-map)
  (modify-syntax-entry ?\" "\"" markdown-mode-syntax-table))

(use-package org
  :ensure t
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c s" . org-store-link)
   ("C-c C" . org-clock-goto))
  (:map org-mode-map
        ("C-,") ; I use this for consult-register-load
        ("C-'") ; I use this as a prefix for cape
        ("C-c C-=" . org-cycle-agenda-files)
        ("$" . math-delimiters-insert)
        ("C-$" . ispell-complete-word)
        ("C-=" . org-emphasize)
        ("M-g o" . consult-org-heading)
        ("C-M-<return>" . org-insert-subheading)
        ("C-M-S-<return>" . org-insert-todo-subheading))
  :custom
  (org-ellipsis "…")
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-startup-indented t)
  (org-highlight-latex-and-related '(latex script entities))
  (org-export-with-smart-quotes t)
  (org-confirm-babel-evaluate nil)
  (org-export-async-init-file "~/.emacs.d/my-lisp/org-async-init.el")
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  (org-entities-user '(("newline" "\\newline" nil "<br>" "\n" "\n" "⏎")))
  (org-preview-latex-image-directory "~/.cache/ltximg/")
  (org-tags-column 0)
  (org-auto-align-tags nil)
  (org-use-speed-commands t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-use-time-grid nil)
  :hook
  (org-mode . turn-on-org-cdlatex)
  (org-mode . ediff-with-org-show-all)
  (org-mode . turn-on-auto-fill)
  (org-mode . org-tweak-syntax-table)
  (org-mode . add-pretty-entities-hook)
  (org-mode . echo-area-tooltips)
  :config
  (cl-pushnew 'org-habit org-modules)
  (defun ediff-with-org-show-all ()
    "Expand all headings prior to ediffing org buffers."
    (add-hook 'ediff-prepare-buffer-hook #'org-fold-show-all nil t))
  (defun add-pretty-entities-hook ()
    "Add `org-toggle-pretty-entities' to local value of `visible-mode-hook'."
    (add-hook 'visible-mode-hook 'org-toggle-pretty-entities nil t))
  (setopt org-structure-template-alist
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
  (setopt org-latex-default-packages-alist
          (cl-set-difference org-latex-default-packages-alist
                             '("fontenc" "textcomp")
                             :test #'equal))
  (setopt org-latex-packages-alist
          (cons '("AUTO" "babel" t ("pdflatex")) org-latex-packages-alist))
  (when (executable-find "texi2pdf")
    (setopt org-latex-pdf-process '("texi2pdf %f")))
  (defun org-tweak-syntax-table ()
    (cl-loop for (ch cl) in '((?< ".") (?> ".") (?\\ "'")
                              (?' "'") (?« "(»") (?» ")«"))
             do (modify-syntax-entry ch cl org-mode-syntax-table)))
  (define-advice org-open-at-point-global
      (:around (fn &optional arg) when-in-org-do-as-the-organs-do)
    (if (derived-mode-p 'org-mode) (org-open-at-point arg) (funcall fn arg)))
  (org-link-set-parameters
   "org-title"
   :store (defun store-org-title-link ()
            "Store a link to the org file visited in the current buffer.
Use the #+TITLE as the link description. The link is only stored
if `org-store-link' is called from the #+TITLE line."
            (when (and (derived-mode-p 'org-mode)
                       (save-excursion
                         (beginning-of-line)
                         (looking-at "#\\+\\(?:TITLE\\|title\\):")))
              (org-link-store-props
               :type "file"
               :link (concat "file:" (buffer-file-name))
               :description (cadar (org-collect-keywords '("TITLE"))))))))

(use-package ob-python
  :defer t
  :custom                      ; Why is the following not the default?
  (org-babel-python-command-nonsession python-shell-interpreter))

(use-package org-config :after org) ; private package

(use-package org-ql
  :ensure t
  :bind
  (:map search-map
        ("q" . org-ql-find)
        ("n" . org-ql-find-in-org-directory)
        ("s" . org-ql-search)
        ("v" . org-ql-view)))

(use-package org-ql-usual-files
  :bind
  (:map search-map
        ("u" . org-ql-usual-files-find)
        ("l" . org-ql-usual-files-open-link)))

(use-package org-ql-completing-read
  :defer t
  :bind
  (:map org-ql-completing-read-map
        ([remap embark-collect])
        ("C-;" . org-ql-syntax-map))
  :config
  (defmacro inserter (string)
    "Command that inserts given STRING."
    `(lambda () ,string (interactive) (insert ,string)))
  (defvar-keymap org-ql-syntax-map
    "t" (inserter "todo:")
    "C" (inserter "clocked:")
    "h" (inserter "heading:")
    ":" (inserter "tags:")
    "s" (inserter "src:")
    "T" (inserter "ts:")
    "p" (inserter "priority:")
    "c" (inserter "category:")
    "l" (inserter "level:"))
  (fset 'org-ql-syntax-map org-ql-syntax-map))

(use-package org-indent
  :bind
  (:map toggle-map
        ("i" . org-indent-mode)))

(use-package org-modern
  :ensure t
  :after org
  :demand t
  :bind
  (:map toggle-map
        ("m" . org-modern-mode))
  :custom
  (org-modern-checkbox
   '((?X . "🞕") (?- . "⧄") (?\s . "🞎")))
  (org-modern-priority-faces
   '((?A :foreground "black" :background "plum")
     (?B :foreground "black" :background "khaki2")
     (?C :foreground "black" :background "azure2")))
  :custom-face
  (org-modern-label ((t (:height 0.8 :inherit fixed-pitch))))
   :config
   (global-org-modern-mode))

(use-package ox-rss :ensure t :defer t)
  
(use-package citeproc :ensure t :defer t)

(use-package jinx
  :ensure t
  :hook
  (emacs-startup . global-jinx-mode)
  :custom
  (jinx-languages "en es")
  :bind
  ("M-$" . jinx-correct)
  ("C-M-$" . jinx-languages))
  
(use-package try :ensure t :defer t)

(use-package logos
  :ensure t
  :custom
  (logos-outlines-are-pages t)
  :bind
  ([remap forward-page] . logos-forward-page-dwim)
  ([remap backward-page] . logos-backward-page-dwim)
  (:map narrow-map
        ("n" . logos-narrow-dwim)))

(use-package keycast
  :ensure t
  :bind (:map toggle-map
              ("k" . keycast-mode-line-mode)
              ("h" . keycast-header-line-mode))
  :config
  (define-advice embark-keymap-prompter
      (:filter-return (cmd) store-action-key+cmd)
    (force-mode-line-update t)
    (setq this-command cmd
          keycast--this-command-keys (this-single-command-keys)
          keycast--this-command-desc cmd))
  (define-advice embark-act (:before (&rest _) force-keycast-update)
    (keycast--update)))

;;; email packages

(use-package email-config) ; private package

(use-package gnus
  :bind
  ("C-c g" . gnus))

(fset 'goto-map goto-map)

(use-package gnus-group
  :config
  (define-keymap :keymap gnus-group-mode-map
    "M-g" #'goto-map
    "M-&" nil "M-n" nil "M-p" nil       ; I use these a lot!
    "C-&" #'gnus-group-universal-argument
    "T" #'gnus-group-get-new-news-this-group))

(use-package gnus-art
  :config
  (define-keymap :keymap gnus-article-mode-map
    "C-h b" nil            ; come on Gnus, that key binding is sacred!
    "M-&" nil              ; also pretty important
    "C-&" #'gnus-summary-universal-argument
    "M-g" #'goto-map
    "{" #'backward-paragraph
    "}" #'forward-paragraph))

(use-package gnus-sum
  :config
  (define-keymap :keymap gnus-summary-mode-map
    "M-&" nil        ; also pretty important
    "C-&" #'gnus-summary-universal-argument
    "M-i" nil        ; I use this for back-to-indentation
    "M-g" #'goto-map ; rescan is also on Z G, and I use that prefix a lot!
    "M-a" #'gnus-symbolic-argument))

(use-package ecomplete
  :defer t
  :custom
  (ecomplete-database-file "~/.private/ecompleterc")
  :config
  (setq completion-category-defaults nil))

(use-package message
  :bind (:map message-mode-map ("M-n"))
  :custom
  (message-signature nil)
  (message-mail-alias-type 'ecomplete)
  (message-self-insert-commands nil)
  (message-expand-name-standard-ui t)
  (message-fill-column nil)
  :hook
  (message-mode . turn-on-visual-line-mode))

(use-package message-extras
  :after message
  :bind
  (:map message-mode-map
        ("C-c x e" . cycle-from-address))
  :hook
  (message-send . set-smtp-server)
  (message-send . message-lint)
  :config
  (defvar-keymap cycle-from-address-repeat-map
    :repeat t
    "e" #'cycle-from-address))

;;; applications

(use-package sx
  :ensure t
  :defer t
  :init
  (defalias 'sx #'sx-tab-all-questions)
  :custom
  (sx-cache-directory "~/.private/sx")
  :custom-face
  (sx-question-mode-content-face ((t (:background unspecified)))))

(use-package nov :ensure t :mode ("\\.epub\\'" . nov-mode))

(use-package osm
  :ensure t
  :defer t
  :custom
  (osm-tile-directory "~/.cache/osm"))

(use-package ement
  :ensure t
  :custom
  (ement-notify-notification-predicates nil) ; stop DESKTOP notifications
  (ement-room-send-message-filter #'ement-room-send-org-filter)
  (ement-room-compose-method 'compose-buffer)
  (ement-room-compose-buffer-window-auto-height-min 5)
  (ement-room-use-variable-pitch t)
  :hook
  (ement-room-mode . variable-pitch-mode)
  :config
  (define-advice ement-room-send-org-filter
      (:around (fn &rest args) dumb-quotes)
    (let (org-export-with-smart-quotes)
      (apply fn args)))
  :bind
  (:prefix-map global-ement-map :prefix "C-c e"
               ("c" . ement-connect)
               ("d" . ement-disconnect)
               ("l" . ement-room-list)
               ("r" . ement-view-room)
               ("k" . ement-kill-buffers)
               ("n" . ement-notify-switch-to-notifications-buffer)
               ("m" . ement-notify-switch-to-mentions-buffer))
  (:map ement-room-mode-map
        ("e" . ement-room-dispatch-edit-message)
        ("p" . ement-room-goto-prev)
        ("n" . ement-room-goto-next)
        ("{" . backward-paragraph)
        ("}" . forward-paragraph)
        ("<" . beginning-of-buffer)
        (">" . end-of-buffer)))

(use-package mastodon
  :ensure t
  :bind
  (:prefix-map global-mastodon-map :prefix "C-c m"
               ("h" . mastodon)
               ("t" . mastodon-toot)
               ("n" . mastodon-notifications-get)
               ("k" . mastodon-profile-view-bookmarks)
               ("o" . mastodon-profile-my-profile)
               ("l" . mastodon-views-view-list-timeline)
               ("s" . mastodon-search-query)
               ("u" . mastodon-url-lookup)
               ("x" . mastodon-views-view-lists)
               ("@" . mastodon-notifications-get-mentions))
  (:map mastodon-mode-map
        ("C-:") ("i")
        ("*" . mastodon-toot-pin-toot-toggle)
        ("{" . backward-paragraph)
        ("}" . forward-paragraph))
  :custom
  (mastodon-group-notifications t)
  (mastodon-tl--fold-toots-at-length nil)
  :hook
  (mastodon-mode . mastodon-async-mode)
  (mastodon-mode . mastodon-recenter-positions)
  (mastodon-mode . variable-pitch-mode)
  (mastodon-toot-mode . turn-on-visual-line-mode)
  :config
  (defun mastodon-recenter-positions ()
    (setq-local recenter-positions '(bottom middle top))))

(use-package lem
  :ensure t
  :bind
  ("C-c l" . lem)
  (:map lem-mode-map
        ("{" . backward-paragraph)
        ("}" . forward-paragraph))
  :hook
  (lem-mode . variable-pitch-mode)
  (lem-mode . lem-imenu-setup)
  :custom
  (lem-highlight-current-item nil)
  (lem-default-listing-type "Subscribed")
  (lem-compose-autocomplete nil))

(use-package fedi-config :after (:any mastodon lem)) ; private package

;;; major modes for programming languages

(use-package elisp-mode
  :config
  (add-to-list
   'lisp-imenu-generic-expression
   '("Keymaps" "^\\s-*(defvar-keymap\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)
   t))

(use-package python
  :defer t
  :custom
  (python-shell-interpreter "python3")
  (python-interpreter "python3"))

(use-package scheme
  :defer t
  :custom
  (scheme-program-name "guile"))

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
  (sly-connected . increase-elision-length)
  :config
  (defun turn-off-sly-symbol-completion-mode ()
    (sly-symbol-completion-mode -1))
  (defun increase-elision-length ()
    (sly-eval
     '(cl:setf (cl:cdr (cl:assoc 'slynk:*string-elision-length*
                                 slynk:*slynk-pprint-bindings*))
               1000))))

(use-package sly-mrepl
  :bind
  (:map sly-mrepl-mode-map
        ([remap display-local-help] . sly-describe-symbol)
        ([remap embark-pp-eval-defun] . sly-compile-defun)
        ([remap pp-macroexpand-expression] . sly-expand-1)
        ([remap pp-eval-expression] . sly-interactive-eval)
        ([remap xref-find-definitions] . sly-edit-definition)
        ("M-h" . consult-history)))

(use-package sly-package-fu
  :bind
  (:map sly-prefix-map
        ("C-s i" . sly-import-symbol-at-point)
        ("C-s x" . sly-export-symbol-at-point))
  :config
  (define-advice sly-package-fu-init (:after () fix-keys)
    ;; C-c LETTER are reserved!
    (keymap-unset sly-mode-map "C-c i")
    (keymap-unset sly-mode-map "C-c x")))

(use-package sly-trace-dialog
  :bind
  (:map sly-trace-dialog-shortcut-mode-map
        ("C-c T") ; C-c LETTER are reserved!
        ("C-c M-t" . sly-trace-dialog)
        ("C-c C-M-t" . sly-toggle-fancy-trace)))

(use-package gap-mode
  :ensure t
  :custom
  (gap-executable "/usr/bin/gap"))

(use-package bqn-mode
  :mode "\\.bqn\\'"
  :custom
  (bqn-arguments (list "-e"
                       (format "BQNLib ⇐ {𝕨•Import%S•file.At𝕩∾%S}"
                               (expand-file-name "~/code/bqn-libs") ".bqn")
                       "-r"))
  :bind
  ("C-c B" . run-bqn))

(use-package comint-extras
  :bind
  ("C-:" . send-to-comint)
  ("C-c J" . run-j)
  ("C-c K" . run-k)
  ("C-c G" . run-goal)
  :config
  (define-run-command "j" "jc")
  (define-run-command "goal" "goal")
  (define-run-command "k" "k"
    :args (list (expand-file-name "~/code/k/repl.k"))
    :sender ("%s" "\a\n")))

(use-package treesit
  :defer t
  :custom
  (treesit-language-source-alist
   '((haskell "https://github.com/tree-sitter/tree-sitter-haskell"))))

(use-package haskell-ts-mode
  :ensure t
  :mode "\\.hs\\'")

(when (executable-find "agda")
  (load-file (shell-command-to-string "agda --emacs-mode locate")))
