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

(use-package diminish :ensure t)

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
  (dolist (dir '("~/texlive/bin/x86_64-linux" "~/bin"))
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
 '(save-interprogram-paste-before-kill t)
 '(password-cache-expiry 300)
 '(debugger-stack-frame-as-list t)
 '(split-width-threshold 140)
 '(y-or-n-p-use-read-key t)
 '(async-shell-command-display-buffer nil))

(defalias 'yes-or-no-p #'y-or-n-p)

(bind-keys
 ("C-d" . delete-forward-char)
 ("M-K" . kill-paragraph)
 ("M-Z" . zap-to-char)
 ("M-o" . other-window)
 ("C-x c" . set-goal-column)
 ("C-x k" . kill-current-buffer)
 ("C-x C-p" . list-packages)
 ("M-s k" . keep-lines)
 ("M-s f" . flush-lines)
 ("M-s c" . count-matches)
 ("C-h p" . describe-package)  ; swap these two
 ("C-h P" . finder-by-keyword)
 ("C-c l" . find-library)
 ([remap list-buffers] . electric-buffer-list)
 ([remap upcase-word] . upcase-dwim)
 ([remap downcase-word] . downcase-dwim)
 ([remap capitalize-word] . capitalize-dwim)
 ([remap just-one-space] . cycle-spacing)
 ([remap count-words-region] . count-words)
 ("C-M-o" . up-list)
 ("C-o" . split-line)
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
 ("i" . visible-mode))

(bind-keys
 :prefix "C-c T"
 :prefix-map time-map
 ("w" . world-clock)
 ("t" . display-time-mode)
 ("c" . calendar))

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
  (modus-themes-load-themes)
  :config
  (if (display-graphic-p)
      (modus-themes-load-operandi)
    (modus-themes-load-vivendi)))

(use-package imenu
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
  :init (recentf-mode))

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
  ("C-M-'" . dabbrev-complete-next)
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
        ("m" . math-delimiters-toggle))
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

(use-package completion-ui
  :custom
  (completion-show-help nil)
  :bind
  (:map toggle-map
        ("SPC" . change-completion-ui))
  (:map completion-list-mode-map
        ("C-g" . abort-recursive-edit)
        ("n" . next-line)
        ("p" . previous-line)
        ("F" . consult-focus-lines)
        ("s" . isearch-forward))
  :init
  (change-completion-ui ?v)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Completions\\*\\'" nil
                 (window-parameters (mode-line-format . none))
                 (window-height . fit-window-to-buffer-max-40%))))

(use-package window-extras
  :bind
  (:map ctl-x-4-map
        ("s" . toggle-window-split)
        ("t" . transpose-windows))
  :commands fit-window-to-buffer-max-40%)

(use-package minibuffer
  :bind
  (:map minibuffer-local-map
        ("M-RET" . exit-minibuffer))
  (:map minibuffer-local-completion-map
        ("<backtab>" . minibuffer-force-complete)
        ("SPC"))
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
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

(use-package minibuffer-extras
  :bind
  (:map minibuffer-local-completion-map
        ("RET" . exit-with-top-completion))
  (:map minibuffer-local-must-match-map
        ("RET" . exit-with-top-completion))
  (:map minibuffer-local-filename-completion-map
        ("RET" . exit-with-top-completion)
        ("<C-backspace>" . up-directory)))

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
     dispatch:initialism dispatch:flex dispatch:prefixes)))

(use-package icomplete
  :demand t
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

(use-package icomplete-vertical
  :ensure t
  :bind (:map icomplete-minibuffer-map
              ("C-v" . icomplete-vertical-toggle)))

(use-package vertico
  :ensure t
  :bind
  (:map vertico-map
        ("<C-backspace>" . up-directory)
        ("M-." . consult-dir)
        ("M-j" . consult-dir-jump-file)
        ("M-q" . vertico-multiform-grid)
        ("?" . vertico-multiform-unobtrusive)
        ("'" . vertico-quick-jump))
  :custom
  (vertico-multiform-categories
   '((embark-keybinding grid)
     (consult-location (:not unobtrusive))
     (kill-ring (:not unobtrusive))
     (imenu (:not unobtrusive))
     (t unobtrusive)))
  :init
  (defvar minibuffer--require-match nil)
  :config
  (vertico-multiform-mode))

(use-package selectrum
  :ensure t
  :custom
  (selectrum-complete-in-buffer nil)
  (selectrum-display-style '(vertical))
  (selectrum-display-style-cycle-list
   '((horizontal :candidates-separator " ⋮ ") (vertical)))
  :bind
  (:map selectrum-minibuffer-map
        ("<C-backspace>" . up-directory)
        ("M-." . consult-dir)
        ("M-j" . consult-dir-jump-file)))

(use-package ivy
  :ensure t
  :diminish
  :defer t
  :config
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))))

(use-package marginalia
  :ensure t
  :bind
  ("M-A" . marginalia-cycle)
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :demand t
  :bind
  ("C-;" . embark-act)
  ("C-:" . embark-act-quit)
  ("C-," . embark-dwim)
  ("C-h b" . embark-bindings)
  ("C-h B" . embark-bindings-at-point)
  ("C-h M" . embark-bindings-in-keymap)
  (:map minibuffer-local-completion-map
        ("M-q" . embark-collect-toggle-view))
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
        ("+" . add-email-to-ecomplete))
  (:map embark-encode-map
        ("p" . topaz-paste-region))
  :hook
  (embark-collect-post-revert . fit-window-to-buffer-max-40%)
  :custom
  (embark-quit-after-action nil)
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key ";")
  (embark-help-key "?")
  :custom-face
  (embark-keybinding
   ((default :inherit default)
    (((class color) (min-colors 88) (background light))
     :background "grey96" :box (:line-width -1 :color "grey80"))
    (((class color) (min-colors 88) (background dark))
     :background "grey19" :box (:line-width -1 :color "grey35"))
    (((class color grayscale) (background light)) :background "grey90")
    (((class color grayscale) (background dark)) :background "grey25")
    (t :background "grey90")))
  :config
  (defun embark-act-quit (&optional arg)
    "Act on target then quit minibuffer.
This is a version of `embark-act' that quits the minibuffer by
default."
    (interactive "P")
    (let ((embark-quit-after-action t))
      (embark-act arg)))
  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (dolist (type '(symbol t))
    (setf (alist-get type embark-collect-initial-view-alist) 'grid))
  (setq display-buffer-alist
        (append
         '(("\\`\\*Embark Collect Completions\\*" nil
            (window-parameters (mode-line-format . none)))
           ("\\`\\*Embark Collect" nil
            (window-height . fit-window-to-buffer-max-40%)))
         display-buffer-alist))
  (defun target-org-table-cell ()
    "Target contents of current cell in an orb table."
    (when (and (derived-mode-p 'org-mode) (org-at-table-p))
      `(org-table-cell
        ,(save-excursion (string-trim (org-table-get-field)))
        . (,(save-excursion (skip-chars-backward "^|") (point))
           . ,(save-excursion (skip-chars-forward "^|") (point))))))
  (push #'target-org-table-cell embark-target-finders)
  (dolist (cmd '(comment-dwim
                 insert-parentheses
                 markdown-insert-code
                 markdown-insert-italic
                 markdown-insert-bold
                 org-emphasize
                 cdlatex-math-modify
                 TeX-font))
    (push #'embark--mark-target (alist-get cmd embark-pre-action-hooks)))
  (push #'embark--xref-push-markers
        (alist-get 'find-file embark-pre-action-hooks)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

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
  ("C-c F" . consult-focus-lines)
  ("M-`" . consult-register-store)
  ("C-`" . consult-register-load)
  ("C-M-`" . consult-register)
  (:map minibuffer-local-map
        ("M-r" . consult-history)
        ("M-s"))
  (:map consult-narrow-map
        ("C-<" . consult-narrow-help))
  :custom
  (register-preview-function #'consult-register-format)
  (completion-in-region-function #'consult-completion-in-region)
  (consult-narrow-key "<")
  :hook
  ((embark-collect-mode completion-list-mode) . consult-preview-at-point-mode)
  (minibuffer-setup . choose-completion-in-region)
  :config
  (when (eq (window-system) 'w32)
    (setq consult-find-args
          (replace-regexp-in-string "\\*" "\\\\*" consult-find-args)))
  (defun choose-completion-in-region ()
    "Use default `completion--in-region' unless we are in `eval-expression'."
    (unless (eq this-command 'eval-expression)
      (setq-local completion-in-region-function #'completion--in-region)))
  (advice-add #'register-preview :override #'consult-register-window)
  (setf (alist-get 'log-edit-mode consult-mode-histories)
        'log-edit-comment-ring)
  (plist-put consult--source-project-file :enabled #'project-current)
  (plist-put
   consult--source-project-file
   :items (lambda ()
            (mapcar #'file-relative-name (project-files (project-current))))))

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
  (("M-j" . avy-goto-word-or-subword-1)
   ("M-i" . avy-goto-char-timer)
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
  ("C-'" . link-hint-open-link)
  :init
  (cl-loop
   for (mode map) in '((minibuffer minibuffer-local-completion-map)
                       (embark embark-collect-mode-map)
                       (help help-mode-map)
                       (info Info-mode-map)
                       (apropos apropos-mode-map)
                       (man Man-mode-map)
                       (woman woman-mode-map)
                       (package package-menu-mode-map)
                       (eww eww-mode-map)
                       (gnus-art gnus-article-mode-map)
                       (gnus-sum gnus-summary-mode-map)
                       (dired dired-mode-map))
   do (eval-after-load mode `(define-key ,map "'" #'link-hint-open-link))))

(use-package elec-pair :init (electric-pair-mode))

(use-package paren :init (show-paren-mode))

(use-package text-mode
  :hook 
  (text-mode . turn-on-visual-line-mode)
  :config
  (modify-syntax-entry ?\" "\"" text-mode-syntax-table))

(use-package outline
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

(use-package restart-emacs
  :ensure t
  :bind ("C-x M-c" . restart-emacs))

(use-package olivetti
  :ensure t
  :bind ("C-c t o" . olivetti-mode))

(use-package shr
  :defer t
  :custom
  (shr-use-colors nil))

(use-package eww
  :defer t
  :custom
  (eww-bookmarks-directory "~/.private/"))

(use-package eww-extras
  :bind
  ("C-x W" . eww-bookmark-jump)
  (:map eww-mode-map
        ("W" . eww-bookmark-jump)))

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

(use-package tramp
  :defer t
  :when (executable-find "plink.exe")
  :custom (tramp-default-method "plink"))

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
     (consult-file-externally (dired-get-filename))
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

(use-package em-hist
  :defer t
  :bind
  (:map eshell-hist-mode-map
        ("M-r" . consult-history)
        ("M-s"))
  :custom (eshell-hist-ignoredups t))

(use-package ffap-extras
  :after (eshell ffap))

(use-package shell
  :bind (:map shell-mode-map
              ("C-c d" . interactive-cd)
              ("M-r" . consult-history)
              ("M-s")))

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

(use-package log-edit
  :bind
  (:map log-edit-mode-map
        ("M-r" . consult-history)
        ("M-s"))
  :hook
  (log-edit-mode . turn-off-visual-line-mode)
  (log-edit-mode . turn-on-auto-fill)
  :config
  (defun clear-log-edit-buffer (&optional _)
    "Clear the buffer if it is in `log-edit-mode'.
Intended to be used as advice for `consult-history'."
    (when (derived-mode-p 'log-edit-mode)
      (delete-minibuffer-contents)))
  (advice-add 'consult-history :before #'clear-log-edit-buffer)
  (remove-hook 'log-edit-hook #'log-edit-show-files))

(use-package log-view
  :bind
  (:map log-view-mode-map
        ("w" . log-view-save-commit-hash))
  :config
  (defun log-view-save-commit-hash ()
    ;; This is Protesilaos' prot-vc-log-kill-hash function
    "Save commit hash of log entry at point to `kill-ring'."
    (interactive)
    (let ((commit (cadr (log-view-current-entry (point) t))))
      (kill-new (format "%s" commit))
      (message "Copied: %s" commit))))

(use-package vc
  :bind
  (:map vc-prefix-map
        ("R" . vc-rename-file)
        ("d" . vc-dir-root)))

(use-package vc-dir
  :bind
  (:map vc-dir-mode-map
        ("r" . vc-revert)))

(use-package magit :ensure t :defer t)

(use-package markdown-mode
  :ensure t
  :bind
  (:map markdown-mode-map
        ("C-=" . markdown-mode-style-map))
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
   ("C-c C" . org-clock-goto))
  (:map org-mode-map
        ("C-,") ; I use this for embark-dwim
        ("C-'") ; and this for link-hint-open-link
        ("C-c C-'" . org-cycle-agenda-files) 
        ("$" . math-delimiters-insert)
        ("C-$" . ispell-complete-word)
        ("C-=" . org-emphasize))
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
  (org-tags-column -66)
  :hook
  (org-mode . turn-on-org-cdlatex)
  (org-mode . ediff-with-org-show-all)
  (org-mode . echo-area-tooltips)
  (org-mode . turn-on-auto-fill)
  (org-mode . turn-off-visual-line-mode)
  (org-mode . org-tweak-syntax-table)
  (org-mode . add-pretty-entities-hook)
  :config
  (defun ediff-with-org-show-all ()
    "Expand all headings prior to ediffing org buffers."
    (add-hook 'ediff-prepare-buffer-hook #'org-show-all nil t))
  (defun echo-area-tooltips ()
    "Show tooltips in the echo area automatically for current buffer."
    (setq-local help-at-pt-display-when-idle t
                help-at-pt-timer-delay 0)
    (help-at-pt-cancel-timer)
    (help-at-pt-set-timer))
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

;;; email packages

(use-package email-config) ; private package

(use-package ecomplete
  :defer t
  :custom
  (ecomplete-database-file "~/.private/ecompleterc")
  :config
  (setq completion-category-defaults nil)
  (ecomplete-setup)
  (defun add-email-to-ecomplete (email)
    "Add email address to ecomplete's database."
    (interactive "sEmail address: ")
    (let (name)
      (if (string-match "^\\(.*\\) <\\(.*\\)>$" email)
          (setq name (match-string 1 email)
                email (match-string 2 email))
        (setq name (read-string "Name: ")))
      (ecomplete-add-item
       'mail email
       (format (cond ((equal name "") "%s%s")
                     ((string-match-p "^[A-Za-z0-9 ]*$" name) "%s <%s>")
                     (t "\"%s\" <%s>"))
               name email))
      (ecomplete-save))))

(use-package message
  :bind (:map message-mode-map
              ("<C-tab>" . expand-mail-aliases))
  :custom
  (message-signature nil)
  (message-mail-alias-type nil)
  (message-expand-name-standard-ui t)
  ;; all-user-mail-addresses-regexp is defined in email-config
  (message-alternative-emails all-user-mail-addresses-regexp)
  :hook
  (message-mode . turn-off-auto-fill)
  (message-mode . turn-on-visual-line-mode)
  (message-sent . message-put-addresses-in-ecomplete))

(use-package message-extras
  :after message
  :bind
  (:map message-mode-map
        ("C-c t f" . toggle-from-address))
  :commands set-smtp-server
  :hook
  (message-send . set-smtp-server)
  (message-send . message-lint))

;;; applications

(use-package sx
  :ensure t
  :defer tx
  :init
  (defalias 'sx #'sx-tab-all-questions)
  :custom
  (sx-cache-directory "~/.private/sx")
  :custom-face
  (sx-question-mode-content-face ((t (:inherit default)))))

;;; major modes

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
    "An indent function for Emacs Lisp that handles some cl construcs.
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
