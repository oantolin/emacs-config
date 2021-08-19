;;; narrow-extras.el --- Miscellaneous narrowing commands    -*- lexical-binding: t; -*-

(declare-function org-edit-src-code 'org-src)
(declare-function org-edit-src-exit 'org-src)
(declare-function TeX-narrow-to-group 'tex)
(declare-function LaTeX-narrow-to-environment 'latex)

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
	((and (bound-and-true-p org-src-mode) (not p))
	 (org-edit-src-exit))
	((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         (or (ignore-errors (org-edit-src-code))
             (ignore-errors (org-narrow-to-block))
             (org-narrow-to-subtree)))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
	((derived-mode-p 'tex-mode)
	 (TeX-narrow-to-group))
        (t (narrow-to-defun))))

(defun narrow-to-point ()
  "Narrow to point, useful to insert text without the possibility of disturbing the environs. Often followed by pasting a rectangle, to make it use new lines."
  (interactive)
  (narrow-to-region (point) (point)))

(defun narrow-to-sexp ()
  "Narrow to sexp containing point."
  (interactive)
  (narrow-to-region
   (save-excursion (up-list -1 t t) (point))
   (save-excursion (up-list +1 t t) (point))))

(provide 'narrow-extras)
