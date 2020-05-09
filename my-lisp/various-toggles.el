;;; -*- lexical-binding: t; -*-

(let ((light 'modus-operandi) (dark 'modus-vivendi))
  (defun toggle-my-theme ()
    "Toggle between a light and dark theme."
    (interactive)
    (let* ((lightp (memq light custom-enabled-themes))
           (this (if lightp light dark))
           (that (if lightp dark light)))
      (disable-theme this)
      (enable-theme that))
    (when (eq major-mode 'org-mode)
      (org-mode-restart))))

(defun toggle-wrapping ()
  "Toggle both auto fill and visual line modes."
  (interactive)
  (auto-fill-mode 'toggle)
  (visual-line-mode 'toggle))

(defvar ispell-current-dictionary)

(with-eval-after-load 'ispell
  (defun toggle-ispell-lang ()
    "Toggle ispell dictionary between English and Spanish."
    (interactive)
    (ispell-change-dictionary
     (pcase ispell-current-dictionary
       ('nil
        (if (string= (system-name) "penguin") "español" "english"))
       ("english" "español")
       ("español" "english")))))

(provide 'various-toggles)
