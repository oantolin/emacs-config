;;; ement-tweaks.el --- Tweaks for ement -*- lexical-binding: t; -*-

;;; Some or all of these will become unnecessary in a future version,
;;; I'll keep checking to see when I can delete code from here.

(require 'ement)

(defgroup ement-tweaks nil
  "Personal tweaks for ement"
  :group 'ement
  :prefix "ement-tweaks-")

(defcustom ement-tweaks-initial-emoji-command #'emoji-insert
  "Command to choose an emoji for a reaction."
  :type '(choice (const emoji-insert)
                 (const emoji-search)
                 (const ignore)         ; type it however you want!
                 function))

(defvar-keymap ement-tweaks-emoji-map
  "s" #'emoji-search
  "i" #'emoji-insert)

(defun ement-tweaks-send-reaction (key position)
  "Send a reaction."
  (interactive
   (list (minibuffer-with-setup-hook
             (lambda ()
               (setq-local after-change-functions
                           (list (lambda (&rest _) (exit-minibuffer))))
               (use-local-map (make-composed-keymap
                               ement-tweaks-emoji-map (current-local-map)))
               (let ((enable-recursive-minibuffers t))
                 (call-interactively ement-tweaks-initial-emoji-command)))
           (read-string "Reaction: "))
         (point)))
  (ement-room-send-reaction key position))

;; The next two functions have non-prefixed names because I might find
;; them generally useful
(defun exit-minibuffer-once ()                               
  (remove-hook 'post-command-hook 'exit-minibuffer-once)
  (exit-minibuffer))

(defun schedule-exit-minibuffer-once ()
  (add-hook 'post-command-hook 'exit-minibuffer-once))

(define-minor-mode ement-tweaks-quick-send-minor-mode
  "Minor mode to immediately send messages from the Ement compose buffer."
  :global t
  (if ement-tweaks-quick-send-minor-mode
      (advice-add 'ement-room-compose-send
                  :before 'schedule-exit-minibuffer-once)
    (advice-remove 'ement-room-compose-send 'schedule-exit-minibuffer-once)))

(provide 'ement-tweaks)
