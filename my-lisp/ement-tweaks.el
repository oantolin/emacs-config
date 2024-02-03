;;; ement-tweaks.el --- Tweaks for ement -*- lexical-binding: t; -*-

;;; Some or all of these will become unnecessary in a future version,
;;; I'll keep checking to see when I can delete code from here.

(require 'ement)

(defgroup ement-tweaks nil
  "Personal tweaks for ement"
  :group 'ement
  :prefix "ement-tweaks-")

(defcustom ement-tweaks-emoji-command #'emoji-search
  "Command to choose an emoji for a reaction."
  :type '(choice (const emoji-insert)
                 (const emoji-search)
                 (const ignore)         ; type it however you want!
                 function))

(defvar-keymap ement-tweaks-emoji-map
  "s" #'emoji-search
  "i" #'emoji-insert
  "c" #'insert-char)

(defun ement-tweaks-send-reaction (key position)
  "Send a reaction.
The user option `ement-tweaks-emoji-command' controls
which emoji insertion command is used to select the emoji.

You're not forced to stick with that emoji insertion command, you
can C-g your way out and type the emoji anyway you'd like.  To
facilitate this, the follow keybindings are enabled in the
minibuffer:

\\{ement-tweaks-emoji-map}"
  (interactive
   (list (minibuffer-with-setup-hook
             (lambda ()
               (setq-local after-change-functions
                           (list (lambda (&rest _) (exit-minibuffer))))
               (use-local-map (make-composed-keymap
                               ement-tweaks-emoji-map (current-local-map)))
               (let ((enable-recursive-minibuffers t))
                 (call-interactively ement-tweaks-emoji-command)))
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
