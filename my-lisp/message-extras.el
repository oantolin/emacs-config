;;; message-extras.el --- Miscellaneous commands for email    -*- lexical-binding: t; -*-

(require 'message)
(require 'smtpmail)

;; The functions here depend on the following variable, which should
;; be set to an alist associating each user mail address with its smtp
;; server. I set mine in a private configuration package.
(defvar all-user-mail-addresses)

(defun toggle-from-address ()
  "Toggle between my email addresses."
  (interactive)
  (save-excursion
    (let ((from (cadr
                 (mail-extract-address-components
                  (message-field-value "From"))))
          (emails (mapcar #'car all-user-mail-addresses)))
      (message-goto-from)
      (zap-up-to-char -1 ?:)
      (insert " "
              (message-make-from
               user-full-name
               (elt emails (mod (1+ (seq-position emails from))
                                (length emails))))))))

(defun set-smtp-server ()
  "Set the stmp server according to the from field.
Add to `message-send-hook'."
  (let ((from (cadr
               (mail-extract-address-components
                (message-field-value "From")))))
    (setq smtpmail-smtp-server (cdr (assoc from all-user-mail-addresses)))))

(defun message-lint ()
  "Check for missing subject or attachments.
Add to `message-send-hook'."
  (let ((pt (point)))
    (message-goto-subject)
    (when (looking-back "Subject: " 9)
      (user-error "Add a subject line"))
    (goto-char pt))
  (cl-flet ((containsp (re) (save-excursion
                              (message-goto-body)
                              (re-search-forward re nil t))))
    (when (and (containsp "attach\\|adjunt")
               (not (containsp "disposition=attachment"))
               (not (y-or-n-p "Send mail without attachments? ")))
      (user-error "Attach some files"))))

(provide 'message-extras)