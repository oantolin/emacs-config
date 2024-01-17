;;; message-extras.el --- Miscellaneous commands for email    -*- lexical-binding: t; -*-

(require 'message)
(require 'smtpmail)

;; The functions here depend on the following variable, which should
;; be set to an alist associating each user mail address with its smtp
;; data (a plist with keys :server, :type and :port). I set mine in a
;; private configuration package.
(defvar all-user-mail-addresses)

(defun cycle-from-address ()
  "Cycle between my email addresses."
  (interactive)
  (save-excursion
    (let ((from (cadr
                 (mail-extract-address-components
                  (message-field-value "From"))))
          (emails (mapcar #'car all-user-mail-addresses)))
      (message-goto-from)
      (delete-region (point) (search-backward ":"))
      (insert ": "
              (message-make-from
               user-full-name
               (elt emails (mod (1+ (seq-position emails from))
                                (length emails))))))))

(defun set-smtp-server ()
  "Set the stmp server according to the from field.
Add to `message-send-hook'."
  (when-let* ((from (cadr
                     (mail-extract-address-components
                      (message-field-value "From"))))
              (server (cdr (assoc from all-user-mail-addresses))))
    (setq smtpmail-smtp-user    (or (plist-get server :user) from)
          smtpmail-smtp-server  (plist-get server :server)
          smtpmail-stream-type  (plist-get server :type)
          smtpmail-smtp-service (plist-get server :port))))

(defun message-lint ()
  "Check for missing subject or attachments.
Add to `message-send-hook'."
  (unless (message-field-value "Subject")
    (message-goto-subject)
    (user-error "Add a subject line"))
  (cl-flet ((containsp (re) (save-excursion
                              (message-goto-body)
                              (re-search-forward re nil t))))
    (when (and (containsp "attach\\|adjunt")
               (not (containsp "disposition=attachment"))
               (not (y-or-n-p "Send mail without attachments? ")))
      (user-error "Attach some files"))))

(provide 'message-extras)
