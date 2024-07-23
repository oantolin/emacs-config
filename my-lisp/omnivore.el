;;; omnivore.el --- Add URLs to Omnivore's reading list  -*- lexical-binding: t; -*-

(defun omnivore-add-url (url)
  "Add URL to Omnivore's reading list.
For simplicity, this uses your browser cookie to authenticate."
  (interactive "sURL: ")
  (browse-url-default-browser
   (concat "https://omnivore.app/api/save?url="
           (browse-url-encode-url url))))

(defun omnivore-add-kill ()
  "Add most recent kill to Omnivore's reading list."
  (interactive)
  (omnivore-add-url (current-kill 0)))

(declare-function eww-current-url "eww")

(defun omnivore-add-eww-url ()
  "Add URL shown in current eww buffer to Omnivore's reading list."
  (interactive)
  (if-let ((url (eww-current-url)))
      (omnivore-add-url url)
    (user-error "Not a eww buffer.")))

(defun omnivore-add-shr-url ()
  "Add URL of shr link at point to Omnivore's reading list."
  (interactive)
  (if-let ((url (get-text-property (point) 'shr-url)))
      (omnivore-add-url url)
    (user-error "No link at point.")))

(provide 'omnivore)
;;; omnivore.el ends here
