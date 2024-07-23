;;; omnivore.el --- Add URLs to Omnivore's reading list  -*- lexical-binding: t; -*-

;; See https://docs.omnivore.app/integrations/api.html
(defconst omnivore--save-query
  "mutation SaveUrl($input: SaveUrlInput!) { saveUrl(input: $input) { ... on SaveSuccess { url clientRequestId } ... on SaveError { errorCodes message } } }"
  "Query used to save a URL to Omnivore's reading list.")

(defun omnivore--uuid ()
  "Return string with random (version 4) UUID.
This function is the same as `org-id-uuid', duplicated here to
avoid having to load Org."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
			  (random)
			  (time-convert nil 'list)
			  (user-uid)
			  (emacs-pid)
			  (user-full-name)
			  user-mail-address
			  (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
	    (substring rnd 0 8)
	    (substring rnd 8 12)
	    (substring rnd 13 16)
	    (format "%x"
		    (logior
		     #b10000000
		     (logand
		      #b10111111
		      (string-to-number
		       (substring rnd 16 18) 16))))
	    (substring rnd 18 20)
	    (substring rnd 20 32))))

(defun omnivore--api-key ()
  "Return Omnivore API key stored in authinfo."
  (if-let ((token (car (auth-source-search :max 1 :host "omnivore.app"))))
      (auth-info-password token)
    (user-error "No Omnivore API key found")))
  
(defun omnivore-add-url (url)
  "Add URL to Omnivore's reading list.
For simplicity, this uses your browser cookie to authenticate."
  (interactive "sURL: ")
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("content-type" . "application/json")
           ("authorization" . ,(omnivore--api-key))))
        (url-request-data
         (json-encode
          `((query . ,omnivore--save-query)
            (variables
             (input
              (clientRequestId . ,(omnivore--uuid))
              (source . api)
              (url . ,url)))))))
    (url-retrieve
     "https://api-prod.omnivore.app/api/graphql"
     (lambda (_) (message "Added %s" url)))))

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
    (user-error "Not a eww buffer")))

(defun omnivore-add-shr-url ()
  "Add URL of shr link at point to Omnivore's reading list."
  (interactive)
  (if-let ((url (get-text-property (point) 'shr-url)))
      (omnivore-add-url url)
    (user-error "No link at point.")))

(provide 'omnivore)
;;; omnivore.el ends here
