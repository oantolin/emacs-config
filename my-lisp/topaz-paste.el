;;; topaz-paste.el --- Generate topaz paste URLs    -*- lexical-binding: t; -*-

;; https://topaz.github.io/paste/ is a "client side paste service",
;; which can be used to share snippets of text. All the data is
;; encoded in the URL as a Base64-encoded LZMA-compressed string.
;; Since both Base64 and LZMA are pretty standard this means you don't
;; actually need the website to generate a URL!

(defun topaz-paste-region (begin end)
  "Save topaz paste URL for region contents on the kill-ring."
  (interactive "r")
  (kill-new
   (concat "https://topaz.github.io/paste/#"
           (string-replace
            "\n" ""
            (with-output-to-string
              (call-shell-region begin end "xz -zc --format=lzma | base64"
                                 nil standard-output))))))

(defun topaz-paste-buffer ()
  "Save topaz paste URL for buffer contents on the kill-ring."
  (interactive)
  (topaz-paste-region (point-min) (point-max)))

(provide 'topaz-paste)
