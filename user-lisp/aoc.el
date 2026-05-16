;;; aoc.el --- Download AoC puzzle inputs   -*- lexical-binding: t; -*-

;; This is function is based on the function from this blog post:
;; https://arjenwiersma.nl/posts/2021-11-13-Advent-of-Code-Helper/

;; In particular, I had no idea what a session cookie was before
;; reading that!

(require 'url)

(defun aoc (year day)
  "Download AoC input for DAY of YEAR; open the puzzle in a browser.
DAY and YEAR default to the current date during December, but
outside December, or with a prefix argument, you are prompted for
them."
  (interactive
   (let ((today (decode-time nil "EST")))
     (if (or current-prefix-arg
             (not (= (decoded-time-month today) 12)))
         (mapcar #'read-number '("Year: " "Day: "))
       (list (decoded-time-year today) (decoded-time-day today)))))
  (let ((url-request-extra-headers
         `(("Cookie"
            . ,(concat "session="
                       (funcall
                        (plist-get
                         (car (auth-source-search :host "adventofcode.com"))
                         :secret))))))
        (url (format "https://adventofcode.com/%d/day/%d" year day)))
    (with-temp-file (format "~/code/advent-of-code/%d/day%02d.txt" year day)
      (url-insert-file-contents (concat url "/input")))
    (browse-url url)))

(provide 'aoc)
