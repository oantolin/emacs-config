;;; aoc.el --- Download AoC puzzle inputs   -*- lexical-binding: t; -*-

;; This is function is based on the function from this blog post:
;; https://arjenwiersma.nl/posts/2021-11-13-Advent-of-Code-Helper/

;; In particular, I had no idea what a session cookie was before
;; reading that!

(defun aoc (year day)
  "Download AoC input for DAY of YEAR, and open the puzzle in `eww'.
DAY and YEAR default to today, but with a prefix argument you are
prompted for them."
  (interactive
   (if current-prefix-arg
       (mapcar #'read-number '("Year: " "Day: "))
     (car (read-from-string (format-time-string "(%Y %d)")))))
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
    (eww url)))

(provide 'aoc)
