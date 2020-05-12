;;; -*- lexical-binding: t; -*-

(defun calc-exchange-rates ()
  "Look up exchange rates online and set them up as calc units."
  (with-current-buffer
      (url-retrieve-synchronously "https://api.openrates.io/latest")
    (let* ((exch (json-read))
           (base (intern (cdr (assq 'base exch)))))
      (setq math-additional-units
            (cons
             (list base nil "Base currency")
             (cl-loop for (curr . rate) in (cdr (assq 'rates exch))
                      collect
                      (list curr (format "%f %s" (/ rate) base)
                            (format "Exchange rate %s=>%s" curr base))))
            math-units-table nil))))

(provide 'calc-exchange-rates)
