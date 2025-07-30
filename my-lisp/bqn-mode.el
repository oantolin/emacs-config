;; bqn-mode.el --- BQN input method     -*- lexical-binding: t; -*-

;;; Input method extracted from Marshall's bqn-mode.

(require 'comint)
(require 'quail)

(defcustom bqn-arguments nil
  "Arguments to BQN intepreter."
  :type '(repeat string)
  :group 'bqn-mode)

(defface bqn-face
  '((t (:family "BQN386 Unicode")))
  "Face for BQN source and inferior-process buffers."
  :group 'bqn-mode)

(defconst bqn--glyph-prefix-table
  '(("\\\\" . 92) ("\\ " . 8255) ("\\?" . 8656) ("\\/" . 8800)
    ("\\>" . 8805) ("\\." . 8781) ("\\<" . 8804) ("\\," . 8766)
    ("\\M" . 8802) ("\\m" . 8801) ("\\B" . 8968) ("\\b" . 8970)
    ("\\V" . 9042) ("\\v" . 8744) ("\\c" . 8595) ("\\X" . 120143)
    ("\\x" . 120169) ("\\Z" . 8904) ("\\z" . 10570) ("\\\"" . 729)
    ("\\'" . 8617) ("\\:" . 183) ("\\;" . 8900) ("\\L" . 187)
    ("\\l" . 10204) ("\\K" . 9022) ("\\k" . 9675) ("\\j" . 8728)
    ("\\H" . 171) ("\\h" . 8888) ("\\G" . 120126) ("\\g" . 120152)
    ("\\F" . 120125) ("\\f" . 120151) ("\\d" . 8597) ("\\S" . 120138)
    ("\\s" . 120164) ("\\a" . 9033) ("\\}" . 8866) ("\\{" . 8867)
    ("\\[" . 8592) ("\\p" . 960) ("\\O" . 8850) ("\\o" . 8848)
    ("\\I" . 8849) ("\\i" . 8847) ("\\u" . 8852) ("\\T" . 9035)
    ("\\t" . 8743) ("\\R" . 120163) ("\\r" . 8593) ("\\E" . 9079)
    ("\\e" . 8714) ("\\W" . 120142) ("\\w" . 120168) ("\\q" . 9021)
    ("\\+" . 8902) ("\\=" . 215) ("\\_" . 8730) ("\\-" . 247)
    ("\\)" . 10217) ("\\0" . 8226) ("\\(" . 10216) ("\\9" . 175)
    ("\\8" . 8734) ("\\^" . 9098) ("\\6" . 733) ("\\%" . 8856)
    ("\\5" . 180) ("\\$" . 9718) ("\\4" . 8988) ("\\#" . 9055)
    ("\\3" . 8316) ("\\@" . 9863) ("\\2" . 168) ("\\!" . 9097)
    ("\\1" . 728) ("\\~" . 172) ("\\`" . 732)))

(quail-define-package "BQN" "UTF-8" "⍉"
                      t "Input mode for BQN" '(("\t" . quail-completion))
                      t nil nil t nil nil nil nil nil t)

(quail-select-package "BQN")

(quail-install-map (quail-map-from-table '((default bqn--glyph-prefix-table))))

;;; run bqn with comint

(defun bqn-comint-send (process input)
  (thread-last
    input
    (substring-no-properties)
    (format ")escaped %S")
    (replace-regexp-in-string "\n" "\\\\n")
    (comint-simple-send process)))

(defun bqn--setup ()
  "Use BQN input method and face in current buffer."
  (set-input-method "BQN")
  (buffer-face-set 'bqn-face))

(defun run-bqn ()
  "Run an inferior bqn process."
  (interactive)
  (pop-to-buffer (apply #'make-comint "bqn" "bqn" nil bqn-arguments))  
  (setq-local comint-input-sender #'bqn-comint-send)
  (bqn--setup))

;;; a simple bqn-mode

(define-derived-mode bqn-mode prog-mode "BQN"
  (bqn--setup))

(provide 'bqn-mode)
