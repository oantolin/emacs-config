;;; calc-rref.el --- Row-reduce echelon form for Calc    -*- lexical-binding: t; -*-

(require 'calc-mtx)

(defun calc-rref (arg)
  "Compute the reduce row echelon form of a matrix"
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "rref" 'calcFunc-rref arg)))

(defun calcFunc-rref (m)
  "Compute the reduce row echelon form of a matrix"
  (if (math-matrixp m)
      (math-with-extra-prec 2 (rref-raw m))
    (math-reject-arg m 'matrixp)))

;; Algorithm from http://rosettacode.org/wiki/Reduced_row_echelon_form
(defun rref-raw (orig-m)
  (let* ((m (math-copy-matrix orig-m))
         (rows (1- (length m)))
         (cols (1- (length (nth 1 m))))
         (lead 1)
         (r 1))
    (catch 'done
      (while (and (<= r rows) (<= lead cols))
        (let ((i r))
          (while (math-zerop (nth lead (nth i m)))
            (setq i (1+ i))
            (when (> i rows)
              (setq i r lead (1+ lead))
              (when (> lead cols) (throw 'done m))))
          (setq m (math-swap-rows m i r))
          (let ((pivot (nth lead (nth r m))) (i 1))
            (unless (math-zerop pivot)
              (let ((j lead))
                (while (<= j cols)
                  (setcar (nthcdr j (nth r m))
                          (math-div (nth j (nth r m)) pivot))
                  (setq j (1+ j)))))
            (while (<= i rows)
              (unless (= i r)
                (let ((j lead) (c (nth lead (nth i m))))
                  (while (<= j cols)
                    (setcar (nthcdr j (nth i m))
                            (math-sub (nth j (nth i m))
                                      (math-mul c (nth j (nth r m)))))
                    (setq j (1+ j)))))
              (setq i (1+ i)))))
        (setq r (1+ r) lead (1+ lead)))
      m)))

(bind-keys
 :map calc-mode-map
 ("v !" . calc-rref)
 ("V !" . calc-rref))

(provide 'calc-rref)
