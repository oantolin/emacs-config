;;; -*- lexical-binding: t; -*-

(defun display-index (i cols phase-1)
  (cond
   ((not phase-1) i)
   ((eq i 'vec) 'vec)
   ((= i cols) 0)
   ((> i cols) (- i 1))
   (t i))  )

(defun actual-basis (basis cols phase-1)
  (mapcar
   (lambda (i)
     (cond
      ((not phase-1) i)
      ((eq i 'vec) 'vec)
      ((= i 0) cols)
      ((>= i cols) (+ i 1))
      (t i)))
   basis))

(defmath dict ((matrixp A) (vectorp b) (vectorp c)
	       (vectorp basis) &optional phase-1)
  "Dictionary for basis variables with indices in `basis'"
  (interactive "p" 4 "dict")
  (setq basis (actual-basis basis (subscr (mdims A) 2) phase-1))
  (message "Actual basis: %s" basis)
  (let* ((big-A (trn (append (trn A) (idn 1 (subscr (mdims A) 1)))))
	 (n (subscr (mdims big-A) 2))
	 (B (mcol big-A basis)))
    (let ((B-1 (inv B))
	  (non-basis (vdiff (index n) basis))
	  (cc (append c (cvec 0 (- n (vlen c))))))
      (let ((A_N (mcol big-A non-basis))
	    (c_B (mrow cc basis))
	    (c_N (mrow cc non-basis)))
	(vconcat
	 (* B-1 (trn (cons b (trn (- A_N)))))
	 (list
	  (let ((c_B*B-1 (* c_B B-1)))
	    (vconcat
	     (* c_B*B-1 b)
	     (- c_N (* c_B*B-1 A_N))))))))))

(defmath pivot ((vectorp basis) (natnum enters) (natnum exits))
  "Replace `exits' with `enters' in `basis'"
  (interactive 3 "pivot")
  (vunion (vdiff basis (cvec exits 1)) (cvec enters 1)))

(defun format-pos (a)
  (pcase a
    (`(frac ,p ,q)  (format "\\frac{%s}{%s}" p q))
    (`(float ,x ,n) (format "%g" (* x (expt 10 n))))
    (_ (format "%s" a))))

(defun format-val (a)
  (if (math-zerop a)
      ""
    (format "%s%s"
	    (if (math-negp a) "-" "")
	    (format-pos (math-abs a)))))

(defun format-term (a j)
  (if (math-zerop a)
      "& "
    (format "& %s%s x_%d"
	    (if (math-negp a) "-" "+")
	    (if (math-equal (math-abs a) 1)
		""
	      (format-pos (math-abs a)))
	    j)))

(defmath latexdict ((matrixp dict) (vectorp basis) &optional phase-1)
  "Render a dictionary in LaTeX"
  (interactive "p" 2 "latexdict")
  (let ((m (- (vlen dict) 1))
	(n (- (vlen (mrow dict 1)) 1)))
    (setq basis (actual-basis basis n phase-1))
    (let ((non-basis (vdiff (index (+ m n)) basis)))
      (let ((s (with-output-to-string
		 (princ "\\[\\begin{array}{*{10}{r}}\n")
		 (for ((i 1 (+ m 1)))
		      (princ
		       (format
			"%s = & %s "
			(if (= i (+ m 1))
			    (if phase-1 " w " " z ")
			  (format "x_%d"
				  (display-index (subscr basis i) n phase-1)))
			(format-val (subscr dict i 1))))
		      (for ((j 1 n))
			   (unless (and phase-1 (= (subscr non-basis j) n))
			     (princ
			      (format-term
			       (subscr dict i (+ j 1))
			       (display-index (subscr non-basis j) n phase-1)))))
		      (when phase-1
			(let ((j (cl-position n non-basis)))
			  (when j
			    (princ (format-term (subscr dict i (+ j 1)) 0)))))
		      (princ "\\\\\n"))
		 (princ "\\end{array}\\]"))))
	(kill-new s)
	'ok))))

(bind-keys
 :map calc-mode-map
 ("z d" . calc-dict)
 ("z p" . calc-pivot)
 ("z l" . calc-latexdict))

(provide 'calc-simplex)
