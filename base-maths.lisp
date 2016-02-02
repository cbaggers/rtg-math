;; {TODO}
;; Man, there is a whole lot of stupid mistakes around this code
;; I know I was a lisp newbie but..jesus. Gotta clean it all up.

(in-package :rtg-math.base-maths)

(defconstant +float-threshold+ 1.0e-6)
;; [TODO] Need to declare type of these as float
(defconstant +pi+ (the single-float 3.1415926535897932384626433832795))
(defconstant +one-degree-in-radians+ (the single-float (/ (* +pi+ 2.0) 360.0)))
(defconstant +one-radian-in-degrees+ (the single-float (/ 180.0 +pi+)))

;;----------------------------------------------------------------

(defun clamp (min max val)
  (min (max val min) max))

(declaim (inline clampf)
	 (ftype (function ((single-float) (single-float) (single-float))
			  (single-float))
		clampf))
(defun clampf (min max float)
  (declare (single-float min max float))
  (the single-float (min (max float min) max)))

;;----------------------------------------------------------------

;;Come back and implement the fast versions of these two
(declaim (inline c-sqrt)
	 (ftype (function ((single-float))
			  (single-float))
		c-sqrt))
(defun c-sqrt (x)
  "Calculates the square root of a number"
  (declare (single-float x))
  (sqrt x))

(declaim (inline c-inv-sqrt)
	 (ftype (function ((single-float))
			  (single-float))
		c-inv-sqrt))
(defun c-inv-sqrt (x)
  "Calculates the inverse square root of a number"
  (declare (single-float x))
  (/ 1.0 (sqrt x)))

;;----------------------------------------------------------------

(declaim (inline radians)
	 (ftype (function ((single-float)) (single-float)) radians))
(defun radians (degrees)
  (declare (single-float degrees))
  (the single-float (* degrees +one-degree-in-radians+)))

(declaim (inline degrees)
	 (ftype (function ((single-float)) (single-float)) degrees))
(defun degrees (radians)
  (declare (single-float radians))
  (the single-float (* radians +one-radian-in-degrees+)))

;;----------------------------------------------------------------
;; Helpers

(defmacro case= (form &body cases)
  (let ((g (gensym "val")))
    (labels ((wrap-case (c) `((= ,g ,(first c)) ,@(rest c))))
      (let* ((cases-but1 (mapcar #'wrap-case (butlast cases)))
	     (last-case (car (last cases)))
	     (last-case (if (eq (car last-case) 'otherwise)
			    `(t ,@(rest last-case))
			    (wrap-case last-case)))
	     (cases (append cases-but1 (list last-case))))
	`(let ((,g ,form))
	   (cond ,@cases))))))
