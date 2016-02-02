(in-package :rtg-math.base-maths)

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

(declaim (inline inv-sqrt)
	 (ftype (function ((single-float))
			  (single-float))
		inv-sqrt))
(defun inv-sqrt (x)
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
