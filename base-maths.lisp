(in-package :rtg-math.base-maths)

(defconstant +pi+ 3.141592653589793s0)
(defconstant +one-degree-in-radians+ 0.017453292519943295s0)
(defconstant +one-radian-in-degrees+ 57.29577951308232s0)

;;----------------------------------------------------------------

(defun clamp (min max val)
  (min (max val min) max))

(declaim (inline clampf)
	 (ftype (function (single-float single-float single-float)
			  single-float)
		clampf))
(defun clampf (min max float)
  (declare (single-float min max float))
  (the single-float (min (max float min) max)))

;;----------------------------------------------------------------

(declaim (inline inv-sqrt)
	 (ftype (function (single-float)
			  single-float)
		inv-sqrt))
(defun inv-sqrt (x)
  "Calculates the inverse square root of a number"
  (declare (single-float x))
  (/ 1.0 (sqrt x)))

;;----------------------------------------------------------------

(declaim (inline radians)
	 (ftype (function (single-float) single-float) radians))
(defun radians (degrees)
  (declare (single-float degrees))
  (the single-float (* degrees +one-degree-in-radians+)))

(declaim (inline degrees)
	 (ftype (function (single-float) single-float) degrees))
(defun degrees (radians)
  (declare (single-float radians))
  (the single-float (* radians +one-radian-in-degrees+)))
