(in-package :rtg-math.base-maths)

(defconstant +pi+ 3.141592653589793f0)
(defconstant +inv-pi+ 0.3183098862f0)
(defconstant +one-degree-in-radians+ 0.017453292519943295f0)
(defconstant +one-radian-in-degrees+ 57.29577951308232f0)
(defconstant +k-epsilon+ 1.e-6)

;; I found the 'earmuffs' on the 'pi' related constants above very frustrating
;; to type when live coding. These are easier and also fit more with CL's pi
;; constant

(defconstant 2pi 6.283185307179586d0)
(defconstant 0.5pi 1.5707963267948966d0)
(defconstant inv-pi 0.3183098861837907d0)

(defconstant pi-f 3.141592653589793f0)
(defconstant 2pi-f 6.283185307179586f0)
(defconstant 0.5pi-f 1.5707963267948966f0)
(defconstant inv-pi-f 0.3183098861837907f0)

;;----------------------------------------------------------------

(defn-inline sfzero-p ((f single-float)) boolean
  (< (abs f) +k-epsilon+))

;;----------------------------------------------------------------

(defn-inline clamp ((min single-float) (max single-float) (val-f single-float))
    single-float
  (min (max val-f min) max))

(defn-inline saturate ((val-f single-float)) single-float
  (min (max val-f 0f0) 1f0))

;;----------------------------------------------------------------

(defn-inline inv-sqrt ((x (single-float 0f0 #.most-positive-single-float)))
    single-float
  "Calculates the inverse square root of a number"
  (/ 1f0 (sqrt x)))

;;----------------------------------------------------------------

(declaim (inline radians-f)
         (ftype (function (single-float) single-float) radians-f))
(defun radians-f (degrees)
  (declare (single-float degrees))
  (the single-float (* degrees +one-degree-in-radians+)))

(declaim (inline degrees-f)
         (ftype (function (single-float) single-float) degrees-f))
(defun degrees-f (radians)
  (declare (single-float radians))
  (the single-float (* radians +one-radian-in-degrees+)))

(declaim (inline radians)
         (ftype (function ((or integer single-float)) single-float)
                radians))
(defun radians (degrees)
  (radians-f (the single-float (float degrees 0f0))))

(declaim (inline degrees)
         (ftype (function ((or integer single-float)) single-float)
                degrees))
(defun degrees (radians)
  (degrees-f (the single-float (float radians 0f0))))

;;----------------------------------------------------------------

(defn-inline lerp ((start single-float) (end single-float)
                   (amount single-float))
    single-float
  (let ((amount (saturate amount)))
    (+ (* start (- 1f0 amount))
       (* end amount))))

(defn-inline mix ((start single-float) (end single-float)
                  (amount single-float))
    single-float
  (lerp start end amount))

(defn smoothstep ((a single-float) (b single-float)
                  (x single-float))
    single-float
  (let* ((x (saturate x))
         (x (/ (- x a) (- b a))))
    (* x x (- 3 (* 2 x)))))

(defvar *coef*
  (make-array 16 :element-type 'single-float
              :initial-contents '(-0.5  1.5 -1.5  0.5
                                  1.0  -2.5  2.0 -0.5
                                  -0.5  0.0  0.5  0.0
                                  0.0   1.0  0.0  0.0)))
(defun spline (x knots)
  (let* ((nknots (length knots))
         (nspans (- nknots 3)))
    (unless (> nspans 0) (error "Spline has too few knots"))
    (let* ((x (* (clamp x 0f0 1f0) nspans))
           (span (if (>= x (- nknots 3))
                     (floor (- nknots 3))
                     (floor x)))
           (x (- x span)))
      (let ((c3 (+ (* (aref *coef* 0)  (elt knots span))
                   (* (aref *coef* 1)  (elt knots (+ 1 span)))
                   (* (aref *coef* 2)  (elt knots (+ 2 span)))
                   (* (aref *coef* 3)  (elt knots (+ 3 span)))))
            (c2 (+ (* (aref *coef* 4)  (elt knots span))
                   (* (aref *coef* 5)  (elt knots (+ 1 span)))
                   (* (aref *coef* 6)  (elt knots (+ 2 span)))
                   (* (aref *coef* 7)  (elt knots (+ 3 span)))))
            (c1 (+ (* (aref *coef* 8)  (elt knots span))
                   (* (aref *coef* 9)  (elt knots (+ 1 span)))
                   (* (aref *coef* 10) (elt knots (+ 2 span)))
                   (* (aref *coef* 11) (elt knots (+ 3 span)))))
            (c0 (+ (* (aref *coef* 12) (elt knots span))
                   (* (aref *coef* 13) (elt knots (+ 1 span)))
                   (* (aref *coef* 14) (elt knots (+ 2 span)))
                   (* (aref *coef* 15) (elt knots (+ 3 span))))))
        (+ c0 (* x (+ c1 (* x (+ c2 (* x c3))))))))))

(defun bias (b x)
  (expt x (/ (log b) (log 0.5))))

(defun gain (g x)
  (if (< x 0.5)
      (/ (bias (- 1 g) (* 2 x)) 2)
      (- 1 (/ (bias (- 1 g) (- 2 (* 2 x))) 2))))

;;----------------------------------------------------------------
