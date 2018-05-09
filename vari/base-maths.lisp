(in-package :rtg-math.base-maths)

;;----------------------------------------------------------------

(varjo:v-defun sfzero-p ((f :float))
  (< (abs f) #.+k-epsilon+))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 saturate (x) "clamp(~a, 0, 1)" (:float) :float)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 inv-sqrt (x) "(1.0f / ~a)" (:float) :float)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 radians-f (x) "radians(~a)" (:float) :float)

(varjo:v-def-glsl-template-fun
 degrees-f (x) "degrees(~a)" (:float) :float)

;;----------------------------------------------------------------
(varjo:v-def-glsl-template-fun
 lerp (a b x) "mix(~a, ~a, ~a)" (:float :float :float) :float)

;; (defvar *coef*
;;   (make-array 16 :element-type ':float
;;               :initial-contents '(-0.5  1.5 -1.5  0.5
;;                                   1.0  -2.5  2.0 -0.5
;;                                   -0.5  0.0  0.5  0.0
;;                                   0.0   1.0  0.0  0.0)))
;; (defun spline (x knots)
;;   (let* ((nknots (length knots))
;;          (nspans (- nknots 3)))
;;     (unless (> nspans 0) (error "Spline has too few knots"))
;;     (let* ((x (* (clamp x 0f0 1f0) nspans))
;;            (span (if (>= x (- nknots 3))
;;                      (floor (- nknots 3))
;;                      (floor x)))
;;            (x (- x span)))
;;       (let ((c3 (+ (* (aref *coef* 0)  (elt knots span))
;;                    (* (aref *coef* 1)  (elt knots (+ 1 span)))
;;                    (* (aref *coef* 2)  (elt knots (+ 2 span)))
;;                    (* (aref *coef* 3)  (elt knots (+ 3 span)))))
;;             (c2 (+ (* (aref *coef* 4)  (elt knots span))
;;                    (* (aref *coef* 5)  (elt knots (+ 1 span)))
;;                    (* (aref *coef* 6)  (elt knots (+ 2 span)))
;;                    (* (aref *coef* 7)  (elt knots (+ 3 span)))))
;;             (c1 (+ (* (aref *coef* 8)  (elt knots span))
;;                    (* (aref *coef* 9)  (elt knots (+ 1 span)))
;;                    (* (aref *coef* 10) (elt knots (+ 2 span)))
;;                    (* (aref *coef* 11) (elt knots (+ 3 span)))))
;;             (c0 (+ (* (aref *coef* 12) (elt knots span))
;;                    (* (aref *coef* 13) (elt knots (+ 1 span)))
;;                    (* (aref *coef* 14) (elt knots (+ 2 span)))
;;                    (* (aref *coef* 15) (elt knots (+ 3 span))))))
;;         (+ c0 (* x (+ c1 (* x (+ c2 (* x c3))))))))))

(varjo:v-defun bias ((b :float) (x :float))
  (expt x (/ (log b) (log 0.5))))

(varjo:v-defun gain ((g :float) (x :float))
  (if (< x 0.5)
      (/ (bias (- 1 g) (* 2 x)) 2)
      (- 1 (/ (bias (- 1 g) (- 2 (* 2 x))) 2))))

;;----------------------------------------------------------------
