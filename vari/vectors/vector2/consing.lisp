(in-package :rtg-math.vector2)

;;----------------------------------------------------------------

(varjo:add-equivalent-name 'vari.glsl:vec2 'make)

;;---------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 copy-vec2 (v) "vec2(~a)" (:vec2) :vec2)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun 0p (v) "(~a==0)" (:vec2) :bool)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 unitp (v) "(length(~a)==1.0f)" (:vec2) :bool)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 = (a b) "(~a == ~a)" (:vec2 :vec2) :bool :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:vec2 :float) :vec2 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:vec2 :float) :vec2 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun + (a b c &rest c) "(~a £+£ ~a £+£ ~a ~{ £+£ ~a~})"
                         (t t t &rest t) :vec2 :pure t)

(varjo:v-define-compiler-macro + ((a t) (b t) (c t) &rest (d t))
  `(+ ,a (+ ,b (+ ,c ,@d))))

(varjo:v-def-glsl-template-fun + (a) "~a" (:vec2) :vec2 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:vec2 :vec2) :vec2 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun - (a b c &rest c) "(~a £-£ ~a £-£ ~a ~{ £-£ ~a~})"
                         (t t t &rest t) :vec2 :pure t)

(varjo:v-define-compiler-macro - ((a t) (b t) (c t) &rest (d t))
  `(- ,a (- ,b (- ,c ,@d))))

(varjo:v-def-glsl-template-fun - (a) "(-~a)" (:vec2) :vec2 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:vec2 :vec2) :vec2 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun * (a b c &rest c) "(~a £*£ ~a £*£ ~a ~{ £*£ ~a~})"
                         (t t t &rest t) :vec2 :pure t)

(varjo:v-define-compiler-macro * ((a t) (b t) (c t) &rest (d t))
  `(* ,a (* ,b (* ,c ,@d))))

(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:vec2 :vec2) :vec2 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun *s (v s) "(~a * ~a)" (:vec2 :float) :vec2 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun /s (v s) "(~a / ~a)" (:vec2 :float) :vec2 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun / (a b c &rest c) "(~a £/£ ~a £/£ ~a ~{ £/£ ~a~})"
                         (t t t &rest t) :vec2 :pure t)

(varjo:v-define-compiler-macro / ((a t) (b t) (c t) &rest (d t))
  `(/ ,a (/ ,b (/ ,c ,@d))))

(varjo:v-def-glsl-template-fun / (a) "(1 / ~a)" (:vec2) :vec2 :pure t)
(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:vec2 :vec2) :vec2 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 negate (a) "(-~a)" (:vec2) :vec2 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 dot (a b) "dot(~a, ~a)" (:vec2 :vec2) :float :pure t)

;;----------------------------------------------------------------

(vari:v-defun face-foreward ((vec-a :vec2) (vec-b :vec2))
  (if (> (dot vector-a vector-b) 0)
      vector-a
      (negate vector-a)))

;;----------------------------------------------------------------

(varjo:v-defun length-squared ((vector-a :vec2))
  (dot vector-a vector-a))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 length (a) "length(~a)" (:vec2) :float :pure t)

;;----------------------------------------------------------------

(varjo:v-defun distance-squared ((vec-a :vec2) (vec-b :vec2))
  (length-squared (- vector-b vector-a)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 distance (a) "distance(~a)" (:vec2) :float :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 abs (a) "abs(~a)" (:vec2) :vec2 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 absolute-dot (a b) "abs(dot(~a, ~a))" (:vec2 :vec2) :float :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 normalize (a) "normalize(~a)" (:vec2) :vec2 :pure t)

;;----------------------------------------------------------------

(varjo:v-defun perp-dot ((vec-a :vec2) (vec-b :vec2))
  (cl:- (cl:* (x vec-a) (y vec-b))
        (cl:* (y vec-a) (x vec-b))))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 cross (a b) "cross(~a, ~a)" (:vec2 :vec2) :vec2 :pure t)

;;----------------------------------------------------------------

;; (defn lerp ((vector-a vec2) (vector-b vec2) (ammount single-float)) vec2
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (%+ (*s vector-a (cl:- 1f0 ammount))
;;       (*s vector-b ammount)))


;; (defn stable-lerp ((vector-a vec2) (vector-b vec2) (ammount single-float)) vec2
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (lerp vector-a vector-b ammount))

;;----------------------------------------------------------------

;; (defn bezier ((a1 vec2) (a2 vec2)
;;                (b1 vec2) (b2 vec2) (ammount single-float)) vec2
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (lerp (lerp a1 a2 ammount)
;;         (lerp b1 b2 ammount)
;;         ammount))

;;----------------------------------------------------------------

;; (defun spline (x knots)
;;   (make (rtg-math.base-maths:spline x (mapcar #'x knots))
;;         (rtg-math.base-maths:spline x (mapcar #'y knots))))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 from-complex (a) "~a" (vari:v-complex) :vec2 :pure t)

;;---------------------------------------------------------------

(varjo:v-defun from-angle ((angle :float))
  (vec2 (sin (cl:- angle)) (cos (cl:- angle))))

;;---------------------------------------------------------------

(varjo:v-defun angle-from ((vec-from :vec2)
                           (vec-to :vec2))
  (atan (cross vec-from vec-to)
        (dot vec-from vec-to)))


(varjo:v-defun angle-between ((vec-a :vec2)
                              (vec-b :vec2))
  (cl:abs (angle-from vec-a vec-b)))

;;---------------------------------------------------------------

(varjo:v-defun rotate ((vec :vec2) (angle :float))
  (let ((sa (sin angle))
        (ca (cos angle))
        (x (x vec))
        (y (y vec)))
    (vec2 (cl:+ (cl:* x ca) (cl:* y sa))
          (cl:+ (cl:* x (cl:- sa)) (cl:* y ca)))))

;;---------------------------------------------------------------

(varjo:add-equivalent-name 'cl:incf 'incf)
(varjo:add-equivalent-name 'cl:decf 'decf)

;;---------------------------------------------------------------
