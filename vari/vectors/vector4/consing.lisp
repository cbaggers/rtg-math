(in-package #:rtg-math.vector4)

;;----------------------------------------------------------------

(varjo:add-equivalent-name 'vari.glsl:vec4 'make)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 copy-vec4 (v) "vec4(~a)" (:vec4) :vec4)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun 0p (v) "(~a==0)" (:vec4) :bool)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 unitp (v) "(length(~a)==1.0f)" (:vec4) :bool)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 = (a b) "(~a == ~a)" (:vec4 :vec4) :bool :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:vec4 :float) :vec4 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:vec4 :float) :vec4 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun + (a b c &rest c) "(~a £+£ ~a £+£ ~a ~{ £+£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro + ((a t) (b t) (c t) &rest (d t))
  `(+ ,a (+ ,b (+ ,c ,@d))))

(varjo:v-def-glsl-template-fun + (a) "~a" (:vec4) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:vec4 :vec4) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun - (a b c &rest c) "(~a £-£ ~a £-£ ~a ~{ £-£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro - ((a t) (b t) (c t) &rest (d t))
  `(- ,a (- ,b (- ,c ,@d))))

(varjo:v-def-glsl-template-fun - (a) "(-~a)" (:vec4) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:vec4 :vec4) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun * (a b c &rest c) "(~a £*£ ~a £*£ ~a ~{ £*£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro * ((a t) (b t) (c t) &rest (d t))
  `(* ,a (* ,b (* ,c ,@d))))

(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:vec4 :vec4) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun *s (v s) "(~a * ~a)" (:vec4 :float) :vec4 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun /s (v s) "(~a / ~a)" (:vec4 :float) :vec4 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun / (a b c &rest c) "(~a £/£ ~a £/£ ~a ~{ £/£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro / ((a t) (b t) (c t) &rest (d t))
  `(/ ,a (/ ,b (/ ,c ,@d))))

(varjo:v-def-glsl-template-fun / (a) "(1 / ~a)" (:vec4) 0 :pure t)
(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:vec4 :vec4) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 negate (a) "(-~a)" (:vec4) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 dot (a b) "dot(~a, ~a)" (:vec4 :vec4) :float :pure t)

;;----------------------------------------------------------------

(vari:v-defun face-foreward ((vec-a :vec4) (vec-b :vec4))
  (if (> (dot vector-a vector-b) 0)
      vector-a
      (negate vector-a)))

;;----------------------------------------------------------------

(varjo:v-defun length-squared ((vector-a :vec4))
  (dot vector-a vector-a))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 length (a) "length(~a)" (:vec4) :float :pure t)

;;----------------------------------------------------------------

(varjo:v-defun distance-squared ((vec-a :vec4) (vec-b :vec4))
  (length-squared (- vector-b vector-a)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 distance (a) "distance(~a)" (:vec4) :float :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 abs (a) "abs(~a)" (:vec4) :vec4 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 absolute-dot (a b) "abs(dot(~a, ~a))" (:vec4 :vec4) :float :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 normalize (a) "normalize(~a)" (:vec4) :vec4 :pure t)

;;----------------------------------------------------------------

;; (defn lerp ((vector-a vec4) (vector-b vec4) (ammount single-float)) vec4
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (%+ (*s vector-a (cl:- 1f0 ammount))
;;       (*s vector-b ammount)))


;; (defn stable-lerp ((vector-a vec4) (vector-b vec4) (ammount single-float)) vec4
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (lerp vector-a vector-b ammount))

;;----------------------------------------------------------------

;; (defn bezier ((a1 vec4) (a2 vec4)
;;                           (b1 vec4) (b2 vec4)
;;                           (ammount single-float)) vec4
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (lerp (lerp a1 a2 ammount)
;;         (lerp b1 b2 ammount)
;;         ammount))

;;----------------------------------------------------------------

;; (defun spline (x knots)
;;   (make (rtg-math.base-maths:spline x (mapcar #'x knots))
;;         (rtg-math.base-maths:spline x (mapcar #'y knots))
;;         (rtg-math.base-maths:spline x (mapcar #'z knots))
;;         (rtg-math.base-maths:spline x (mapcar #'w knots))))

;;---------------------------------------------------------------

(varjo:add-equivalent-name 'cl:incf 'incf)
(varjo:add-equivalent-name 'cl:decf 'decf)

;;---------------------------------------------------------------
