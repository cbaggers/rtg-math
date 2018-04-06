(in-package #:rtg-math.vector3)

;;---------------------------------------------------------------

(varjo:add-equivalent-name 'vari.glsl:vec3 'make)

;;---------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 copy-vec3 (v) "vec3(~a)" (:vec3) :vec3)

;;---------------------------------------------------------------

(varjo:v-def-glsl-template-fun 0p (v) "(~a==0)" (:vec3) :bool)

;;---------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 unitp (v) "(length(~a)==1.0f)" (:vec3) :bool)

;;---------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 = (a b) "(~a == ~a)" (:vec3 :vec3) :bool :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun +s (v s) "(~a + ~a)" (:vec3 :float) :vec3 :pure t)
(varjo:v-def-glsl-template-fun -s (v s) "(~a - ~a)" (:vec3 :float) :vec3 :pure t)

;;---------------------------------------------------------------

(varjo:v-def-glsl-template-fun + (a b c &rest c) "(~a £+£ ~a £+£ ~a ~{ £+£ ~a~})"
                         (t t t &rest t) :vec3 :pure t)

(varjo:v-define-compiler-macro + ((a t) (b t) (c t) &rest (d t))
  `(+ ,a (+ ,b (+ ,c ,@d))))

(varjo:v-def-glsl-template-fun + (a) "~a" (:vec3) :vec3 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:vec3 :vec3) :vec3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun - (a b c &rest c) "(~a £-£ ~a £-£ ~a ~{ £-£ ~a~})"
                         (t t t &rest t) :vec3 :pure t)

(varjo:v-define-compiler-macro - ((a t) (b t) (c t) &rest (d t))
  `(- ,a (- ,b (- ,c ,@d))))

(varjo:v-def-glsl-template-fun - (a) "(-~a)" (:vec3) :vec3 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:vec3 :vec3) :vec3 :pure t)

;;---------------------------------------------------------------

(varjo:v-def-glsl-template-fun * (a b c &rest c) "(~a £*£ ~a £*£ ~a ~{ £*£ ~a~})"
                         (t t t &rest t) :vec3 :pure t)

(varjo:v-define-compiler-macro * ((a t) (b t) (c t) &rest (d t))
  `(* ,a (* ,b (* ,c ,@d))))

(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:vec3 :vec3) :vec3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun *s (v s) "(~a * ~a)" (:vec3 :float) :vec3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun /s (v s) "(~a / ~a)" (:vec3 :float) :vec3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun / (a b c &rest c) "(~a £/£ ~a £/£ ~a ~{ £/£ ~a~})"
                         (t t t &rest t) :vec3 :pure t)

(varjo:v-define-compiler-macro / ((a t) (b t) (c t) &rest (d t))
  `(/ ,a (/ ,b (/ ,c ,@d))))

(varjo:v-def-glsl-template-fun / (a) "(1 / ~a)" (:vec3) :vec3 :pure t)
(varjo:v-def-glsl-template-fun / (a b) "(~a / ~a)" (:vec3 :vec3) :vec3 :pure t)

;;---------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 negate (a) "(-~a)" (:vec3) :vec3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 dot (a b) "dot(~a, ~a)" (:vec3 :vec3) :float :pure t)

;;----------------------------------------------------------------

(vari:v-defun face-foreward ((vec-a :vec3) (vec-b :vec3))
  (if (> (dot vector-a vector-b) 0)
      vector-a
      (negate vector-a)))

;;----------------------------------------------------------------

(varjo:v-defun length-squared ((vector-a :vec3))
  (dot vector-a vector-a))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 length (a) "length(~a)" (:vec3) :float :pure t)

;;----------------------------------------------------------------

(varjo:v-defun distance-squared ((vec-a :vec3) (vec-b :vec3))
  (length-squared (- vector-b vector-a)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 distance (a) "distance(~a)" (:vec3) :float :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 abs (a) "abs(~a)" (:vec3) :vec3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 absolute-dot (a b) "abs(dot(~a, ~a))" (:vec3 :vec3) :float :pure t)

;;---------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 normalize (a) "normalize(~a)" (:vec3) :vec3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 cross (a b) "cross(~a, ~a)" (:vec3 :vec3) :vec3 :pure t)

;;----------------------------------------------------------------

;; (defun closest-point-on-line (line point)
;;   (destructuring-bind (line-point line-dir) line
;;     (let* ((diff-vec (- point line-point))
;;            (dist-sq (dot line-dir line-dir))
;;            (projection (dot diff-vec line-dir)))
;;       (v:+ line-point (*s line-dir (cl:/ projection dist-sq))))))

;; (defun closest-point-on-norm-line (line point)
;;   (destructuring-bind (line-point line-dir) line
;;     (let* ((diff-vec (- point line-point))
;;            (projection (dot diff-vec line-dir)))
;;       (v:+ line-point (*s line-dir projection)))))

;; (defun distance-to-line-sq (line point)
;;   (destructuring-bind (line-point line-dir) line
;;     (let* ((diff-vec (- point line-point))
;;            (dist-sq (dot line-dir line-dir))
;;            (diff-sq (dot diff-vec diff-vec))
;;            (projection (dot diff-vec line-dir)))
;;       (cl:- diff-sq (cl:/ (cl:* projection projection) dist-sq)))))

;; (defun distance-to-norm-line-sq (line point)
;;   (destructuring-bind (line-point line-dir) line
;;     (let* ((diff-vec (- point line-point))
;;            (diff-sq (dot diff-vec diff-vec))
;;            (projection (dot diff-vec line-dir)))
;;       (cl:- diff-sq (cl:* projection projection)))))

;;----------------------------------------------------------------

;; (defn lerp ((vector-a vec3) (vector-b vec3) (ammount single-float)) vec3
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (%+ (*s vector-a (cl:- 1f0 ammount))
;;       (*s vector-b ammount)))

;; (defn stable-lerp ((vector-a vec3) (vector-b vec3) (ammount single-float)) vec3
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (lerp vector-a vector-b ammount))

;;----------------------------------------------------------------

;; (defn bezier ((a1 vec3) (a2 vec3)
;;                (b1 vec3) (b2 vec3)
;;                (ammount single-float)) vec3
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (lerp (lerp a1 a2 ammount)
;;         (lerp b1 b2 ammount)
;;         ammount))

;;----------------------------------------------------------------

;; (defun spline (x knots)
;;   (make (rtg-math.base-maths:spline x (mapcar #'x knots))
;;         (rtg-math.base-maths:spline x (mapcar #'y knots))
;;         (rtg-math.base-maths:spline x (mapcar #'z knots))))

;;----------------------------------------------------------------

(varjo:v-defun rotate ((vec :vec3) (rotation :vec3))
  ;; this was based on rotation-from-euler, so this rotates clockwise
  ;; which is ass
  (let* ((x (x vec))
         (y (y vec))
         (z (z vec))
         (rx (x rotation))
         (ry (y rotation))
         (rz (z rotation))
         (sx (sin rx)) (cx (cos rx))
         (sy (sin ry)) (cy (cos ry))
         (sz (sin rz)) (cz (cos rz)))
    (vec3 (cl:+ (cl:* x (cl:* cy cz))
                (cl:* y (cl:- (cl:* cy sz)))
                (cl:* z sy))
          (cl:+ (cl:* x (cl:+ (cl:* sx sy cz) (cl:* cx sz)))
                (cl:* y (cl:- (cl:* cx cz) (cl:* sx sy sz)))
                (cl:* z (cl:- (cl:* sx cy))))
          (cl:+ (cl:* x (cl:- (cl:* sx sz) (cl:* cx sy cz)))
                (cl:* y (cl:+ (cl:* cx sy sz) (cl:* sx cz)))
                (cl:* z (cl:* cx cy))))))

;;---------------------------------------------------------------

(varjo:add-equivalent-name 'cl:incf 'incf)
(varjo:add-equivalent-name 'cl:decf 'decf)

;;---------------------------------------------------------------
