(in-package :rtg-math.matrix3)

;; All matrices are stored in column-major format, but when you
;; write them (like in m!) then you write them in row-major format.

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 0! () "mat3(0.0)" () :mat3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 make (a b c d e f g h i)
 "mat3(~a,~a,~a,~a,~a,~a,~a,~a,~a)"
 (:float :float :float :float :float :float :float :float :float)
 :mat3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 copy-mat3 (v) "mat3(~a)" (:mat3) :mat3)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 identity () "mat3(1.0)" () :mat3 :pure t)

;;----------------------------------------------------------------

(varjo:v-defun from-rows ((row-1 :vec3) (row-2 :vec3) (row-3 :vec3))
  (make (x row-1) (y row-1) (z row-1)
        (x row-2) (y row-2) (z row-2)
        (x row-3) (y row-3) (z row-3)))

;;----------------------------------------------------------------

(varjo:v-defun get-rows ((mat-a :mat3))
  (values (v3:make (melm mat-a 0 0)
                   (melm mat-a 0 1)
                   (melm mat-a 0 2))
          (v3:make (melm mat-a 1 0)
                   (melm mat-a 1 1)
                   (melm mat-a 1 2))
          (v3:make (melm mat-a 2 0)
                   (melm mat-a 2 1)
                   (melm mat-a 2 2))))

;;----------------------------------------------------------------

(varjo:v-defun get-row ((mat-a :mat3) (row-num :int))
  (v3:make (melm mat-a row-num 0)
           (melm mat-a row-num 1)
           (melm mat-a row-num 2)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 from-columns (a b c) "mat3(~a,~a,~a)" (:vec3 :vec3 :vec3) :mat3 :pure t)

;;----------------------------------------------------------------


(varjo:v-defun get-columns ((mat-a :mat3))
  (values (get-column mat-a 0)
          (get-column mat-a 1)
          (get-column mat-a 2)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 get-column (a b) "~a[~a]" (:mat3 :int) :vec3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 0p (a) "(~a==0)" (:mat3) :bool :pure t)

;;----------------------------------------------------------------

(varjo:v-defun identityp ((mat-a :mat3))
  (and (cl:= mat-a (identity))))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 = (a b) "(~a==~a)" (:mat3 :mat3) :bool :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 vari.glsl:determinant (a) "determinant(~a)" (:mat3) :vec3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 affine-inverse (a) "inverse(~a)" (:mat3) :mat3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 transpose (a) "transpose(~a)" (:mat3) :mat3 :pure t)

;;----------------------------------------------------------------

(varjo:v-defun adjoint ((mat-a :mat3))
  (make
   (cl:- (cl:* (melm mat-to-mutate 1 1) (melm mat-to-mutate 2 2))
         (cl:* (melm mat-to-mutate 1 2) (melm mat-to-mutate 2 1)))
   (cl:- (cl:* (melm mat-to-mutate 0 2) (melm mat-to-mutate 2 1))
         (cl:* (melm mat-to-mutate 0 1) (melm mat-to-mutate 2 2)))
   (cl:- (cl:* (melm mat-to-mutate 0 1) (melm mat-to-mutate 1 2))
         (cl:* (melm mat-to-mutate 0 2) (melm mat-to-mutate 1 1)))
   (cl:- (cl:* (melm mat-to-mutate 1 2) (melm mat-to-mutate 2 0))
         (cl:* (melm mat-to-mutate 1 0) (melm mat-to-mutate 2 2)))
   (cl:- (cl:* (melm mat-to-mutate 0 0) (melm mat-to-mutate 2 2))
         (cl:* (melm mat-to-mutate 0 2) (melm mat-to-mutate 2 0)))
   (cl:- (cl:* (melm mat-to-mutate 0 2) (melm mat-to-mutate 1 0))
         (cl:* (melm mat-to-mutate 0 0) (melm mat-to-mutate 1 2)))
   (cl:- (cl:* (melm mat-to-mutate 1 0) (melm mat-to-mutate 2 1))
         (cl:* (melm mat-to-mutate 1 1) (melm mat-to-mutate 2 0)))
   (cl:- (cl:* (melm mat-to-mutate 0 1) (melm mat-to-mutate 2 0))
         (cl:* (melm mat-to-mutate 0 0) (melm mat-to-mutate 2 1)))
   (cl:- (cl:* (melm mat-to-mutate 0 0) (melm mat-to-mutate 1 1))
         (cl:* (melm mat-to-mutate 0 1) (melm mat-to-mutate 1 0)))))

;;----------------------------------------------------------------

(varjo:v-defun trace ((mat-a :mat3))
  (cl:+ (melm mat-a 0 0) (melm mat-a 1 1) (melm mat-a 2 2)))

;;----------------------------------------------------------------

;;Rotation goes here, requires quaternion

;;----------------------------------------------------------------

(varjo:v-defun rotation-from-euler ((vec3-a :vec3))
  (let ((x (x vec3-a)) (y (y vec3-a)) (z (z vec3-a)))
    (let ((sx (sin x)) (cx (cos x))
          (sy (sin y)) (cy (cos y))
          (sz (sin z)) (cz (cos z)))
      (make (cl:* cy cz)
            (cl:- (cl:* cy sz))
            sy

            (cl:+ (cl:* sx sy cz) (cl:* cx sz))
            (cl:+ (cl:- (cl:* sx sx sz)) (cl:* cx cz))
            (cl:- (cl:* sx cy))

            (cl:+ (cl:- (cl:* cx sy cz)) (cl:* sx sz))
            (cl:+ (cl:* cx sy sz) (cl:* sx cz))
            (cl:* cx cy)))))

;;----------------------------------------------------------------

(varjo:v-defun scale ((scale-vec3 :vec3)) :mat3
               (make (x scale-vec3)  0.0             0.0
                     0.0             (y scale-vec3)  0.0
                     0.0             0.0             (z scale-vec3)))

;;----------------------------------------------------------------

(varjo:v-defun rotation-x ((angle :float))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make 1.0  0.0  0.0
          0.0  c-a  (cl:- s-a)
          0.0  s-a  c-a)))

;;----------------------------------------------------------------

(varjo:v-defun rotation-y ((angle :float))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make c-a         0.0    s-a
          0.0         1.0    0.0
          (cl:- s-a)  0.0    c-a)))

;;----------------------------------------------------------------

(varjo:v-defun rotation-z ((angle :float))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make c-a  (cl:- s-a)  0.0
          s-a  c-a         0.0
          0.0  0.0         1.0)))

;;----------------------------------------------------------------

(varjo:v-defun set-rotation-from-axis-angle ((mat-to-mutate :mat3)
                                             (axis3 :vec3)
                                             (angle :float))
  (let ((c (cos angle))
        (s (sin angle))
        (g (cl:- 1f0 (cos angle))))
    (let* ((axis3 (normalize axis3))
           (x (x axis3))
           (y (y axis3))
           (z (z axis3))
           (gxx (cl:* g x x))
           (gxy (cl:* g x y))
           (gxz (cl:* g x z))
           (gyy (cl:* g y y))
           (gyz (cl:* g y z))
           (gzz (cl:* g z z)))
      (make
       (cl:+ gxx c)        (cl:- gxy (cl:* s z))  (cl:+ gxz (cl:* s y))
       (cl:+ gxy (cl:* s z))  (cl:+ gyy c)        (cl:- gyz (cl:* s x))
       (cl:- gxz (cl:* s y))  (cl:+ gyz (cl:* s x))  (cl:+ gzz c)))))

;;----------------------------------------------------------------

(varjo:v-defun get-fixed-angles ((mat-a :mat3))
  (let* ((sy (melm mat-a 0 2)))
    (let ((cy (sqrt (cl:- 1f0 (cl:* sy sy)))))
      (if (not (cl:= 0f0 cy)) ; [TODO: not correct PI-epsilon]
          (let* ((factor (cl:/ 1.0 cy))
                 (sx (cl:* factor (cl:- (melm mat-a 2 1))))
                 (cx (cl:* factor (melm mat-a 2 2)))
                 (sz (cl:* factor (cl:- (melm mat-a 1 0))))
                 (cz (cl:* factor (melm mat-a 0 0))))
            (v3:make (atan sx cx) (atan sy cy) (atan sz cz)))
          (let* ((sz 0.0)
                 (cx 1.0)
                 (sx (melm mat-a 1 2))
                 (cz (melm mat-a 1 1)))
            (v3:make (atan sx cx) (atan sy cy) (atan sz cz)))))))

;;----------------------------------------------------------------

;; [TODO] find out how we can declaim angle to be float
;; [TODO] Comment the fuck out of this and work out how it works
;; (defn get-axis-angle ((mat-a mat3)) vec3
;;   "Gets one possible axis-angle pair that will generate this
;;    matrix.

;;    Assumes that this is a rotation matrix. It is critical that this
;;    is true (and elements are between -1f0 and 1f0) as otherwise you will
;;    at best get a runtime error, and most likely a silently incorrect result."
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (let* ((c-a (cl:* 0.5 (cl:- (trace mat-a) 1.0))))
;;     (declare (type (single-float -1f0 1f0) c-a))
;;     (let ((angle (acos c-a)))
;;       (cond ((cl:= 0f0 angle) ;; <-angle is zero so axis can be anything
;;              (v! 1.0 0.0 0.0))
;;             ((< angle rtg-math.base-maths:+pi+)
;;                                         ;its not 180 degrees
;;              (let ((axis (v! (cl:- (melm mat-a 1 2) (melm mat-a 2 1))
;;                              (cl:- (melm mat-a 2 0) (melm mat-a 0 2))
;;                              (cl:- (melm mat-a 0 1) (melm mat-a 1 0)))))
;;                (v3:normalize axis)))
;;             (t (let* ((i (if (> (melm mat-a 1 1) (melm mat-a 0 0))
;;                              1
;;                              (if (> (melm mat-a 2 2)
;;                                     (melm mat-a 0 0))
;;                                  2
;;                                  0)))
;;                       (j (mod (cl:+ i 1) 3))
;;                       (k (mod (cl:+ j 1) 3))
;;                       (tmp-s (cl:+ 1.0 (cl:- (melm mat-a i i)
;;                                              (melm mat-a j j)
;;                                              (melm mat-a k k))))
;;                       (s (sqrt (the (single-float 0f0 #.most-positive-single-float)
;;                                     tmp-s)))
;;                       (recip (cl:/ 1.0 s))
;;                       (result (v! 0.0 0.0 0.0)))
;;                  (setf (aref result i) (cl:* 0.5 s))
;;                  (setf (aref result j) (cl:* recip (melm mat-a i j)))
;;                  (setf (aref result j) (cl:* recip (melm mat-a k i)))
;;                  result))))))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun + (a b c &rest c) "(~a £+£ ~a £+£ ~a ~{ £+£ ~a~})"
                               (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro + ((a t) (b t) (c t) &rest (d t))
  `(+ ,a (+ ,b (+ ,c ,@d))))

(varjo:v-def-glsl-template-fun + (a) "~a" (:mat3) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:mat3 :mat3) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun - (a b c &rest c) "(~a £-£ ~a £-£ ~a ~{ £-£ ~a~})"
                               (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro - ((a t) (b t) (c t) &rest (d t))
  `(- ,a (- ,b (- ,c ,@d))))

(varjo:v-def-glsl-template-fun - (a) "(-~a)" (:mat3) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:mat3 :mat3) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 negate (a) "(-~a)" (:mat3) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 *v (a b) "(~a * ~a)" (:mat3 :vec3) :vec3 :pure t)

;;----------------------------------------------------------------

(varjo:v-defun mrow*vec3 ((vec :vec3) (mat-a :mat3))
  (make (cl:+ (cl:* (x vec3-to-mutate) (melm mat-a 0 0))
              (cl:* (y vec3-to-mutate) (melm mat-a 1 0))
              (cl:* (z vec3-to-mutate) (melm mat-a 2 0)))

        (cl:+ (cl:* (x vec3-to-mutate) (melm mat-a 0 1))
              (cl:* (y vec3-to-mutate) (melm mat-a 1 1))
              (cl:* (z vec3-to-mutate) (melm mat-a 2 1)))

        (cl:+ (cl:* (x vec3-to-mutate) (melm mat-a 0 2))
              (cl:* (y vec3-to-mutate) (melm mat-a 1 2))
              (cl:* (z vec3-to-mutate) (melm mat-a 2 2)))))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun * (a b c &rest c) "(~a £*£ ~a £*£ ~a ~{ £*£ ~a~})"
                               (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro * ((a t) (b t) (c t) &rest (d t))
  `(* ,a (* ,b (* ,c ,@d))))

(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:mat3 :mat3) :mat3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 *s (v s) "(~a * ~a)" (:mat3 :float) :mat3 :pure t)

;;----------------------------------------------------------------
;; makes a matrix to orient something towards a point

(varjo:v-defun from-direction ((up3 :vec3) (dir3 :vec3))
  (let* ((zaxis (v3:normalize dir3))
         (xaxis (v3-n:normalize (v3:cross zaxis up3)))
         (yaxis (v3:cross xaxis zaxis)))
    (m3:make (x xaxis) (x yaxis) (cl:- (x zaxis))
             (y xaxis) (y yaxis) (cl:- (y zaxis))
             (z xaxis) (z yaxis) (cl:- (z zaxis)))))

;;----------------------------------------------------------------
;; makes a matrix to orient something at a point towards another point

(varjo:v-defun point-at ((up :vec3) (from3 :vec3) (to3 :vec3))
  (from-direction up (v3:- to3 from3)))

;;----------------------------------------------------------------
;; Makes the rotation portion of a world->view look-at matrix

(varjo:v-defun look-at ((up3 :vec3) (from3 :vec3) (to3 :vec3))
  (let* ((zaxis (v3-n:normalize (v3:- to3 from3)))
         (xaxis (v3-n:normalize (v3:cross zaxis up3)))
         (yaxis (v3:cross xaxis zaxis)))
    (m3:make (x xaxis) (y xaxis) (z xaxis)
             (x yaxis) (y yaxis) (z yaxis)
             (cl:- (x zaxis)) (cl:- (y zaxis)) (cl:- (z zaxis)))))

;;----------------------------------------------------------------
