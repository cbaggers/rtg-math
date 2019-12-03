(in-package :rtg-math.matrix4)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 0! () "mat4(0.0)" () :mat4 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 make (a b c d e f g h i j k l m n o p)
 "mat4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
 (:float :float :float :float :float :float :float :float
         :float :float :float :float :float :float :float :float)
 :mat4 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 copy-mat4 (v) "mat4(~a)" (:mat4) :mat4)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 identity () "mat4(1.0)" () :mat4 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 to-mat3 (m) "mat3(~a)" (:mat4) :mat3 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 from-mat3 (m) "mat4(~a)" (:mat3) :mat4 :pure t)

;;----------------------------------------------------------------

(varjo:v-defun from-rows ((row-1 :vec4)
                          (row-2 :vec4)
                          (row-3 :vec4)
                          (row-4 :vec4))
  (make (x row-1) (x row-2) (x row-3) (x row-4)
	(y row-1) (y row-2) (y row-3) (y row-4)
	(z row-1) (z row-2) (z row-3) (z row-4)
	(w row-1) (w row-2) (w row-3) (w row-4)))

(varjo:v-defun from-rows-v3 ((row-1 :vec3) (row-2 :vec3) (row-3 :vec3))
  (make (x row-1) (x row-2) (x row-3) 0f0
        (y row-1) (y row-2) (y row-3) 0f0
	(z row-1) (z row-2) (z row-3) 0f0
        0f0       0f0       0f0       1f0))

;;----------------------------------------------------------------

(varjo:v-defun get-rows ((mat-a :mat4))
  (values (v4:make (melm mat-a 0 0)
                   (melm mat-a 0 1)
                   (melm mat-a 0 2)
                   (melm mat-a 0 3))
          (v4:make (melm mat-a 1 0)
                   (melm mat-a 1 1)
                   (melm mat-a 1 2)
                   (melm mat-a 1 3))
          (v4:make (melm mat-a 2 0)
                   (melm mat-a 2 1)
                   (melm mat-a 2 2)
                   (melm mat-a 2 3))
          (v4:make (melm mat-a 3 0)
                   (melm mat-a 3 1)
                   (melm mat-a 3 2)
                   (melm mat-a 3 3))))

;;----------------------------------------------------------------

(varjo:v-defun get-row ((mat-a :mat4) (row-num :int))
  (v4:make (melm mat-a row-num 0)
           (melm mat-a row-num 1)
           (melm mat-a row-num 2)
           (melm mat-a row-num 3)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 from-columns (a b c d) "mat4(~a,~a,~a,~a)" (:vec4 :vec4 :vec4 :vec4) :mat4
 :pure t)

(varjo:v-def-glsl-template-fun
 from-columns-v3 (a b c) "mat4(vec4(~a,0),vec4(~a,0),vec4(~a,0), vec4(0,0,0,1))"
 (:vec3 :vec3 :vec3) :mat4
 :pure t)

;;----------------------------------------------------------------

(varjo:v-defun get-columns ((mat-a :mat4))
  (values (get-column mat-a 0)
          (get-column mat-a 1)
          (get-column mat-a 2)
          (get-column mat-a 3)))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 get-column (a b) "~a[~a]" (:mat4 :int) :vec4 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 0p (a) "(~a==0)" (:mat4) :bool :pure t)

;;----------------------------------------------------------------

(varjo:v-defun identityp ((mat-a :mat4))
  (and (cl:= mat-a (identity))))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 = (a b) "(~a==~a)" (:mat4 :mat4) :bool :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 vari.glsl:determinant (a) "determinant(~a)" (:mat4) :vec4 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 affine-inverse (a) "inverse(~a)" (:mat4) :mat4 :pure t)

;;----------------------------------------------------------------

;; (defn inverse ((matrix mat4)) mat4
;;   ;;(declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (let ((det (m4:determinant matrix)))
;;     (if (cl:= det 0f0)
;;         (error "Cannot invert matrix with zero determinant:~%  ~S"
;;                matrix)
;;         (macrolet ((a (x y z)
;;                      (declare (optimize (safety 3) (speed 0)))
;;                      (multiple-value-bind (r1 c1) (truncate (cl:- x 11) 10)
;;                        (multiple-value-bind (r2 c2) (truncate (cl:- y 11) 10)
;;                          (multiple-value-bind (r3 c3) (truncate (cl:- z 11) 10)
;;                            `(cl:* (melm matrix ,r1 ,c1)
;;                                   (melm matrix ,r2 ,c2)
;;                                   (melm matrix ,r3 ,c3)))))))
;;           (let ((m
;;                  (make
;;                   ;; row 1
;;                   (cl:- (cl:+ (a 22 33 44) (a 23 34 42) (a 24 32 43))
;;                         (a 22 34 43) (a 23 32 44) (a 24 33 42))
;;                   (cl:- (cl:+ (a 12 34 43) (a 13 32 44) (a 14 33 42))
;;                         (a 12 33 44) (a 13 34 42) (a 14 32 43))
;;                   (cl:- (cl:+ (a 12 23 44) (a 13 24 42) (a 14 22 43))
;;                         (a 12 24 43) (a 13 22 44) (a 14 23 42))
;;                   (cl:- (cl:+ (a 12 24 33) (a 13 22 34) (a 14 23 32))
;;                         (a 12 23 34) (a 13 24 32) (a 14 22 33))
;;                   ;; row 2
;;                   (cl:- (cl:+ (a 21 34 43) (a 23 31 44) (a 24 33 41))
;;                         (a 21 33 44) (a 23 34 41) (a 24 31 43))
;;                   (cl:- (cl:+ (a 11 33 44) (a 13 34 41) (a 14 31 43))
;;                         (a 11 34 43) (a 13 31 44) (a 14 33 41))
;;                   (cl:- (cl:+ (a 11 24 43) (a 13 21 44) (a 14 23 41))
;;                         (a 11 23 44) (a 13 24 41) (a 14 21 43))
;;                   (cl:- (cl:+ (a 11 23 34) (a 13 24 31) (a 14 21 33))
;;                         (a 11 24 33) (a 13 21 34) (a 14 23 31))
;;                   ;; row 3
;;                   (cl:- (cl:+ (a 21 32 44) (a 22 34 41) (a 24 31 42))
;;                         (a 21 34 42) (a 22 31 44) (a 24 32 41))
;;                   (cl:- (cl:+ (a 11 34 42) (a 12 31 44) (a 14 32 41))
;;                         (a 11 32 44) (a 12 34 41) (a 14 31 42))
;;                   (cl:- (cl:+ (a 11 22 44) (a 12 24 41) (a 14 21 42))
;;                         (a 11 24 42) (a 12 21 44) (a 14 22 41))
;;                   (cl:- (cl:+ (a 11 24 32) (a 12 21 34) (a 14 22 31))
;;                         (a 11 22 34) (a 12 24 31) (a 14 21 32))
;;                   ;; row 4
;;                   (cl:- (cl:+ (a 21 33 42) (a 22 31 43) (a 23 32 41))
;;                         (a 21 32 43) (a 22 33 41) (a 23 31 42))
;;                   (cl:- (cl:+ (a 11 32 43) (a 12 33 41) (a 13 31 42))
;;                         (a 11 33 42) (a 12 31 43) (a 13 32 41))
;;                   (cl:- (cl:+ (a 11 23 42) (a 12 21 43) (a 13 22 41))
;;                         (a 11 22 43) (a 12 23 41) (a 13 21 42))
;;                   (cl:- (cl:+ (a 11 22 33) (a 12 23 31) (a 13 21 32))
;;                         (a 11 23 32) (a 12 21 33) (a 13 22 31)))))
;;             (dotimes (i 4)
;;               (dotimes (j 4)
;;                 (setf (melm m i j) (cl:/ (melm m i j) det))))
;;             m)))))

;;----------------------------------------------------------------
;; {TODO} could just feed straight from array into make

(varjo:v-def-glsl-template-fun
 transpose (a) "transpose(~a)" (:mat4) :mat4 :pure t)

;;----------------------------------------------------------------

(varjo:v-defun adjoint ((mat-a :mat4))
  (make (minor mat-a 1 2 3 1 2 3)
        (cl:- (minor mat-a 0 2 3 1 2 3))
        (minor mat-a 0 1 3 1 2 3)
        (cl:- (minor mat-a 0 1 2 1 2 3))

        (cl:- (minor mat-a 1 2 3 0 2 3))
        (minor mat-a 0 2 3 0 2 3)
        (cl:- (minor mat-a 0 1 3 0 2 3))
        (minor mat-a 0 1 2 0 2 3)

        (minor mat-a 1 2 3 0 1 3)
        (cl:- (minor mat-a 0 2 3 0 1 3))
        (minor mat-a 0 1 3 0 1 3)
        (cl:- (minor mat-a 0 1 2 0 1 3))

        (cl:- (minor mat-a 1 2 3 0 1 2))
        (minor mat-a 0 2 3 0 1 2)
        (cl:- (minor mat-a 0 1 3 0 1 2))
        (minor mat-a 0 1 2 0 1 2)))


;;----------------------------------------------------------------

(varjo:v-defun trace ((mat-a :mat4))
  (cl:+ (melm mat-a 0 0)
        (melm mat-a 1 1)
        (melm mat-a 2 2)
        (melm mat-a 3 3)))

;;----------------------------------------------------------------

(varjo:v-defun translation ((vec-a :vec3))
  (make
   1f0  0f0  0f0  0.0
   0f0  1f0  0f0  0.0
   0f0  0f0  1f0  0.0
   (x vec-a) (y vec-a) (z vec-a) 1f0))

(varjo:v-defun translation ((vec-a :vec4))
  (make
   1f0  0f0  0f0  0.0
   0f0  1f0  0f0  0.0
   0f0  0f0  1f0  0.0
   (x vec-a) (y vec-a) (z vec-a) 1f0))

;;----------------------------------------------------------------

(varjo:v-defun rotation-from-mat3 ((m-a :mat3))
  (make
   (m3:melm m-a 0 0)  (m3:melm m-a 1 0)  (m3:melm m-a 2 0)  0f0
   (m3:melm m-a 0 1)  (m3:melm m-a 1 1)  (m3:melm m-a 2 1)  0f0
   (m3:melm m-a 0 2)  (m3:melm m-a 1 2)  (m3:melm m-a 2 2)  0f0
   0f0                0f0                0f0                1f0))

;;----------------------------------------------------------------

(varjo:v-defun rotation-from-euler ((vec3-a :vec3))
  (let ((x (x vec3-a))
        (y (y vec3-a))
        (z (z vec3-a)))
    (let ((sx (sin x)) (cx (cos x))
          (sy (sin y)) (cy (cos y))
          (sz (sin z)) (cz (cos z)))
      (make
       (cl:* cy cz)
       (cl:- (cl:* cy sz))
       sy
       0f0

       (cl:+ (cl:* sx sy cz) (cl:* cx sz))
       (cl:- (cl:* cx cz) (cl:* sx sy sz))
       (cl:- (cl:* sx cy))
       0f0

       (cl:- (cl:* sx sz) (cl:* cx sy cz))
       (cl:+ (cl:* cx sy sz) (cl:* sx cz))
       (cl:* cx cy)
       0f0

       0f0 0f0 0f0 1f0))))

;;----------------------------------------------------------------

(varjo:v-defun scale ((scale-vec3 :vec3))
  (make
   (x scale-vec3)  0f0               0f0             0f0
   0f0             (y scale-vec3)    0f0             0f0
   0f0             0f0               (z scale-vec3)  0f0
   0f0             0f0               0f0             1f0))

;;----------------------------------------------------------------

(varjo:v-defun rotation-x ((angle :float))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make 1f0  0f0  0f0     0f0
          0f0  c-a  s-a 0f0
          0f0  (cl:- s-a)  c-a     0f0
          0f0  0f0  0f0     1f0)))

;;----------------------------------------------------------------

(varjo:v-defun rotation-y ((angle :float))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make c-a         0f0  (cl:- s-a)  0f0
          0f0         1f0  0f0  0f0
          s-a  0f0  c-a  0f0
          0f0         0f0  0f0  1f0)))

;;----------------------------------------------------------------

(varjo:v-defun rotation-z ((angle :float))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make c-a  s-a  0f0  0f0
          (cl:- s-a)  c-a      0f0  0f0
          0f0  0f0      1f0  0f0
          0f0  0f0      0f0  1f0)))

;;----------------------------------------------------------------

(varjo:v-defun rotation-from-axis-angle ((axis3 :vec3) (angle :float))
  (let ((c (cos angle))
        (s (sin angle))
        (g (cl:- 1f0 (cos angle))))
    (let* ((x (x axis3))
           (y (y axis3))
           (z (z axis3))
           (gxx (cl:* g x x))
           (gxy (cl:* g x y))
           (gxz (cl:* g x z))
           (gyy (cl:* g y y))
           (gyz (cl:* g y z))
           (gzz (cl:* g z z)))
      (make
       (cl:+ gxx c)        (cl:+ gxy (cl:* s z))  (cl:- gxz (cl:* s y)) 0f0
       (cl:- gxy (cl:* s z))  (cl:+ gyy c)        (cl:+ gyz (cl:* s x)) 0f0
       (cl:+ gxz (cl:* s y))  (cl:- gyz (cl:* s x))  (cl:+ gzz c)       0f0
       0f0              0f0              0f0             1f0))))

;;----------------------------------------------------------------

;; [TODO] returned as vector x-y-z

(varjo:v-defun get-fixed-angles ((mat-a :mat4))
  (let ((sy (melm mat-a 0 2)))
    (let ((cy (sqrt (cl:- 1f0 (cl:* sy sy)))))
      (if (cl:= 0f0 cy)
          (let ((sz 0f0)
                (cz 1f0)
                (sx (melm mat-a 2 1))
                (cx (melm mat-a 1 1)))
            (v3:make (atan sx cx) (atan sy cy) (atan sz cz)))
          (let* ((factor (cl:/ 1f0 cy)) ; normal case
                 (sx (cl:- (cl:* factor (melm mat-a 1 2))))
                 (cx (cl:* factor (melm mat-a 2 2)))
                 (sz (cl:- (cl:* factor (melm mat-a 0 1))))
                 (cz (cl:* factor (melm mat-a 0 0))))
            (v3:make (atan sx cx) (atan sy cy) (atan sz cz)))))))

;;----------------------------------------------------------------

;; Triggers flow-id bug
;;
;; (varjo:v-defun get-axis-angle ((mat-a :mat4))
;;   (let* ((trace-a (cl:+ (melm mat-a 0 0) (melm mat-a 1 1)
;;                         (melm mat-a 2 2)))
;;          (cos-theta (cl:* 0.5 (cl:- trace-a 1f0)))
;;          (angle (acos cos-theta)))
;;     (cond ((cl:= 0f0 angle) (values (v! 1f0 0f0 0f0) angle))
;;           ((cl:= 0f0 (cl:- rtg-math.base-maths:+pi+ angle))
;;            (values
;;             (v3:normalize
;;              (v! (cl:- (melm mat-a 2 1) (melm mat-a 1 2))
;;                  (cl:- (melm mat-a 0 2) (melm mat-a 2 0))
;;                  (cl:- (melm mat-a 1 0) (melm mat-a 0 1))))
;;             angle))
;;           (t (labels ((biggest-trace (matr)
;;                         (let ((x 0))
;;                           (if (> (melm matr 1 1) (melm matr 0 0))
;;                               (setf x 1))
;;                           (if (> (melm matr 2 2) (melm matr x x))
;;                               (setf x 2))
;;                           x)))
;;                (let* ((i (biggest-trace mat-a))
;;                       (j (mod (cl:+ i 1) 3))
;;                       (k (mod (cl:+ i 1) 3))
;;                       (tmp-s (cl:+ 1f0 (cl:- (melm mat-a i i)
;;                                              (melm mat-a j j)
;;                                              (melm mat-a k k))))
;;                       (s (the (single-float 0f0 #.most-positive-single-float)
;;                               tmp-s))
;;                       (recip (cl:/ 1f0 s)))
;;                  (values (v! (cl:* 0.5 s)
;;                              (cl:* recip (aref mat-a (cl:+ i (cl:* 4 j))))
;;                              (cl:* recip (aref mat-a (cl:+ k (cl:* 4 i)))))
;;                          angle)))))))


;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun + (a b c &rest c) "(~a £+£ ~a £+£ ~a ~{ £+£ ~a~})"
                               (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro + ((a t) (b t) (c t) &rest (d t))
  `(+ ,a (+ ,b (+ ,c ,@d))))

(varjo:v-def-glsl-template-fun + (a) "~a" (:mat4) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (:mat4 :mat4) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun - (a b c &rest c) "(~a £-£ ~a £-£ ~a ~{ £-£ ~a~})"
                               (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro - ((a t) (b t) (c t) &rest (d t))
  `(- ,a (- ,b (- ,c ,@d))))

(varjo:v-def-glsl-template-fun - (a) "(-~a)" (:mat4) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (:mat4 :mat4) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 negate (a) "(-~a)" (:mat4) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 *v (a b) "(~a * ~a)" (:mat4 :vec3) :vec3 :pure t)

(varjo:v-defun *v3 ((mat-a :mat4) (vec3 :vec3))
  (make (cl:+ (cl:* (melm mat-a 0 0) (x vec3-to-mutate))
              (cl:* (melm mat-a 0 1) (y vec3-to-mutate))
              (cl:* (melm mat-a 0 2) (z vec3-to-mutate))
              (melm mat-a 0 3))
        (cl:+ (cl:* (melm mat-a 1 0) (x vec3-to-mutate))
              (cl:* (melm mat-a 1 1) (y vec3-to-mutate))
              (cl:* (melm mat-a 1 2) (z vec3-to-mutate))
              (melm mat-a 1 3))
        (cl:+ (cl:* (melm mat-a 2 0) (x vec3-to-mutate))
              (cl:* (melm mat-a 2 1) (y vec3-to-mutate))
              (cl:* (melm mat-a 2 2) (z vec3-to-mutate))
              (melm mat-a 2 3))))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 *s (v s) "(~a * ~a)" (:mat3 :float) :mat3 :pure t)

;;----------------------------------------------------------------

(varjo:v-defun mrow*vec4 ((vec :vec4) (mat-a :mat4))
  (make
   (cl:+ (cl:* (x vec4-to-mutate) (melm mat-a 0 0))
         (cl:* (y vec4-to-mutate) (melm mat-a 1 0))
         (cl:* (z vec4-to-mutate) (melm mat-a 2 0))
         (cl:* (w vec4-to-mutate) (melm mat-a 3 0)))

   (cl:+ (cl:* (x vec4-to-mutate) (melm mat-a 0 1))
         (cl:* (y vec4-to-mutate) (melm mat-a 1 1))
         (cl:* (z vec4-to-mutate) (melm mat-a 2 1))
         (cl:* (w vec4-to-mutate) (melm mat-a 3 1)))

   (cl:+ (cl:* (x vec4-to-mutate) (melm mat-a 0 2))
         (cl:* (y vec4-to-mutate) (melm mat-a 1 2))
         (cl:* (z vec4-to-mutate) (melm mat-a 2 2))
         (cl:* (w vec4-to-mutate) (melm mat-a 3 2)))

   (cl:+ (cl:* (x vec4-to-mutate) (melm mat-a 0 3))
         (cl:* (y vec4-to-mutate) (melm mat-a 1 3))
         (cl:* (z vec4-to-mutate) (melm mat-a 2 3))
         (cl:* (w vec4-to-mutate) (melm mat-a 3 3)))))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun * (a b c &rest c) "(~a £*£ ~a £*£ ~a ~{ £*£ ~a~})"
                               (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro * ((a t) (b t) (c t) &rest (d t))
  `(* ,a (* ,b (* ,c ,@d))))

(varjo:v-def-glsl-template-fun * (a b) "(~a * ~a)" (:mat4 :mat4) :mat4 :pure t)

;;----------------------------------------------------------------
;; makes a matrix to orient something towards a point

(varjo:v-defun from-direction ((up3 :vec3) (dir3 :vec3))
  (let* ((zaxis (v3:normalize dir3))
         (xaxis (v3-n:normalize (v3:cross zaxis up3)))
         (yaxis (v3:cross xaxis zaxis)))
    (m4:make (x xaxis) (x yaxis) (cl:- (x zaxis)) 0f0
             (y xaxis) (y yaxis) (cl:- (y zaxis)) 0f0
             (z xaxis) (z yaxis) (cl:- (z zaxis)) 0f0
             0f0       0f0       0f0              1f0)))

;;----------------------------------------------------------------
;; makes a matrix to orient something at a point towards another point

(varjo:v-defun point-at ((up :vec3) (from3 :vec3) (to3 :vec3))
  (from-direction up (v3:- to3 from3)))

;;----------------------------------------------------------------
;; Makes a world->view look-at matrix

(varjo:v-defun look-at ((up3 :vec3) (from3 :vec3) (to3 :vec3))
  (let* ((zaxis (v3-n:normalize (v3:- to3 from3)))
         (xaxis (v3-n:normalize (v3:cross zaxis up3)))
         (yaxis (v3:cross xaxis zaxis)))
    (m4:make (x xaxis) (y xaxis) (z xaxis) (cl:- (v3:dot xaxis from3))
             (x yaxis) (y yaxis) (z yaxis) (cl:- (v3:dot yaxis from3))
             (cl:- (x zaxis)) (cl:- (y zaxis)) (cl:- (z zaxis)) (v3:dot zaxis from3)
             0f0 0f0 0f0 1f0)))

;;----------------------------------------------------------------
