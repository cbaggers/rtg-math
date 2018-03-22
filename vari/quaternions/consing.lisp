(in-package #:rtg-math.quaternions)

;;----------------------------------------------------------------;;

(varjo:v-def-glsl-template-fun
 q! (w x y z) "vec4(~a,~a,~a,~a)" (:float :float :float :float) quaternion)

(varjo:v-def-glsl-template-fun
 make (w x y z) "vec4(~a,~a,~a,~a)" (:float :float :float :float) quaternion)

(varjo:v-def-glsl-template-fun
 0! () "vec4(0)" () quaternion)

(varjo:v-def-glsl-template-fun
 = (a b) "(~a == ~a)" (quaternion quaternion) :bool :pure t)

(varjo:v-def-glsl-template-fun
 /= (a b) "(~a != ~a)" (quaternion quaternion) :bool :pure t)

(varjo:v-def-glsl-template-fun
 copy (a) "vec4(~a)" (quaternion) quaternion :pure t)

(varjo:v-def-glsl-template-fun
 0p (v) "(~a==0)" (quaternion) :bool)

(vari:v-defun unitp ((quat quaternion))
  (cl:= 1f0 (dot quat quat)))

(varjo:v-def-glsl-template-fun
 identity () "vec4(1,0,0,0)" () quaternion)

(vari:v-defun identity-p ((quat quaternion))
  (and (cl:= 1f0 (cl:- 1.0 (w quat)))
       (cl:= 0f0 (x quat))
       (cl:= 0f0 (y quat))
       (cl:= 0f0 (z quat))))

;; (vari:v-defun from-mat3 ((mat3 :mat3))
;;   (let ((trace (m3:trace mat3)))
;;     (if (> trace 0f0)
;;         (let* ((s (sqrt (cl:+ trace 1f0)))
;;                (recip (/ 0.5 s)))
;;           (set-components
;;            (cl:* 0.5 s)
;;            (cl:* (cl:- (m3:melm mat3 2 1) (m3:melm mat3 1 2)) recip)
;;            (cl:* (cl:- (m3:melm mat3 0 2) (m3:melm mat3 2 0)) recip)
;;            (cl:* (cl:- (m3:melm mat3 1 0) (m3:melm mat3 0 1)) recip)
;;            quat-to-mutate))
;;         ;;
;;         (let* ((i (if (> (m3:melm mat3 1 1) (m3:melm mat3 0 0)) 1 0))
;;                (i (if (> (m3:melm mat3 2 2) (m3:melm mat3 i i)) 2 i))
;;                (j (mod (cl:+ 1 i) 3))
;;                (k (mod (cl:+ 1 j) 3))
;;                (tmp-s (cl:+ (cl:- (m3:melm mat3 i i)
;;                                   (m3:melm mat3 j j)
;;                                   (m3:melm mat3 k k))
;;                             1.0))
;;                (s (sqrt (the (single-float 0f0 #.most-positive-single-float)
;;                              tmp-s)))
;;                (recip (/ 0.5 s)))
;;           (setf (w quat-to-mutate) (cl:* (cl:- (m3:melm mat3 k j)
;;                                                (m3:melm mat3 j k))
;;                                          recip))
;;           (setf (aref quat-to-mutate (1+ i)) (cl:* s 0.5))
;;           (setf (aref quat-to-mutate (1+ j))
;;                 (cl:* (cl:+ (m3:melm mat3 j i) (m3:melm mat3 i j))
;;                       recip))
;;           (setf (aref quat-to-mutate (1+ k))
;;                 (cl:* (cl:+ (m3:melm mat3 k i) (m3:melm mat3 i k))
;;                       recip))
;;           (normalize quat-to-mutate)))))

(vari:v-defun from-axis-angle ((axis-vec3 :vec3) (angle :float))
  (let* ((half-angle (/ angle 2.0))
         (sin-half-angle (sin half-angle))
         (cos-half-angle (cos half-angle))
         (scale-factor (/ sin-half-angle (sqrt length))))
    (make cos-half-angle
          (cl:* scale-factor (aref axis-vec3 0))
          (cl:* scale-factor (aref axis-vec3 1))
          (cl:* scale-factor (aref axis-vec3 2)))))

;; (defn from-axies ((x-axies :vec3) (y-axies :vec3) (z-axies :vec3))
;;   (from-mat3
;;    (m3:make
;;     (aref x-axies 0) (aref y-axies 1) (aref z-axies 2)
;;     (aref x-axies 0) (aref y-axies 1) (aref z-axies 2)
;;     (aref x-axies 0) (aref y-axies 1) (aref z-axies 2))))

;; (defn look-at ((up3 vec3) (from3 vec3) (to3 vec3)) quaternion
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (from-mat3
;;    (m3:look-at up3 from3 to3)))

;; (defn point-at ((up3 vec3) (from3 vec3) (to3 vec3)) quaternion
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (from-mat3
;;    (m3:point-at up3 from3 to3)))

;; (defn from-direction ((up3 vec3) (dir3 vec3)) quaternion
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (from-mat3
;;    (m3:from-direction up3 dir3)))

(vari:v-defun to-direction ((quat quaternion))
  (rotate (v3:make 0f0 0f0 -1f0) quat))

(vari:v-defun to-direction-vec4 ((quat quaternion))
  (rotate-v4 (v4:make 0f0 0f0 -1f0 0f0) quat))

(vari:v-defun from-fixed-angles ((x-rot :float)
                                 (y-rot :float)
                                 (z-rot :float))
  (let* ((x-rot (/ x-rot 2.0))
         (y-rot (/ y-rot 2.0))
         (z-rot (/ z-rot 2.0))
         (cos-x (cos x-rot))
         (sin-x (sin x-rot))
         (cos-y (cos y-rot))
         (sin-y (sin y-rot))
         (cos-z (cos z-rot))
         (sin-z (sin z-rot)))
    (normalize
     (q! (cl:- (cl:* cos-x cos-y cos-z) (cl:* sin-x sin-y sin-z))
         (cl:- (cl:* sin-x cos-y cos-z) (cl:* cos-x sin-y sin-z))
         (cl:- (cl:* cos-x sin-y cos-z) (cl:* sin-x cos-y sin-z))
         (cl:- (cl:* cos-x cos-y sin-z) (cl:* sin-x sin-y cos-x))))))

(vari:v-defun from-fixed-angles-v3 ((angles :vec3))
  (from-fixed-angles (v:x angles) (v:y angles) (v:z angles)))

(vari:v-def-glsl-template-fun
 magnitude (q) "length(~a)" (quaternion) :float)

(vari:v-def-glsl-template-fun
 norm (q) "dot(~a)" (quaternion) :float)

(vari:v-defun get-axis-angle ((quat quaternion))
  (let ((w (w quat)))
    (values
     (let ((length (sqrt (cl:- 1.0 (cl:* w w)))))
       (let ((length (/ 1.0 length)))
         (v3:make (cl:* length (x quat))
                  (cl:* length (y quat))
                  (cl:* length (z quat)))))
     (cl:* 2.0 (acos w)))))

(vari:v-defun normalize ((quat quaternion))
  (let* ((length-squared (dot quat quat))
         (factor (inv-sqrt length-squared)))
    (q! (cl:* (w quat) factor)
        (cl:* (x quat) factor)
        (cl:* (y quat) factor)
        (cl:* (z quat) factor))))


(vari:v-defun conjugate ((quat quaternion))
  (q! (w quat)
      (cl:- (x quat))
      (cl:- (y quat))
      (cl:- (z quat))))

(vari:v-defun inverse ((quat quaternion))
  (let* ((norm (norm quat))
        (norm-recip (/ 1.0 norm)))
    (q! (cl:* norm-recip (w quat))
        (cl:- (cl:* norm-recip (x quat)))
        (cl:- (cl:* norm-recip (y quat)))
        (cl:- (cl:* norm-recip (z quat))))))

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun + (a b c &rest c) "(~a £+£ ~a £+£ ~a ~{ £+£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro + ((a t) (b t) (c t) &rest (d t))
  `(+ ,a (+ ,b (+ ,c ,@d))))

(varjo:v-def-glsl-template-fun + (a) "~a" (quaternion) 0 :pure t)
(varjo:v-def-glsl-template-fun + (a b) "(~a + ~a)" (quaternion quaternion) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun - (a b c &rest c) "(~a £-£ ~a £-£ ~a ~{ £-£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(varjo:v-define-compiler-macro - ((a t) (b t) (c t) &rest (d t))
  `(- ,a (- ,b (- ,c ,@d))))

(varjo:v-def-glsl-template-fun - (a) "(-~a)" (quaternion) 0 :pure t)
(varjo:v-def-glsl-template-fun - (a b) "(~a - ~a)" (quaternion quaternion) 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 *s (v s) "(~a * ~a)" (quaternion :float) quaternion :pure t)

;;----------------------------------------------------------------

(vari:v-defun * ((quat-a quaternion) (quat-b quaternion))
  (q! (cl:- (cl:* (w quat-a) (w quat-b))
            (cl:* (x quat-a) (x quat-b))
            (cl:* (y quat-a) (y quat-b))
            (cl:* (z quat-a) (z quat-b)))
      (cl:- (cl:+ (cl:* (w quat-a) (x quat-b))
                  (cl:* (x quat-a) (w quat-b))
                  (cl:* (y quat-a) (z quat-b)))
            (cl:* (z quat-a) (y quat-b)))
      (cl:- (cl:+ (cl:* (w quat-a) (y quat-b))
                  (cl:* (y quat-a) (w quat-b))
                  (cl:* (z quat-a) (x quat-b)))
            (cl:* (x quat-a) (z quat-b)))
      (cl:- (cl:+ (cl:* (w quat-a) (z quat-b))
                  (cl:* (z quat-a) (w quat-b))
                  (cl:* (x quat-a) (y quat-b)))
            (cl:* (y quat-a) (x quat-b)))))

;;----------------------------------------------------------------

(vari:v-defun to-mat3 ((quat quaternion))
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (let ((x2 (cl:+ x x)) (y2 (cl:+ y y)) (z2 (cl:+ z z)))
      (let ((wx (cl:* w x2))  (wy (cl:* w y2))  (wz (cl:* w z2))
            (xx (cl:* x x2))  (xy (cl:* x y2))  (xz (cl:* x z2))
            (yy (cl:* y y2))  (yz (cl:* y z2))
            (zz (cl:* z z2)))
        (m3:make
         (cl:- 1.0 (cl:+ yy zz)) (cl:- xy wz)            (cl:+ xz wy)
         (cl:+ xy wz)            (cl:- 1.0 (cl:+ xx zz)) (cl:- yz wx)
         (cl:- xz wy)            (cl:+ yz wx)            (cl:- 1.0 (cl:+ xx yy)))))))

(vari:v-defun to-mat4 ((quat quaternion))
  (let ((w (w quat))  (x (x quat))  (y (y quat))  (z (z quat)))
    (let ((x2 (cl:+ x x)) (y2 (cl:+ y y)) (z2 (cl:+ z z)))
      (let ((wx (cl:* w x2))  (wy (cl:* w y2))  (wz (cl:* w z2))
            (xx (cl:* x x2))  (xy (cl:* x y2))  (xz (cl:* x z2))
            (yy (cl:* y y2))  (yz (cl:* y z2))
            (zz (cl:* z z2)))
        (m4:make
         (cl:- 1.0 (cl:+ yy zz))  (cl:- xy wz)            (cl:+ xz wy)             0.0
         (cl:+ xy wz)             (cl:- 1.0 (cl:+ xx zz)) (cl:- yz wx)             0.0
         (cl:- xz wy)             (cl:+ yz wx)            (cl:- 1.0 (cl:+ xx yy))  0.0
         0.0                      0.0                     0.0                      1.0)))))

;;----------------------------------------------------------------

;; [TODO] Look into assets (this should be a unit quaternion
(varjo:v-defun rotate ((vec3 :vec3) (quat quaternion))
  (let* ((v-mult (cl:* 2.0 (cl:+ (cl:* (x quat) (aref vec3 0))
                                 (cl:* (y quat) (aref vec3 1))
                                 (cl:* (z quat) (aref vec3 2)))))
         (cross-mult (cl:* 2.0 (w quat)))
         (p-mult (cl:- (cl:* cross-mult (w quat)) 1.0)))
    (v3:make (cl:+ (cl:* p-mult (aref vec3 0))
                   (cl:* v-mult (x quat))
                   (cl:* cross-mult
                         (cl:- (cl:* (y quat) (aref vec3 2))
                               (cl:* (z quat) (aref vec3 1)))))
             (cl:+ (cl:* p-mult (aref vec3 1))
                   (cl:* v-mult (y quat))
                   (cl:* cross-mult
                         (cl:- (cl:* (z quat) (aref vec3 0))
                               (cl:* (x quat) (aref vec3 2)))))
             (cl:+ (cl:* p-mult (aref vec3 2))
                   (cl:* v-mult (z quat))
                   (cl:* cross-mult
                         (cl:- (cl:* (x quat) (aref vec3 1))
                               (cl:* (y quat) (aref vec3 0))))))))

(vari:v-defun rotate-v4 ((vec4 :vec4) (quat quaternion))
  (let* ((v-mult (cl:* 2.0 (cl:+ (cl:* (x quat) (aref vec4 0))
                                 (cl:* (y quat) (aref vec4 1))
                                 (cl:* (z quat) (aref vec4 2)))))
         (cross-mult (cl:* 2.0 (w quat)))
         (p-mult (cl:- (cl:* cross-mult (w quat)) 1.0)))
    (v4:make (cl:+ (cl:* p-mult (aref vec4 0))
                   (cl:* v-mult (x quat))
                   (cl:* cross-mult
                         (cl:- (cl:* (y quat) (aref vec4 2))
                               (cl:* (z quat) (aref vec4 1)))))
             (cl:+ (cl:* p-mult (aref vec4 1))
                   (cl:* v-mult (y quat))
                   (cl:* cross-mult
                         (cl:- (cl:* (z quat) (aref vec4 0))
                               (cl:* (x quat) (aref vec4 2)))))
             (cl:+ (cl:* p-mult (aref vec4 2))
                   (cl:* v-mult (z quat))
                   (cl:* cross-mult
                         (cl:- (cl:* (x quat) (aref vec4 1))
                               (cl:* (y quat) (aref vec4 0)))))
             (aref vec4 3))))

;;----------------------------------------------------------------

(vari:v-defun lerp ((start-quat quaternion) (end-quat quaternion) (pos :float))
  (let ((cos-angle (dot start-quat end-quat)))
    (if (>= cos-angle 0f0)
        (+ (*s end-quat pos)
           (*s start-quat (cl:- 1.0 pos)))
        (+ (*s end-quat pos)
           (*s start-quat (cl:- pos 1.0))))))

(vari:v-defun slerp ((start-quat quaternion)
                     (end-quat quaternion)
                     (pos :float))
  ;; get cos of 'angle' between quaternions
  (multiple-value-bind (start-mult end-mult)
      (let ((cos-angle (dot start-quat end-quat)))
        ;; if angle between quaternions is less than 90 degrees
        (if (> cos-angle 0f0)
            ;; if angle is greater than zero
            (if (> (cl:- 1.0 cos-angle) 0f0)
                (let* ((angle (acos cos-angle))
                       (recip-sin-angle (/ 1.0 (sin angle))))
                  (values (cl:* (sin (cl:* (cl:- 1.0 pos) angle))
                                recip-sin-angle)
                          (cl:* (sin (cl:* pos angle))
                                recip-sin-angle)))
                ;; angle is close to zero
                (values (cl:- 1.0 pos) pos))
            ;; we take the shorter route
            ;; if angle is less that 180 degrees
            (if (> (cl:+ 1.0 cos-angle) 0f0)
                (let* ((angle (acos (cl:- cos-angle)))
                       (recip-sin-angle (/ 1.0 (sin angle))))
                  (values (cl:* (sin (cl:* (cl:- pos 1.0) angle))
                                recip-sin-angle)
                          (cl:* (sin (cl:* pos angle))
                                recip-sin-angle)))
                ;; angle is close to 180 degrees
                (values (cl:- pos 1.0) pos))))
    (+ (*s start-quat start-mult)
       (*s end-quat end-mult))))

(vari:v-defun approx-slerp ((start-quat quaternion)
                            (end-quat quaternion)
                            (pos :float))
  (let* ((cos-angle (dot start-quat end-quat))
         (factor (expt (cl:- 1.0 (cl:* 0.7878088 cos-angle)) 2.0))
         (k (cl:* 0.5069269 factor))
         (b (cl:* 2.0 k))
         (c (cl:* -3 k))
         (d (cl:+ 1 k))
         (pos (cl:+ (cl:* pos (cl:+ c (cl:* b pos))) d)))
    ;; if angle is less than 90 degrees
    (if (> cos-angle 0f0)
        ;; use standard interp
        (+ (*s end-quat pos)
           (*s start-quat (cl:- 1.0 pos)))
        ;; take shorter path
        (+ (*s end-quat pos)
           (*s start-quat (cl:- pos 1.0))))))
