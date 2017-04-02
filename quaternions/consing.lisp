(in-package #:rtg-math.quaternions)

;;----------------------------------------------------------------;;

(defn q! ((w single-float) (x single-float) (y single-float) (z single-float))
    quaternion
  "This takes 4 floats and give back a vector4, this is just an
   array but it specifies the array type and populates it.
   For speed reasons it will not accept integers so make sure
   you hand it floats."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((q (make-array 4 :element-type `single-float)))
    (setf (aref q 0) w
          (aref q 1) x
          (aref q 2) y
          (aref q 3) z)
    q))

(defn-inline 0! () quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make-array 4 :element-type 'single-float :initial-element 0f0))

(defn 0p ((quat quaternion)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (cl:= 0f0 (cl:+ (cl:* w w) (cl:* x x) (cl:* y y) (cl:* z z)))))

(defn unitp ((quat quaternion)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (cl:= 0f0 (cl:- 1.0 (cl:* w w) (cl:* x x) (cl:* y y) (cl:* z z)))))

(defn identity () quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (q! 1.0 0.0 0.0 0.0))

(defn identity-p ((quat quaternion)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (cl:= 0f0 (cl:- 1.0 (w quat)))
       (cl:= 0f0 (x quat))
       (cl:= 0f0 (y quat))
       (cl:= 0f0 (z quat))))

(defn from-mat3 ((mat3 mat3)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (q-n:from-mat3 (0!) mat3))

(defn from-axis-angle ((axis-vec3 vec3) (angle single-float)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (q-n:from-axis-angle (0!) axis-vec3 angle))

(defn from-axies ((x-axies vec3) (y-axies vec3) (z-axies vec3)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (from-mat3
   (m3:make
    (aref x-axies 0) (aref y-axies 1) (aref z-axies 2)
    (aref x-axies 0) (aref y-axies 1) (aref z-axies 2)
    (aref x-axies 0) (aref y-axies 1) (aref z-axies 2))))

(defn from-look-at ((from3 vec3) (to3 vec3)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((dir (v3:- from3 to3))
         (n-dir (v3:normalize dir))
         (right (v3:make (v:z n-dir) 0.0 (cl:- (v:x n-dir))))
         (n-right (v3:normalize right))
         (up (v3:cross n-dir n-right)))
    (q:from-axies n-right up n-dir)))

(defn to-look-at ((quat quaternion)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((v (v3:make 0f0 0f0 -1f0)))
    (m3:*v (to-mat3 quat) v)))

(defn to-look-at-vec4 ((quat quaternion)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((v (v4:make 0f0 0f0 -1f0 0f0)))
    (m4:*v (to-mat4 quat) v)))

(defn from-fixed-angles ((x-rot single-float) (y-rot single-float)
                         (z-rot single-float)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (q-n:from-fixed-angles (0!) x-rot y-rot z-rot))

(defn magnitude ((quat quaternion)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (sqrt (cl:+ (cl:* w w) (cl:* x x) (cl:* y y) (cl:* z z)))))

(defn norm ((quat quaternion)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (cl:+ (cl:* w w) (cl:* x x) (cl:* y y) (cl:* z z))))

(defn = ((q1 quaternion) (q2 quaternion)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (cl:= 0f0 (cl:- (w q2) (w q1)))
       (cl:= 0f0 (cl:- (x q2) (x q1)))
       (cl:= 0f0 (cl:- (y q2) (y q1)))
       (cl:= 0f0 (cl:- (z q2) (z q1)))))

(defn /= ((q1 quaternion) (q2 quaternion)) boolean
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (not (or (cl:= 0f0 (cl:- (w q2) (w q1)))
           (cl:= 0f0 (cl:- (x q2) (x q1)))
           (cl:= 0f0 (cl:- (y q2) (y q1)))
           (cl:= 0f0 (cl:- (z q2) (z q1))))))

(defn copy ((quat quaternion)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (q! (w quat) (x quat) (y quat) (z quat)))

(defn get-axis-angle ((quat quaternion)) list
  "Gets one possible axis-angle pair that will generate this
   quaternion

   Assumes that this is a normalized quaternion. It is critical that this
   is true as otherwise you will at best get a runtime error, and most likely a
   silently incorrect result."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((w (w quat)))
    (declare (type (single-float -1f0 1f0) w))
    (list
     (let ((length (sqrt (cl:- 1.0 (cl:* w w)))))
       (if (cl:= 0f0 length)
           (v3:make 0.0 0.0 0.0)
           (let ((length (/ 1.0 length)))
             (v3:make (cl:* length (x quat))
                      (cl:* length (y quat))
                      (cl:* length (z quat))))))
     (cl:* 2.0 (acos w)))))

(defn normalize ((quat quaternion)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((length-squared (dot quat quat)))
    (declare ((single-float 0f0 #.most-positive-single-float)
              length-squared))
    (if (cl:= 0f0 length-squared)
        (0!)
        (let ((factor (inv-sqrt length-squared)))
          (q! (cl:* (w quat) factor)
              (cl:* (x quat) factor)
              (cl:* (y quat) factor)
              (cl:* (z quat) factor))))))

(defn qconjugate ((quat quaternion)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (warn "q:qconjugate is deprecated, please switch to q:conjugate")
  (q! (w quat) (cl:- (x quat)) (cl:- (y quat)) (cl:- (z quat))))

(defn conjugate ((quat quaternion)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (q! (w quat) (cl:- (x quat)) (cl:- (y quat)) (cl:- (z quat))))

(defn inverse ((quat quaternion)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((norm (norm quat)))
    (if (cl:= 0f0 norm)
        (identity)
        (let ((norm-recip (/ 1.0 norm)))
          (q! (cl:* norm-recip (w quat))
              (cl:- (cl:* norm-recip (x quat)))
              (cl:- (cl:* norm-recip (y quat)))
              (cl:- (cl:* norm-recip (z quat))))))))

;;----------------------------------------------------------------

(defn %+ ((quat-a quaternion) (quat-b quaternion)) quaternion
  "Add two quats and return a new quat containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (q! (cl:+ (w quat-a) (w quat-b))
      (cl:+ (x quat-a) (x quat-b))
      (cl:+ (y quat-a) (y quat-b))
      (cl:+ (z quat-a) (z quat-b))))

(defn + (&rest (quats quaternion)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if quats
      (let ((w 0f0)
            (x 0f0)
            (y 0f0)
            (z 0f0))
        (declare (single-float x y z w))
        (loop :for vec :in quats :do
           (incf w (w vec))
           (incf x (x vec))
           (incf y (y vec))
           (incf z (z vec)))
        (q! w x y z))
      (identity)))

(define-compiler-macro + (&rest components)
  (reduce (lambda (a x) `(%+ ,a ,x)) components))

;;----------------------------------------------------------------

(defn %- ((quat-a quaternion) (quat-b quaternion)) quaternion
  "Add two quats and return a new quat containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (q! (cl:- (w quat-a) (w quat-b))
      (cl:- (x quat-a) (x quat-b))
      (cl:- (y quat-a) (y quat-b))
      (cl:- (z quat-a) (z quat-b))))

(defn - ((quat quaternion) &rest (quats quaternion)) quaternion
  "takes any number of vectors and add them all together
   returning a new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (assert quat)
  (if quats
      (let ((x (x quat))
            (y (y quat))
            (z (z quat))
            (w (w quat)))
        (declare (single-float x y z w))
        (loop :for vec :in quats :do
           (cl:decf x (x vec))
           (cl:decf y (y vec))
           (cl:decf z (z vec))
           (cl:decf w (w vec)))
        (q! x y z w))
      quat))

(define-compiler-macro - (&rest components)
  (reduce (lambda (a x) `(%- ,a ,x)) components))

;;----------------------------------------------------------------

(defn *s ((quat-a quaternion) (scalar single-float)) quaternion
  "Multiply quat by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (q! (cl:* (w quat-a) scalar)
      (cl:* (x quat-a) scalar)
      (cl:* (y quat-a) scalar)
      (cl:* (z quat-a) scalar)))

;;----------------------------------------------------------------

(defn * ((quat-a quaternion) (quat-b quaternion)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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

(defn to-mat3 ((quat quaternion)) mat3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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

(defn to-mat4 ((quat quaternion)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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
(defn rotate ((vec3 vec3) (quat quaternion)) vec3
  "Rotate vec3 by quaternion. Assumes quaternion is normalized."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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

;;----------------------------------------------------------------

(defn lerp ((start-quat quaternion) (end-quat quaternion) (pos single-float))
    quaternion
  "Linearaly interpolate between two quaternions. Note that this
   will always take the shortest path."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  ;; get cos of 'angle' between quaternions
  (let ((cos-angle (dot start-quat end-quat)))
    (if (>= cos-angle 0f0)
        (+ (*s end-quat pos)
           (*s start-quat (cl:- 1.0 pos)))
        (+ (*s end-quat pos)
           (*s start-quat (cl:- pos 1.0))))))

(defn slerp ((start-quat quaternion) (end-quat quaternion) (pos single-float))
    quaternion
  "Spherically interpolate between two quaternions. Note that this
   will always take the shortest path."
  ;;(declare (optimize (speed 3) (safety 1) (debug 1)))
  ;; get cos of 'angle' between quaternions
  (destructuring-bind (start-mult end-mult)
      (let ((cos-angle (dot start-quat end-quat)))
        ;; if angle between quaternions is less than 90 degrees
        (if (> cos-angle 0f0)
            ;; if angle is greater than zero
            (if (> (cl:- 1.0 cos-angle) 0f0)
                (let* ((angle (acos cos-angle))
                       (recip-sin-angle (/ 1.0 (sin angle))))
                  (list (cl:* (sin (cl:* (cl:- 1.0 pos) angle))
                              recip-sin-angle)
                        (cl:* (sin (cl:* pos angle))
                              recip-sin-angle)))
                ;; angle is close to zero
                (list (cl:- 1.0 pos) pos))
            ;; we take the shorter route
            ;; if angle is less that 180 degrees
            (if (> (cl:+ 1.0 cos-angle) 0f0)
                (let* ((angle (acos (cl:- cos-angle)))
                       (recip-sin-angle (/ 1.0 (sin angle))))
                  (list (cl:* (sin (cl:* (cl:- pos 1.0) angle))
                              recip-sin-angle)
                        (cl:* (sin (cl:* pos angle))
                              recip-sin-angle)))
                ;; angle is close to 180 degrees
                (list (cl:- pos 1.0) pos))))
    (+ (*s start-quat start-mult)
       (*s end-quat end-mult))))

(defn approx-slerp ((start-quat quaternion) (end-quat quaternion)
                    (pos single-float)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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
