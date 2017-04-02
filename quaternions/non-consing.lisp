(in-package #:rtg-math.quaternions.non-consing)

;;----------------------------------------------------------------

(defn-inline set-components ((w single-float) (x single-float)
                             (y single-float) (z single-float)
                             (quat quaternion)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (w quat) w
        (x quat) x
        (y quat) y
        (z quat) z)
  quat)

;;----------------------------------------------------------------

(defn normalize ((quat-to-mutate quaternion)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((length-squared (dot quat-to-mutate quat-to-mutate)))
    (declare ((single-float 0f0 #.most-positive-single-float)
              length-squared))
    (if (cl:= 0f0 length-squared)
        quat-to-mutate
        (let ((factor (inv-sqrt length-squared)))
          (set-components (cl:* (w quat-to-mutate) factor)
                          (cl:* (x quat-to-mutate) factor)
                          (cl:* (y quat-to-mutate) factor)
                          (cl:* (z quat-to-mutate) factor)
                          quat-to-mutate)))))

;;----------------------------------------------------------------

(defn from-mat3 ((quat-to-mutate quaternion) (mat3 mat3)) quaternion
  "Creates a quaternion from a 3x3 rotation matrix

   Assumes that this is a rotation matrix. It is critical that this
   is true (and elements are between -1f0 and 1f0) as otherwise you will
   at best get a runtime error, and most likely a silently incorrect result."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((trace (m3:trace mat3)))
    (if (> trace 0f0)
        (let* ((s (sqrt (cl:+ trace 1f0)))
               (recip (/ 0.5 s)))
          (set-components
           (cl:* 0.5 s)
           (cl:* (cl:- (m3:melm mat3 2 1) (m3:melm mat3 1 2)) recip)
           (cl:* (cl:- (m3:melm mat3 0 2) (m3:melm mat3 2 0)) recip)
           (cl:* (cl:- (m3:melm mat3 1 0) (m3:melm mat3 0 1)) recip)
           quat-to-mutate))
        ;;
        (let* ((i (if (> (m3:melm mat3 1 1) (m3:melm mat3 0 0)) 1 0))
               (i (if (> (m3:melm mat3 2 2) (m3:melm mat3 i i)) 2 i))
               (j (mod (cl:+ 1 i) 3))
               (k (mod (cl:+ 1 j) 3))
               (tmp-s (cl:+ (cl:- (m3:melm mat3 i i)
                                  (m3:melm mat3 j j)
                                  (m3:melm mat3 k k))
                            1.0))
               (s (sqrt (the (single-float 0f0 #.most-positive-single-float)
                             tmp-s)))
               (recip (/ 0.5 s)))
          (setf (w quat-to-mutate) (cl:* (cl:- (m3:melm mat3 k j)
                                               (m3:melm mat3 j k))
                                         recip))
          (setf (aref quat-to-mutate (1+ i)) (cl:* s 0.5))
          (setf (aref quat-to-mutate (1+ j))
                (cl:* (cl:+ (m3:melm mat3 j i) (m3:melm mat3 i j))
                      recip))
          (setf (aref quat-to-mutate (1+ k))
                (cl:* (cl:+ (m3:melm mat3 k i) (m3:melm mat3 i k))
                      recip))
          (normalize quat-to-mutate)))))

;;----------------------------------------------------------------

(defn from-axis-angle ((quat-to-mutate quaternion)
                       (axis-vec3 vec3) (angle single-float)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((length (v3:length-squared axis-vec3)))
    (if (cl:= 0f0 length)
        (set-components 1f0 0f0 0f0 0f0
                        quat-to-mutate)
        (let* ((half-angle (/ angle 2.0))
               (sin-half-angle (sin half-angle))
               (cos-half-angle (cos half-angle))
               (scale-factor (/ sin-half-angle (sqrt length))))
          (set-components cos-half-angle
                          (cl:* scale-factor (aref axis-vec3 0))
                          (cl:* scale-factor (aref axis-vec3 1))
                          (cl:* scale-factor (aref axis-vec3 2))
                          quat-to-mutate)))))

;;----------------------------------------------------------------

(defn from-fixed-angles ((quat-to-mutate quaternion)
                         (x-rot single-float) (y-rot single-float)
                         (z-rot single-float)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((x-rot (/ x-rot 2.0))
         (y-rot (/ y-rot 2.0))
         (z-rot (/ z-rot 2.0))
         (cos-x (cos x-rot)) (sin-x (sin x-rot))
         (cos-y (cos y-rot)) (sin-y (sin y-rot))
         (cos-z (cos z-rot)) (sin-z (sin z-rot)))
    (set-components (cl:- (cl:* cos-x cos-y cos-z) (cl:* sin-x sin-y sin-z))
                    (cl:- (cl:* sin-x cos-y cos-z) (cl:* cos-x sin-y sin-z))
                    (cl:- (cl:* cos-x sin-y cos-z) (cl:* sin-x cos-y sin-z))
                    (cl:- (cl:* cos-x cos-y sin-z) (cl:* sin-x sin-y cos-x))
                    quat-to-mutate)))

;;----------------------------------------------------------------

(defn conjugate ((quat-to-mutate quaternion)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (w quat-to-mutate)
                  (cl:- (x quat-to-mutate))
                  (cl:- (y quat-to-mutate))
                  (cl:- (z quat-to-mutate))
                  quat-to-mutate))

;;----------------------------------------------------------------

(defn %+ ((accum-quat quaternion) (to-add-quat quaternion)) quaternion
  "Add two quattors and return a new quattor containing the result"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl:incf (aref accum-quat 0) (aref to-add-quat 0))
  (cl:incf (aref accum-quat 1) (aref to-add-quat 1))
  (cl:incf (aref accum-quat 2) (aref to-add-quat 2))
  (cl:incf (aref accum-quat 3) (aref to-add-quat 3))
  accum-quat)

(defn + ((accum-quat quaternion) &rest (quaternions quaternion)) quaternion
  "Add two quattors and return a new quattor containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for quat :in quaternions :do (%+ accum-quat quat))
  accum-quat)

(define-compiler-macro + (&whole whole accum-quat &rest quaternions)
  (assert accum-quat)
  (case= (cl:length quaternions)
    (0 accum-quat)
    (1 `(%+ ,accum-quat ,(first quaternions)))
    (otherwise whole)))

;;---------------------------------------------------------------

(defn %- ((accum-quat quaternion) (to-add-quat quaternion)) quaternion
  "Add two quattors and return a new quattor containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:decf (aref accum-quat 0) (aref to-add-quat 0))
  (cl:decf (aref accum-quat 1) (aref to-add-quat 1))
  (cl:decf (aref accum-quat 2) (aref to-add-quat 2))
  (cl:decf (aref accum-quat 2) (aref to-add-quat 3))
  accum-quat)

(defn - ((accum-quat quaternion) &rest (quaternions quaternion)) quaternion
  "Add two quattors and return a new quattor containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for quat :in quaternions :do (%- accum-quat quat))
  accum-quat)

(define-compiler-macro - (&whole whole accum-quat &rest quaternions)
  (assert accum-quat)
  (case= (cl:length quaternions)
    (0 accum-quat)
    (1 `(%- ,accum-quat ,(first quaternions)))
    (otherwise whole)))

;;----------------------------------------------------------------

(defn * ((quat-to-mutate quaternion) (quat-b quaternion)) quaternion
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (cl:- (cl:* (w quat-to-mutate) (w quat-b))
                        (cl:* (x quat-to-mutate) (x quat-b))
                        (cl:* (y quat-to-mutate) (y quat-b))
                        (cl:* (z quat-to-mutate) (z quat-b)))
                  (cl:- (cl:+ (cl:* (w quat-to-mutate) (x quat-b))
                              (cl:* (x quat-to-mutate) (w quat-b))
                              (cl:* (y quat-to-mutate) (z quat-b)))
                        (cl:* (z quat-to-mutate) (y quat-b)))
                  (cl:- (cl:+ (cl:* (w quat-to-mutate) (y quat-b))
                              (cl:* (y quat-to-mutate) (w quat-b))
                              (cl:* (z quat-to-mutate) (x quat-b)))
                        (cl:* (x quat-to-mutate) (z quat-b)))
                  (cl:- (cl:+ (cl:* (w quat-to-mutate) (z quat-b))
                              (cl:* (z quat-to-mutate) (w quat-b))
                              (cl:* (x quat-to-mutate) (y quat-b)))
                        (cl:* (y quat-to-mutate) (x quat-b)))
                  quat-to-mutate))

;;----------------------------------------------------------------

(defn to-mat3 ((mat-to-mutate mat3) (quat quaternion)) mat3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat))
         ;;
         (x2 (cl:+ x x)) (y2 (cl:+ y y)) (z2 (cl:+ z z))
         ;;
         (wx (cl:* w x2))  (wy (cl:* w y2))  (wz (cl:* w z2))
         (xx (cl:* x x2))  (xy (cl:* x y2))  (xz (cl:* x z2))
         (yy (cl:* y y2))  (yz (cl:* y z2))
         (zz (cl:* z z2)))
    (m3-n:set-components
     (cl:- 1.0 (cl:+ yy zz)) (cl:- xy wz)            (cl:+ xz wy)
     (cl:+ xy wz)            (cl:- 1.0 (cl:+ xx zz)) (cl:- yz wx)
     (cl:- xz wy)            (cl:+ yz wx)            (cl:- 1.0 (cl:+ xx yy))
     mat-to-mutate)))

;;----------------------------------------------------------------

(defn to-mat4 ((mat-to-mutate mat4) (quat quaternion)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((w (w quat))  (x (x quat))  (y (y quat))  (z (z quat))
         ;;
         (x2 (cl:+ x x)) (y2 (cl:+ y y)) (z2 (cl:+ z z))
         ;;
         (wx (cl:* w x2))  (wy (cl:* w y2))  (wz (cl:* w z2))
         (xx (cl:* x x2))  (xy (cl:* x y2))  (xz (cl:* x z2))
         (yy (cl:* y y2))  (yz (cl:* y z2))
         (zz (cl:* z z2)))
    (m4-n:set-components
     (cl:- 1.0 (cl:+ yy zz))  (cl:- xy wz)            (cl:+ xz wy)             0.0
     (cl:+ xy wz)             (cl:- 1.0 (cl:+ xx zz)) (cl:- yz wx)             0.0
     (cl:- xz wy)             (cl:+ yz wx)            (cl:- 1.0 (cl:+ xx yy))  0.0
     0.0                      0.0                     0.0                      1.0
     mat-to-mutate)))

;;----------------------------------------------------------------

(defn rotate ((vec3-to-mutate vec3) (quat quaternion)) vec3
  "Rotate vec3 by quaternion. Assumes quaternion is normalized."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((v-mult (cl:* 2.0 (cl:+ (cl:* (x quat) (aref vec3-to-mutate 0))
                                 (cl:* (y quat) (aref vec3-to-mutate 1))
                                 (cl:* (z quat) (aref vec3-to-mutate 2)))))
         (cross-mult (cl:* 2.0 (w quat)))
         (p-mult (cl:- (cl:* cross-mult (w quat)) 1.0)))
    (v3-n:set-components
     (cl:+ (cl:* p-mult (aref vec3-to-mutate 0))
           (cl:* v-mult (x quat))
           (cl:* cross-mult
                 (cl:- (cl:* (y quat) (aref vec3-to-mutate 2))
                       (cl:* (z quat) (aref vec3-to-mutate 1)))))
     (cl:+ (cl:* p-mult (aref vec3-to-mutate 1))
           (cl:* v-mult (y quat))
           (cl:* cross-mult
                 (cl:- (cl:* (z quat) (aref vec3-to-mutate 0))
                       (cl:* (x quat) (aref vec3-to-mutate 2)))))
     (cl:+ (cl:* p-mult (aref vec3-to-mutate 2))
           (cl:* v-mult (z quat))
           (cl:* cross-mult
                 (cl:- (cl:* (x quat) (aref vec3-to-mutate 1))
                       (cl:* (y quat) (aref vec3-to-mutate 0)))))
     vec3-to-mutate)))

;;----------------------------------------------------------------
