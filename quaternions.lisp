(in-package #:rtg-math.quaternions)

;;----------------------------------------------------------------

(defun w (quat)
  "Returns the w component of the quaternion"
  (aref quat 0))

(defun x (quat)
  "Returns the x component of the quaternion"
  (aref quat 1))

(defun y (quat)
  "Returns the y component of the quaternion"
  (aref quat 2))

(defun z (quat)
  "Returns the z component of the quaternion"
  (aref quat 3))

;;----------------------------------------------------------------;;

(declaim (inline q!)
         (ftype (function (single-float
                           single-float
                           single-float
                           single-float)
                          quaternion)
                q!))
(defun q! (w x y z)
  "This takes 4 floats and give back a vector4, this is just an
   array but it specifies the array type and populates it.
   For speed reasons it will not accept integers so make sure
   you hand it floats."
  (declare (single-float x y z w))
  (let ((q (make-array 4 :element-type `single-float)))
    (setf (aref q 0) w
          (aref q 1) x
          (aref q 2) y
          (aref q 3) z)
    q))

(defun 0! ()
  (q! 0.0 0.0 0.0 0.0))

(defun 0p (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (cl:= 0f0 (cl:+ (cl:* w w) (cl:* x x) (cl:* y y) (cl:* z z)))))

(defun unitp (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (cl:= 0f0 (cl:- 1.0 (cl:* w w) (cl:* x x) (cl:* y y) (cl:* z z)))))

(defun identity ()
  (q! 1.0 0.0 0.0 0.0))

(defun identity-p (quat)
  (and (cl:= 0f0 (cl:- 1.0 (w quat)))
       (cl:= 0f0 (x quat))
       (cl:= 0f0 (y quat))
       (cl:= 0f0 (z quat))))

(defun from-mat3 (mat3)
  (let ((trace (m3:trace mat3)))
    (if (> trace 0s0)
        (let* ((s (sqrt (cl:+ trace 1s0)))
               (recip (/ 0.5 s)))
          (q! (cl:* 0.5 s)
	      (cl:* (cl:- (m3:melm mat3 2 1) (m3:melm mat3 1 2)) recip)
	      (cl:* (cl:- (m3:melm mat3 0 2) (m3:melm mat3 2 0)) recip)
	      (cl:* (cl:- (m3:melm mat3 1 0) (m3:melm mat3 0 1)) recip)))

        (let* ((i (if (> (m3:melm mat3 1 1) (m3:melm mat3 0 0)) 1 0))
               (i (if (> (m3:melm mat3 2 2) (m3:melm mat3 i i)) 2 i))
               (j (mod (cl:+ 1 i) 3))
               (k (mod (cl:+ 1 j) 3))
               (s (sqrt (cl:+ (cl:- (m3:melm mat3 i i)
				    (m3:melm mat3 j j)
				    (m3:melm mat3 k k))
			      1.0)))
               (recip (/ 0.5 s))
               (quat (q! (cl:* (cl:- (m3:melm mat3 k j)
				     (m3:melm mat3 j k))
			       recip) 0.0 0.0 0.0)))
          (setf (aref quat i) (cl:* s 0.5)
                (aref quat j) (cl:* (cl:+ (m3:melm mat3 j i) (m3:melm mat3 i j))
				    recip)
                (aref quat k) (cl:* (cl:+ (m3:melm mat3 k i) (m3:melm mat3 i k))
				    recip))
          (normalize quat)))))

(defun from-mat3-no-normalize (mat3)
  (let ((trace (m3:trace mat3)))
    (if (> trace 0s0)
        (let* ((s (sqrt (cl:+ trace 1s0)))
               (recip (/ 0.5 s)))
          (q! (cl:* 0.5 s)
	      (cl:* (cl:- (m3:melm mat3 2 1) (m3:melm mat3 1 2)) recip)
	      (cl:* (cl:- (m3:melm mat3 0 2) (m3:melm mat3 2 0)) recip)
	      (cl:* (cl:- (m3:melm mat3 1 0) (m3:melm mat3 0 1)) recip)))

        (let* ((i (if (> (m3:melm mat3 1 1) (m3:melm mat3 0 0)) 1 0))
               (i (if (> (m3:melm mat3 2 2) (m3:melm mat3 i i)) 2 i))
               (j (mod (cl:+ 1 i) 3))
               (k (mod (cl:+ 1 j) 3))
               (s (sqrt (cl:+ (cl:- (m3:melm mat3 i i)
				    (m3:melm mat3 j j)
				    (m3:melm mat3 k k))
			      1.0)))
               (recip (/ 0.5 s))
               (quat (q! (cl:* (cl:- (m3:melm mat3 k j)
				     (m3:melm mat3 j k))
			       recip) 0.0 0.0 0.0)))
          (setf (aref quat i) (cl:* s 0.5)
                (aref quat j) (cl:* (cl:+ (m3:melm mat3 j i) (m3:melm mat3 i j))
				    recip)
                (aref quat k) (cl:* (cl:+ (m3:melm mat3 k i) (m3:melm mat3 i k))
				    recip))
          quat))))

(defun from-axis-angle (axis-vec3 angle)
  (let ((length (v3:length-squared axis-vec3)))
    (if (cl:= 0f0 length)
        (identity)
        (let* ((half-angle (/ angle 2.0))
               (sin-half-angle (sin half-angle))
               (cos-half-angle (cos half-angle))
               (scale-factor (/ sin-half-angle (sqrt length))))
          (v4:make cos-half-angle
		   (cl:* scale-factor (aref axis-vec3 0))
		   (cl:* scale-factor (aref axis-vec3 1))
		   (cl:* scale-factor (aref axis-vec3 2)))))))

(defun from-axies (x-axies y-axies z-axies)
  (from-mat3
   (m3:make
    (aref x-axies 0) (aref y-axies 1) (aref z-axies 2)
    (aref x-axies 0) (aref y-axies 1) (aref z-axies 2)
    (aref x-axies 0) (aref y-axies 1) (aref z-axies 2))))

(defun from-look-at (from3 to3)
  (let* ((dir (v3:- from3 to3))
         (n-dir (v3:normalize dir))
         (right (v3:make (v:z n-dir) 0.0 (cl:- (v:x n-dir))))
         (n-right (v3:normalize right))
         (up (v3:cross n-dir n-right)))
    (q:from-axies n-right up n-dir)))

(defun to-look-at (quat)
  (let ((v (v3:make 0s0 0s0 -1s0)))
    (m3:*v (to-mat3 quat) v)))

(defun to-look-at-vec4 (quat)
  (let ((v (v4:make 0s0 0s0 -1s0 0s0)))
    (m4:*v (to-mat4 quat) v)))

(defun from-fixed-angles (x-rot y-rot z-rot)
  (let ((x-rot (/ x-rot 2.0))
        (y-rot (/ y-rot 2.0))
        (z-rot (/ z-rot 2.0)))
    (let ((cos-x (cos x-rot)) (sin-x (sin x-rot))
          (cos-y (cos y-rot)) (sin-y (sin y-rot))
          (cos-z (cos z-rot)) (sin-z (sin z-rot)))
      (q! (cl:- (cl:* cos-x cos-y cos-z) (cl:* sin-x sin-y sin-z))
	  (cl:- (cl:* sin-x cos-y cos-z) (cl:* cos-x sin-y sin-z))
	  (cl:- (cl:* cos-x sin-y cos-z) (cl:* sin-x cos-y sin-z))
	  (cl:- (cl:* cos-x cos-y sin-z) (cl:* sin-x sin-y cos-x))))))

(defun magnitude (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (sqrt (cl:+ (cl:* w w) (cl:* x x) (cl:* y y) (cl:* z z)))))

(defun norm (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (cl:+ (cl:* w w) (cl:* x x) (cl:* y y) (cl:* z z))))

(defun = (q1 q2)
  (and (cl:= 0f0 (cl:- (w q2) (w q1))) (cl:= 0f0 (cl:- (x q2) (x q1)))
       (cl:= 0f0 (cl:- (y q2) (y q1))) (cl:= 0f0 (cl:- (z q2) (z q1)))))

;;[TODO] This seems wrong!...but book says it's right
(defun /= (q1 q2)
  (not (or (cl:= 0f0 (cl:- (w q2) (w q1))) (cl:= 0f0 (cl:- (x q2) (x q1)))
           (cl:= 0f0 (cl:- (y q2) (y q1))) (cl:= 0f0 (cl:- (z q2) (z q1))))))

(defun copy (quat)
  (q! (w quat) (x quat) (y quat) (z quat)))

(defun get-axis-angle (quat)
  (list
   (let ((length (sqrt (cl:- 1.0 (cl:* (w quat) (w quat))))))
     (if (cl:= 0f0 length)
         (v3:make 0.0 0.0 0.0)
         (let ((length (/ 1.0 length)))
           (v3:make (cl:* length (x quat))
		    (cl:* length (y quat))
		    (cl:* length (z quat))))))
   (cl:* 2.0 (acos (w quat)))))

(defun normalize (quat)
  (let ((length-squared (v4:dot quat quat)))
    (if (cl:= 0f0 length-squared)
        (0!)
        (let ((factor (inv-sqrt length-squared)))
          (q! (cl:* (w quat) factor)
	      (cl:* (x quat) factor)
	      (cl:* (y quat) factor)
	      (cl:* (z quat) factor))))))

(defun qconjugate (quat)
  (q! (w quat) (cl:- (x quat)) (cl:- (y quat)) (cl:- (z quat))))

(defun inverse (quat)
  (let ((norm (norm quat)))
    (if (cl:= 0f0 norm)
        (identity)
        (let ((norm-recip (/ 1.0 norm)))
          (q! (cl:* norm-recip (w quat))
	      (cl:- (cl:* norm-recip (x quat)))
	      (cl:- (cl:* norm-recip (y quat)))
	      (cl:- (cl:* norm-recip (z quat))))))))

(defun + (&rest quats)
  (apply #'v4:+ quats))

(define-compiler-macro + (&rest components)
  (reduce (lambda (a x) `(v4:+ ,a ,x)) components))

(defun - (&rest quats)
  (apply #'v4:- quats))

(define-compiler-macro - (&rest components)
  (reduce (lambda (a x) `(v4:- ,a ,x)) components))

(defun *s (quat-a scalar)
  (v:* quat-a scalar))

(defun * (quat-a quat-b)
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

(defun to-mat3 (quat)
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

(defun to-mat4 (quat)
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

(defun dot (quat-a quat-b)
  (v4:dot quat-a quat-b))

;; [TODO] Look into assets (this should be a unit quaternion
(defun rotate (vec3 quat)
  "Rotate vec3 by quaternion. Assumes quaternion is normalized."
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

;; [TODO] Could be faster (see q+1 area)
(defun lerp (start-quat end-quat pos)
  "Linearaly interpolate between two quaternions. Note that this
   will always take the shortest path."
  ;; get cos of 'angle' between quaternions
  (let ((cos-angle (v4:dot start-quat end-quat)))
    (if (>= cos-angle 0f0)
        (+ (*s end-quat pos)
	   (*s start-quat (cl:- 1.0 pos)))
        (+ (*s end-quat pos)
	   (*s start-quat (cl:- pos 1.0))))))

(defun slerp (start-quat end-quat pos)
  "Spherically interpolate between two quaternions. Note that this
   will always take the shortest path."
  ;; get cos of 'angle' between quaternions
  (destructuring-bind (start-mult end-mult)
      (let ((cos-angle (v4:dot start-quat end-quat)))
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

(defun approx-slerp (start-quat end-quat pos)
  (let* ((cos-angle (v4:dot start-quat end-quat))
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
