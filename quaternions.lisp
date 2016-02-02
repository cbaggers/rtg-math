(in-package #:rtg-math.quaternions)

;;----------------------------------------------------------------

(defun w (quat)
  "Returns the w component of the quaternion"
  (svref quat 0))

(defun x (quat)
  "Returns the x component of the quaternion"
  (svref quat 1))

(defun y (quat)
  "Returns the y component of the quaternion"
  (svref quat 2))

(defun z (quat)
  "Returns the z component of the quaternion"
  (svref quat 3))

;;----------------------------------------------------------------;;

(defun 0! ()
  (q! 0.0 0.0 0.0 0.0))

(defun 0p (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (cl:= 0f0 (+ (* w w) (* x x) (* y y) (* z z)))))

(defun unitp (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (cl:= 0f0 (- 1.0 (* w w) (* x x) (* y y) (* z z)))))

(defun identity ()
  (q! 1.0 0.0 0.0 0.0))

(defun identity-p (quat)
  (and (cl:= 0f0 (- 1.0 (w quat)))
       (cl:= 0f0 (x quat))
       (cl:= 0f0 (y quat))
       (cl:= 0f0 (z quat))))

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
    (setf (svref q 0) w
          (svref q 1) x
          (svref q 2) y
          (svref q 3) z)
    q))

(defun from-matrix3 (mat3)
  (let ((trace (m3:trace mat3)))
    (if (> trace 0.0)
        (let* ((s (sqrt (+ 1.0 trace)))
               (recip (/ 0.5 s)))
          (q! (* s 0.5)
	      (* (- (m3:melm mat3 2 1) (m3:melm mat3 1 2)) recip)
	      (* (- (m3:melm mat3 0 2) (m3:melm mat3 2 0)) recip)
	      (* (- (m3:melm mat3 1 0) (m3:melm mat3 0 1)) recip)))
        (let* ((i (if (> (m3:melm mat3 1 1) (m3:melm mat3 0 0)) 1 0))
               (i (if (> (m3:melm mat3 2 2) (m3:melm mat3 i i)) 2 i))
               (j (mod (+ 1 i) 3))
               (k (mod (+ 1 j) 3))
               (s (sqrt (+ (- (m3:melm mat3 i i)
			      (m3:melm mat3 j j)
			      (m3:melm mat3 k k))
			   1.0)))
               (recip (/ 0.5 s))
               (quat (q! (* (- (m3:melm mat3 k j)
			       (m3:melm mat3 j k))
			    recip) 0.0 0.0 0.0)))
          (setf (svref quat i) (* s 0.5)
                (svref quat j) (* (+ (m3:melm mat3 j i) (m3:melm mat3 i j))
				  recip)
                (svref quat k) (* (+ (m3:melm mat3 k i) (m3:melm mat3 i k))
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
		   (* scale-factor (svref axis-vec3 0))
		   (* scale-factor (svref axis-vec3 1))
		   (* scale-factor (svref axis-vec3 2)))))))

(defun from-axies (x-axies y-axies z-axies)
  (from-matrix3
   (m3:make
    (svref x-axies 0) (svref y-axies 1) (svref z-axies 2)
    (svref x-axies 0) (svref y-axies 1) (svref z-axies 2)
    (svref x-axies 0) (svref y-axies 1) (svref z-axies 2))))

(defun from-look-at (from3 to3)
  (let* ((dir (v3:- from3 to3))
         (n-dir (v3:normalize dir))
         (right (v3:make (v:z n-dir) 0.0 (- (v:x n-dir))))
         (n-right (v3:normalize right))
         (up (v3:cross n-dir n-right)))
    (q:from-axies n-right up n-dir)))

(defun from-fixed-angles (x-rot y-rot z-rot)
  (let ((x-rot (/ x-rot 2.0))
        (y-rot (/ y-rot 2.0))
        (z-rot (/ z-rot 2.0)))
    (let ((cos-x (cos x-rot)) (sin-x (sin x-rot))
          (cos-y (cos y-rot)) (sin-y (sin y-rot))
          (cos-z (cos z-rot)) (sin-z (sin z-rot)))
      (q! (- (* cos-x cos-y cos-z) (* sin-x sin-y sin-z))
	  (- (* sin-x cos-y cos-z) (* cos-x sin-y sin-z))
	  (- (* cos-x sin-y cos-z) (* sin-x cos-y sin-z))
	  (- (* cos-x cos-y sin-z) (* sin-x sin-y cos-x))))))

(defun magnitude (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (sqrt (+ (* w w) (* x x) (* y y) (* z z)))))

(defun norm (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (+ (* w w) (* x x) (* y y) (* z z))))

(defun = (q1 q2)
  (and (cl:= 0f0 (- (w q2) (w q1))) (cl:= 0f0 (- (x q2) (x q1)))
       (cl:= 0f0 (- (y q2) (y q1))) (cl:= 0f0 (- (z q2) (z q1)))))

;;[TODO] This seems wrong!...but book says it's right
(defun /= (q1 q2)
  (not (or (cl:= 0f0 (- (w q2) (w q1))) (cl:= 0f0 (- (x q2) (x q1)))
           (cl:= 0f0 (- (y q2) (y q1))) (cl:= 0f0 (- (z q2) (z q1))))))

(defun copy (quat)
  (q! (w quat) (x quat) (y quat) (z quat)))

(defun get-axis-angle (quat)
  (list
   (let ((length (sqrt (- 1.0 (* (w quat) (w quat))))))
     (if (cl:= 0f0 length)
         (v3:make 0.0 0.0 0.0)
         (let ((length (/ 1.0 length)))
           (v3:make (* length (x quat))
		    (* length (y quat))
		    (* length (z quat))))))
   (* 2.0 (acos (w quat)))))

(defun normalize (quat)
  (let ((length-squared (v4:dot quat quat)))
    (if (cl:= 0f0 length-squared)
        (q0)
        (let ((factor (inv-sqrt length-squared)))
          (q! (* (w quat) factor)
	      (* (x quat) factor)
	      (* (y quat) factor)
	      (* (z quat) factor))))))

(defun qconjugate (quat)
  (q! (w quat) (- (x quat)) (- (y quat)) (- (z quat))))

(defun inverse (quat)
  (let ((norm (norm quat)))
    (if (cl:= 0f0 norm)
        (identity)
        (let ((norm-recip (/ 1.0 norm)))
          (q! (* norm-recip (w quat))
	      (- (* norm-recip (x quat)))
	      (- (* norm-recip (y quat)))
	      (- (* norm-recip (z quat))))))))

(defun + (&rest quats)
  (apply #'v4:+ quats))

(defun - (&rest quats)
  (apply #'v4:- quats))

(defun *s (quat-a scalar)
  (v4:* quat-a scalar))

(defun * (quat-a quat-b)
  (q! (- (* (w quat-a) (w quat-b))
	 (* (x quat-a) (x quat-b))
	 (* (y quat-a) (y quat-b))
	 (* (z quat-a) (z quat-b)))
      (- (+ (* (w quat-a) (x quat-b))
	    (* (x quat-a) (w quat-b))
	    (* (y quat-a) (z quat-b)))
	 (* (z quat-a) (y quat-b)))
      (- (+ (* (w quat-a) (y quat-b))
	    (* (y quat-a) (w quat-b))
	    (* (z quat-a) (x quat-b)))
	 (* (x quat-a) (z quat-b)))
      (- (+ (* (w quat-a) (z quat-b))
	    (* (z quat-a) (w quat-b))
	    (* (x quat-a) (y quat-b)))
	 (* (y quat-a) (x quat-b)))))

(defun to-mat3 (quat)
  (let ((w (w quat)) (x (x quat)) (y (y quat)) (z (z quat)))
    (let ((x2 (+ x x)) (y2 (+ y y)) (z2 (+ z z)))
      (let ((wx (* w x2))  (wy (* w y2))  (wz (* w z2))
            (xx (* x x2))  (xy (* x y2))  (xz (* x z2))
            (yy (* y y2))  (yz (* y z2))
            (zz (* z z2)))
        (m3:make
         (- 1.0 (+ yy zz)) (- xy wz)         (+ xz wy)
         (+ xy wz)         (- 1.0 (+ xx zz)) (- yz wx)
         (- xz wy)         (+ yz wx)         (- 1.0 (+ xx yy)))))))

(defun to-mat4 (quat)
  (let ((w (w quat))  (x (x quat))  (y (y quat))  (z (z quat)))
    (let ((x2 (+ x x)) (y2 (+ y y)) (z2 (+ z z)))
      (let ((wx (* w x2))  (wy (* w y2))  (wz (* w z2))
            (xx (* x x2))  (xy (* x y2))  (xz (* x z2))
            (yy (* y y2))  (yz (* y z2))
            (zz (* z z2)))
        (m4:make
         (- 1.0 (+ yy zz)) (- xy wz) (+ xz wy) 0.0
         (+ xy wz) (- 1.0 (+ xx zz)) (- yz wx) 0.0
         (- xz wy) (+ yz wx) (- 1.0 (+ xx yy)) 0.0
         0.0 0.0 0.0 1.0)))))

(defun dot (quat-a quat-b)
  (v4:dot quat-a quat-b))

;; [TODO] Look into assets (this should be a unit quaternion
(defun rotate (vec3 quat)
  "Rotate vec3 by quaternion. Assumes quaternion is normalized."
  (let* ((v-mult (* 2.0 (+ (* (x quat) (svref vec3 0))
                           (* (y quat) (svref vec3 1))
                           (* (z quat) (svref vec3 2)))))
         (cross-mult (* 2.0 (w quat)))
         (p-mult (- (* cross-mult (w quat)) 1.0)))
    (v3:make (+ (* p-mult (svref vec3 0))
		(* v-mult (x quat))
		(* cross-mult
		   (- (* (y quat) (svref vec3 2))
		      (* (z quat) (svref vec3 1)))))
	     (+ (* p-mult (svref vec3 1))
		(* v-mult (y quat))
		(* cross-mult
		   (- (* (z quat) (svref vec3 0))
		      (* (x quat) (svref vec3 2)))))
	     (+ (* p-mult (svref vec3 2))
		(* v-mult (z quat))
		(* cross-mult
		   (- (* (x quat) (svref vec3 1))
		      (* (y quat) (svref vec3 0))))))))

;; [TODO] Could be faster (see q+1 area)
(defun lerp (start-quat end-quat pos)
  "Linearaly interpolate between two quaternions. Note that this
   will always take the shortest path."
  ;; get cos of 'angle' between quaternions
  (let ((cos-angle (v4:dot start-quat end-quat)))
    (if (>= cos-angle 0f0)
        (q+1 (q* end-quat pos)
             (q* start-quat (- 1.0 pos)))
        (q+1 (q* end-quat pos)
             (q* start-quat (- pos 1.0))))))

(defun slerp (start-quat end-quat pos)
  "Spherically interpolate between two quaternions. Note that this
   will always take the shortest path."
  ;; get cos of 'angle' between quaternions
  (destructuring-bind (start-mult end-mult)
      (let ((cos-angle (v4:dot start-quat end-quat)))
        ;; if angle between quaternions is less than 90 degrees
        (if (> cos-angle 0f0)
            ;; if angle is greater than zero
            (if (> (- 1.0 cos-angle) 0f0)
                (let* ((angle (acos cos-angle))
                       (recip-sin-angle (/ 1.0 (sin angle))))
                  (list (* (sin (* (- 1.0 pos) angle))
                           recip-sin-angle)
                        (* (sin (* pos angle))
                           recip-sin-angle)))
                ;; angle is close to zero
                (list (- 1.0 pos) pos))
            ;; we take the shorter route
            ;; if angle is less that 180 degrees
            (if (> (+ 1.0 cos-angle) 0f0)
                (let* ((angle (acos (- cos-angle)))
                       (recip-sin-angle (/ 1.0 (sin angle))))
                  (list (* (sin (* (- pos 1.0) angle))
                           recip-sin-angle)
                        (* (sin (* pos angle))
                           recip-sin-angle)))
                ;; angle is close to 180 degrees
                (list (- pos 1.0) pos))))
    (q+1 (q* start-quat start-mult)
         (q* end-quat end-mult))))

(defun approx-slerp (start-quat end-quat pos)
  (let* ((cos-angle (v4:dot start-quat end-quat))
         (factor (expt (- 1.0 (* 0.7878088 cos-angle)) 2.0))
         (k (* 0.5069269 factor))
         (b (* 2.0 k))
         (c (* -3 k))
         (d (+ 1 k))
         (pos (+ (* pos (+ c (* b pos))) d)))
    ;; if angle is less than 90 degrees
    (if (> cos-angle 0f0)
        ;; use standard interp
        (q+1 (q* end-quat pos)
             (q* start-quat (- 1.0 pos)))
        ;; take shorter path
        (q+1 (q* end-quat pos)
             (q* start-quat (- pos 1.0))))))
