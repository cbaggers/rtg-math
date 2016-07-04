(in-package #:rtg-math.vector3)

;;---------------------------------------------------------------

(defun-typed-inline make
    ((x single-float) (y single-float) (z single-float)) -> vec3
  (let (( vec (make-array 3 :element-type `single-float)))
    (setf (aref vec 0) x
	  (aref vec 1) y
	  (aref vec 2) z)
    vec))

;;---------------------------------------------------------------

;;[TODO] What is faster (cl:* x x) or (expt x 2) ?
(declaim (inline 0p)
         (ftype (function (vec3)
                          (boolean)) 0p))
(defun 0p (vector-a)
  "Checks if the length of the vector is zero. As this is a
   floating point number it checks to see if the length is
   below a threshold set in the base-maths package"
  (declare (vec3 vector-a))
  (cl:= 0f0 (cl:+ (EXPT (AREF VECTOR-A 0) 2) (EXPT (AREF VECTOR-A 1) 2)
		  (EXPT (AREF VECTOR-A 2) 2))))

;;---------------------------------------------------------------

(declaim (inline unitp)
         (ftype (function (vec3)
                          (boolean)) unitp))
(defun unitp (vector-a)
  "Checks if the vector is of unit length. As this is a
   floating point number it checks to see if the length is
   within the range of 1 + or - and threshold set in base-maths"
  (declare (vec3 vector-a))
  (cl:= 0f0 (cl:- 1.0 (cl:+ (EXPT (AREF VECTOR-A 0) 2) (EXPT (AREF VECTOR-A 1) 2)
			    (EXPT (AREF VECTOR-A 2) 2)))))
;;---------------------------------------------------------------

(declaim (inline =)
         (ftype (function (vec3
                           vec3)
                          (boolean)) =))
(defun = (vector-a vector-b)
  "Returns either t if the two vectors are equal.
   Otherwise it returns nil."
  (declare (vec3 vector-a vector-b))
  (AND (cl:= (AREF VECTOR-A 0) (AREF VECTOR-B 0))
       (cl:= (AREF VECTOR-A 1) (AREF VECTOR-B 1))
       (cl:= (AREF VECTOR-A 2) (AREF VECTOR-B 2))))

;;---------------------------------------------------------------

;; Not sure how to optomise this
(defun + (&rest vec3s)
  "takes any number of vectors and add them all together
   returning a new vector"
  (reduce #'%+ vec3s))

;;---------------------------------------------------------------

(declaim (inline %+)
         (ftype (function (vec3
                           vec3)
                          vec3) %+))
(defun %+ (vector-a vector-b)
  "Add two vectors and return a new vector containing the result"
  (declare (vec3 vector-a vector-b))
  (MAKE (cl:+ (AREF VECTOR-A 0) (AREF VECTOR-B 0))
	(cl:+ (AREF VECTOR-A 1) (AREF VECTOR-B 1))
	(cl:+ (AREF VECTOR-A 2) (AREF VECTOR-B 2))))

;;---------------------------------------------------------------

;; Not sure how to optomise this
(defun - (&rest vec3s)
  "takes any number of vectors and subtract them and return
   a new vector4"
  (reduce #'%- vec3s))

;;---------------------------------------------------------------

(declaim (inline %-)
         (ftype (function (vec3
                           vec3)
                          vec3) %-))
(defun %- (vector-a vector-b)
  "Subtract two vectors and return a new vector containing
   the result"
  (declare (vec3 vector-a vector-b))
  (MAKE (cl:- (AREF VECTOR-A 0) (AREF VECTOR-B 0))
	(cl:- (AREF VECTOR-A 1) (AREF VECTOR-B 1))
	(cl:- (AREF VECTOR-A 2) (AREF VECTOR-B 2))))

;;---------------------------------------------------------------

(declaim (inline *s)
         (ftype (function (vec3 single-float) vec3) *s))
(defun *s (vector-a a)
  "Multiply vector by scalar"
  (declare (vec3 vector-a)
           (single-float a))
  (MAKE (cl:* (AREF VECTOR-A 0) A) (cl:* (AREF VECTOR-A 1) A)
	(cl:* (AREF VECTOR-A 2) A)))

;;---------------------------------------------------------------

(declaim (inline *)
         (ftype (function (vec3
                           vec3)
                          vec3)
                *))
(defun * (vector-a vector-b)
  "Multiplies components, is not dot product, not sure what
   i'll need this for yet but hey!"
  (declare (vec3 vector-a vector-b))
  (MAKE (cl:* (AREF VECTOR-A 0) (AREF VECTOR-B 0))
	(cl:* (AREF VECTOR-A 1) (AREF VECTOR-B 1))
	(cl:* (AREF VECTOR-A 2) (AREF VECTOR-B 2))))

;;---------------------------------------------------------------

(declaim (inline /s)
         (ftype (function (vec3 single-float) vec3)
		/s))
(defun /s (vector-a a)
  "divide vector by scalar and return result as new vector"
  (declare (vec3 vector-a)
           (single-float a))
  (let ((b (cl:/ 1 a)))
    (MAKE (cl:* (AREF VECTOR-A 0) B) (cl:* (AREF VECTOR-A 1) B)
	  (cl:* (AREF VECTOR-A 2) B))))

;;---------------------------------------------------------------

(declaim (inline /)
         (ftype (function (vec3 vec3) vec3)
                /))
(defun / (vector-a vector-b)
  "Divides components, not sure what, i'll need this for
   yet but hey!"
  (declare (vec3 vector-a vector-b))
  (MAKE (cl:/ (AREF VECTOR-A 0) (AREF VECTOR-B 0))
	(cl:/ (AREF VECTOR-A 1) (AREF VECTOR-B 1))
	(cl:/ (AREF VECTOR-A 2) (AREF VECTOR-B 2))))

;;---------------------------------------------------------------

(declaim (inline negate)
         (ftype (function (vec3)
                          vec3)
                negate))
(defun negate (vector-a)
  "Return a vector that is the negative of the vector passed in"
  (declare (vec3 vector-a))
  (MAKE (cl:- (AREF VECTOR-A 0)) (cl:- (AREF VECTOR-A 1)) (cl:- (AREF VECTOR-A 2))))

;;----------------------------------------------------------------

(declaim (inline face-foreward)
         (ftype (function (vec3
                           vec3)
                          vec3)
                face-foreward))
(defun face-foreward (vector-a vector-b)
  (declare (vec3 vector-a vector-b))
  (if (> (print (dot vector-a vector-b)) 0)
      vector-a
      (negate vector-a)))

;;---------------------------------------------------------------

(declaim (inline length-squared)
         (ftype (function (vec3)
                          single-float) length-squared))
(defun length-squared (vector-a)
  "Return the squared length of the vector. A regular length
   is the square root of this value. The sqrt function is slow
   so if all thats needs doing is to compare lengths then always
   use the length squared function"
  (declare (vec3 vector-a))
  (let ((x (x vector-a))
        (y (y vector-a))
        (z (z vector-a)))
    (cl:+ (cl:* x x) (cl:* y y) (cl:* z z))))

;;---------------------------------------------------------------

(declaim (inline length)
         (ftype (function (vec3)
                          single-float) length))
(defun length (vector-a)
  "Returns the length of a vector
   If you only need to compare relative lengths then definately
   stick to length-squared as the sqrt is a slow operation."
  (declare (vec3 vector-a))
  (sqrt (length-squared vector-a)))

;;---------------------------------------------------------------

(declaim (inline distance-squared)
         (ftype (function (vec3
                           vec3)
                          single-float)
                distance-squared))
(defun distance-squared (vector-a vector-b)
  "finds the squared distance between 2 points defined by vectors
   vector-a & vector-b"
  (declare (vec3 vector-a vector-b))
  (length-squared (- vector-b vector-a)))

;;---------------------------------------------------------------

(declaim (inline distance)
         (ftype (function (vec3
                           vec3)
                          single-float)
                distance))
(defun distance (vector-a vector-b)
  "Return the distance between 2 points defined by vectors
   vector-a & vector-b. If comparing distances, use
   c-distance-squared as it desnt require a sqrt and thus is
   faster."
  (declare (vec3 vector-a vector-b))
  (sqrt (distance-squared vector-a vector-b)))

;;---------------------------------------------------------------

(declaim (inline dot)
         (ftype (function (vec3
                           vec3)
                          single-float)
                dot))
(defun dot (vector-a vector-b)
  "Return the dot product of the vector-a and vector-b."
  (declare (vec3 vector-a vector-b))
  (cl:+ (cl:* (AREF VECTOR-A 0) (AREF VECTOR-B 0))
        (cl:* (AREF VECTOR-A 1) (AREF VECTOR-B 1))
        (cl:* (AREF VECTOR-A 2) (AREF VECTOR-B 2))))

;;---------------------------------------------------------------

(declaim (inline absolute-dot)
         (ftype (function (vec3
                           vec3)
                          single-float)
                absolute-dot))
(defun absolute-dot (vector-a vector-b)
  "Return the absolute dot product of the vector-a and vector-b."
  (declare (vec3 vector-a vector-b))
  (cl:+ (ABS (cl:* (AREF VECTOR-A 0) (AREF VECTOR-B 0)))
        (ABS (cl:* (AREF VECTOR-A 1) (AREF VECTOR-B 1)))
        (ABS (cl:* (AREF VECTOR-A 2) (AREF VECTOR-B 2)))))

;;---------------------------------------------------------------

;; [TODO] shouldnt this return a zero vector in event of zero
;; length? does it matter?
(declaim (inline normalize)
         (ftype (function (vec3)
                          vec3)
                normalize))
(defun normalize (vector-a)
  "This normalizes the vector, it makes sure a zero length
   vector won't throw an error."
  (declare (vec3 vector-a))
  (let ((len (length-squared vector-a)))
    (if (cl:= 0f0 len)
        vector-a
        (*s vector-a (inv-sqrt len)))))

;;---------------------------------------------------------------

(declaim (inline cross)
         (ftype (function (vec3
                           vec3)
                          vec3)
                cross))
(defun cross (vec-a vec-b)
  "Calculates the cross-product of 2 vectors, i.e. the vector
   that lies perpendicular to them both. The resultign vector
   will <b>NOT</b> be normalized, to maximise efficiency
   The returned vector will be on the side from which the arc
   from u to v is anticlockwise.
   This is because CEPL uses a right-handed coordinate system.
   Another note on the cross product is that if vec-a and
   vec-b are normalized the length of the resulting vector
   will be sin(a) where a is the angle between the two vectors.
   The fact that we don't normalize may be useful in our
   quaternion functions later on."
  (declare (vec3 vec-a vec-b))
  (make
   (cl:- (cl:* (y vec-a) (z vec-b)) (cl:* (z vec-a) (y vec-b)))
   (cl:- (cl:* (z vec-a) (x vec-b)) (cl:* (x vec-a) (z vec-b)))
   (cl:- (cl:* (x vec-a) (y vec-b)) (cl:* (y vec-a) (x vec-b)))))


;;----------------------------------------------------------------

(defun closest-point-on-line (line point)
  (destructuring-bind (line-point line-dir) line
    (let* ((diff-vec (- point line-point))
           (dist-sq (dot line-dir line-dir))
           (projection (dot diff-vec line-dir)))
      (v:+ line-point (*s line-dir (cl:/ projection dist-sq))))))

(defun closest-point-on-norm-line (line point)
  (destructuring-bind (line-point line-dir) line
    (let* ((diff-vec (- point line-point))
           (projection (dot diff-vec line-dir)))
      (v:+ line-point (*s line-dir projection)))))

(defun distance-to-line-sq (line point)
  (destructuring-bind (line-point line-dir) line
    (let* ((diff-vec (- point line-point))
           (dist-sq (dot line-dir line-dir))
           (diff-sq (dot diff-vec diff-vec))
           (projection (dot diff-vec line-dir)))
      (cl:- diff-sq (cl:/ (cl:* projection projection) dist-sq)))))

(defun distance-to-norm-line-sq (line point)
  (destructuring-bind (line-point line-dir) line
    (let* ((diff-vec (- point line-point))
           (diff-sq (dot diff-vec diff-vec))
           (projection (dot diff-vec line-dir)))
      (cl:- diff-sq (cl:* projection projection)))))

;;----------------------------------------------------------------

(declaim (inline lerp)
         (ftype (function (vec3
                           vec3
                           single-float)
                          vec3)
                lerp))
(defun lerp (vector-a vector-b ammount)
  (declare (vec3 vector-a vector-b))
  (%+ vector-a (*s (%- vector-b vector-a) ammount)))

;; (declaim (inline slerp)
;;          (ftype (function (vec3
;;                            vec3
;;                            single-float)
;;                           vec3)
;;                 slerp))
;; (defun slerp3 (vector-a vector-b ammount)
;;   (let ((angle (cl:* (acos (clampf -1.0 1.0 (dot vector-a vector-b))) ammount))
;;         (relative-vec (normalize (%- vector-b (*s vector-a dot)))))
;;     (%+ (*s vector-a (cos angle)) (*s relative-vec (sin angle)))))

;;----------------------------------------------------------------

(declaim (inline bezier)
         (ftype (function (vec3
                           vec3
                           vec3
                           vec3
                           single-float)
                          vec3)
                bezier))
(defun bezier (a1 a2 b1 b2 ammount)
  (declare (vec3 a1 a2 b1 b2)
           (single-float ammount))
  (lerp (lerp a1 a2 ammount)
        (lerp b1 b2 ammount)
        ammount))

;;----------------------------------------------------------------

(defun spline (x knots)
  (make (rtg-math.maths:spline x (mapcar #'x knots))
	(rtg-math.maths:spline x (mapcar #'y knots))
	(rtg-math.maths:spline x (mapcar #'z knots))))
