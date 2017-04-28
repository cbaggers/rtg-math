(in-package #:rtg-math.vector3)

;;---------------------------------------------------------------

(defn-inline make ((x single-float) (y single-float) (z single-float)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((vec (make-array 3 :element-type `single-float)))
    (setf (aref vec 0) x
          (aref vec 1) y
          (aref vec 2) z)
    vec))

;;---------------------------------------------------------------

(defn-inline copy-vec3 ((vec3 vec3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((vec (make-array 3 :element-type `single-float)))
    (setf (aref vec 0) (aref vec3 0)
          (aref vec 1) (aref vec3 1)
          (aref vec 2) (aref vec3 2))
    vec))

;;---------------------------------------------------------------

(defn 0p ((vector-a vec3)) boolean
  "Checks if the length of the vector is zero. As this is a
   floating point number it checks to see if the length is
   below a threshold set in the base-maths package"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:= 0f0 (cl:+ (expt (aref vector-a 0) 2)
                  (expt (aref vector-a 1) 2)
                  (expt (aref vector-a 2) 2))))

;;---------------------------------------------------------------

(defn unitp ((vector-a vec3)) boolean
  "Checks if the vector is of unit length. As this is a
   floating point number it checks to see if the length is
   within the range of 1 + or - and threshold set in base-maths"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:= 0f0 (cl:- 1.0 (cl:+ (expt (aref vector-a 0) 2)
                            (expt (aref vector-a 1) 2)
                            (expt (aref vector-a 2) 2)))))
;;---------------------------------------------------------------

(defn = ((vector-a vec3) (vector-b vec3)) boolean
  "Returns either t if the two vectors are equal.
   Otherwise it returns nil."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (cl:= (aref vector-a 0) (aref vector-b 0))
       (cl:= (aref vector-a 1) (aref vector-b 1))
       (cl:= (aref vector-a 2) (aref vector-b 2))))

;;----------------------------------------------------------------

(defn +s ((vec3 vec3) (scalar single-float)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:+ (x vec3) scalar)
        (cl:+ (y vec3) scalar)
        (cl:+ (z vec3) scalar)))

;;----------------------------------------------------------------

(defn -s ((vec3 vec3) (scalar single-float)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (x vec3) scalar)
        (cl:- (y vec3) scalar)
        (cl:- (z vec3) scalar)))

;;---------------------------------------------------------------

(defn + (&rest (vec3s vec3)) vec3
  "takes any number of vectors and add them all together
   returning a new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if vec3s
      (let ((x 0f0)
            (y 0f0)
            (z 0f0))
        (declare (single-float x y z))
        (loop :for vec :in vec3s :do
           (let ((vec vec))
             (declare (vec3 vec))
             (cl:incf x (x vec))
             (cl:incf y (y vec))
             (cl:incf z (z vec))))
        (make x y z))
      (make 0f0 0f0 0f0)))

(define-compiler-macro + (&whole whole &rest vec3s)
  (case= (cl:length vec3s)
    (0 (make 0f0 0f0 0f0))
    (1 (first vec3s))
    (2 `(%+ ,@vec3s))
    (otherwise whole)))

(defn %+ ((vector-a vec3) (vector-b vec3)) vec3
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:+ (aref vector-a 0) (aref vector-b 0))
        (cl:+ (aref vector-a 1) (aref vector-b 1))
        (cl:+ (aref vector-a 2) (aref vector-b 2))))

;;---------------------------------------------------------------

(defn - ((vec3 vec3) &rest (vec3s vec3)) vec3
  "takes any number of vectors and add them all together
   returning a new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (assert vec3)
  (if vec3s
      (let ((x (x vec3))
            (y (y vec3))
            (z (z vec3)))
        (declare (single-float x y z))
        (loop :for vec :in vec3s :do
           (let ((vec vec))
             (declare (vec3 vec))
             (cl:decf x (x vec))
             (cl:decf y (y vec))
             (cl:decf z (z vec))))
        (make x y z))
      vec3))

(define-compiler-macro - (&whole whole &rest vec3s)
  (case= (cl:length vec3s)
    (2 `(%- ,@vec3s))
    (otherwise whole)))

(defn %- ((vector-a vec3) (vector-b vec3)) vec3
  "Subtract two vectors and return a new vector containing
   the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (aref vector-a 0) (aref vector-b 0))
        (cl:- (aref vector-a 1) (aref vector-b 1))
        (cl:- (aref vector-a 2) (aref vector-b 2))))

;;---------------------------------------------------------------

(defn *s ((vector-a vec3) (a single-float)) vec3
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:* (aref vector-a 0) a)
        (cl:* (aref vector-a 1) a)
        (cl:* (aref vector-a 2) a)))

;;---------------------------------------------------------------

(defn * (&rest (vec3s vec3)) vec3
  "takes any number of vectors and multiply them all together
   returning a new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if vec3s
      (destructuring-bind (vec3 . vec3s) vec3s
        (declare (vec3 vec3))
        (let ((x (x vec3))
              (y (y vec3))
              (z (z vec3)))
          (declare (single-float x y z))
          (loop :for vec :in vec3s :do
             (let ((vec vec))
               (declare (vec3 vec))
               (setf x (cl:* x (x vec)))
               (setf y (cl:* y (y vec)))
               (setf z (cl:* z (z vec)))))
          (make x y z)))
      (make 1f0 1f0 1f0)))

(define-compiler-macro * (&whole whole &rest vec3s)
  (case= (cl:length vec3s)
    (0 `(make 1f0 1f0 1f0))
    (1 (first vec3s))
    (2 `(*v ,@vec3s))
    (otherwise whole)))

(defn *v ((vector-a vec3) (vector-b vec3)) vec3
  "Multiplies components"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:* (aref vector-a 0) (aref vector-b 0))
        (cl:* (aref vector-a 1) (aref vector-b 1))
        (cl:* (aref vector-a 2) (aref vector-b 2))))

;;---------------------------------------------------------------

(defn /s ((vector-a vec3) (a single-float)) vec3
  "divide vector by scalar and return result as new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((b (cl:/ 1 a)))
    (make (cl:* (aref vector-a 0) b)
          (cl:* (aref vector-a 1) b)
          (cl:* (aref vector-a 2) b))))

;;---------------------------------------------------------------

(defn / ((vector-a vec3) (vector-b vec3)) vec3
  "Divides components"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:/ (aref vector-a 0) (aref vector-b 0))
        (cl:/ (aref vector-a 1) (aref vector-b 1))
        (cl:/ (aref vector-a 2) (aref vector-b 2))))

;;---------------------------------------------------------------

(defn negate ((vector-a vec3)) vec3
  "Return a vector that is the negative of the vector passed in"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (aref vector-a 0))
        (cl:- (aref vector-a 1))
        (cl:- (aref vector-a 2))))

;;----------------------------------------------------------------

(defn dot ((vector-a vec3) (vector-b vec3)) single-float
  "Return the dot product of the vector-a and vector-b."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (cl:* (aref vector-a 0) (aref vector-b 0))
        (cl:* (aref vector-a 1) (aref vector-b 1))
        (cl:* (aref vector-a 2) (aref vector-b 2))))

;;---------------------------------------------------------------

(defn face-foreward ((vector-a vec3) (vector-b vec3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if (> (dot vector-a vector-b) 0)
      vector-a
      (negate vector-a)))

;;---------------------------------------------------------------

(defn length-squared ((vector-a vec3))
    (single-float 0f0 #.most-positive-single-float)
  "Return the squared length of the vector. A regular length
   is the square root of this value. The sqrt function is slow
   so if all thats needs doing is to compare lengths then always
   use the length squared function"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((x (x vector-a))
        (y (y vector-a))
        (z (z vector-a)))
    (cl:+ (cl:* x x) (cl:* y y) (cl:* z z))))

;;---------------------------------------------------------------

(defn length ((vector-a vec3)) (single-float 0f0 #.most-positive-single-float)
  "Returns the length of a vector
   If you only need to compare relative lengths then definately
   stick to length-squared as the sqrt is a slow operation."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (sqrt (length-squared vector-a)))

;;---------------------------------------------------------------

(defn distance-squared ((vector-a vec3) (vector-b vec3))
    (single-float 0f0 #.most-positive-single-float)
  "finds the squared distance between 2 points defined by vectors
   vector-a & vector-b"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (length-squared (- vector-b vector-a)))

;;---------------------------------------------------------------

(defn distance ((vector-a vec3) (vector-b vec3))
    (single-float 0f0 #.most-positive-single-float)
  "Return the distance between 2 points defined by vectors
   vector-a & vector-b. If comparing distances, use
   c-distance-squared as it desnt require a sqrt and thus is
   faster."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (sqrt (distance-squared vector-a vector-b)))

;;---------------------------------------------------------------

(defn abs ((vector-a vec3)) vec3
  "Return the vec3 containing the abs of the original vec3's components."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:abs (x vector-a))
        (cl:abs (y vector-a))
        (cl:abs (y vector-a))))

;;---------------------------------------------------------------

(defn absolute-dot ((vector-a vec3) (vector-b vec3)) single-float
  "Return the absolute dot product of the vector-a and vector-b."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (cl:abs (cl:* (aref vector-a 0) (aref vector-b 0)))
        (cl:abs (cl:* (aref vector-a 1) (aref vector-b 1)))
        (cl:abs (cl:* (aref vector-a 2) (aref vector-b 2)))))

;;---------------------------------------------------------------

(defn normalize ((vector-a vec3)) vec3
  "This normalizes the vector, it makes sure a zero length
   vector won't throw an error."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((len (length-squared vector-a)))
    (if (cl:= 0f0 len)
        vector-a
        (*s vector-a (inv-sqrt len)))))

;;---------------------------------------------------------------

(defn cross ((vec-a vec3) (vec-b vec3)) vec3
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
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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

(defn lerp ((vector-a vec3) (vector-b vec3) (ammount single-float)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%+ (*s vector-a (cl:- 1f0 ammount))
      (*s vector-b ammount)))


(defn stable-lerp ((vector-a vec3) (vector-b vec3) (ammount single-float)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lerp vector-a vector-b ammount))

;;----------------------------------------------------------------

(defn bezier ((a1 vec3) (a2 vec3)
               (b1 vec3) (b2 vec3)
               (ammount single-float)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lerp (lerp a1 a2 ammount)
        (lerp b1 b2 ammount)
        ammount))

;;----------------------------------------------------------------

(defun spline (x knots)
  (make (rtg-math.maths:spline x (mapcar #'x knots))
        (rtg-math.maths:spline x (mapcar #'y knots))
        (rtg-math.maths:spline x (mapcar #'z knots))))
