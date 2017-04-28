(in-package #:rtg-math.vector4)

;;----------------------------------------------------------------

(defn make ((x single-float) (y single-float)
            (z single-float) (w single-float)) vec4
  "This takes 4 floats and give back a vector4, this is just an
   array but it specifies the array type and populates it.
   For speed reasons it will not accept integers so make sure
   you hand it floats."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((vec (make-array 4 :element-type `single-float)))
    (setf (aref vec 0) x
          (aref vec 1) y
          (aref vec 2) z
          (aref vec 3) w)
    vec))

;;----------------------------------------------------------------

(defn-inline copy-vec4 ((vec4 vec4)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((vec (make-array 4 :element-type `single-float)))
    (setf (aref vec 0) (aref vec4 0)
          (aref vec 1) (aref vec4 1)
          (aref vec 2) (aref vec4 2)
          (aref vec 3) (aref vec4 3))
    vec))

;;----------------------------------------------------------------

;;[TODO] What is faster (cl:* x x) or (expt x 2) ?
(defn 0p ((vector-a vec4)) boolean
  "Checks if the length of the vector is zero. As this is a
   floating point number it checks to see if the length is
   below a threshold set in the base-maths package"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:= 0f0 (cl:+ (expt (aref vector-a 0) 2)
                  (expt (aref vector-a 1) 2)
                  (expt (aref vector-a 2) 2)
                  (expt (aref vector-a 3) 2))))

;;----------------------------------------------------------------

(defn unitp ((vector-a vec4)) boolean
  "Checks if the vector is of unit length. As this is a
   floating point number it checks to see if the length is
   within the range of 1 + or - and threshold set in base-maths"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:= 0f0 (cl:- 1.0 (cl:+ (expt (aref vector-a 0) 2)
                            (expt (aref vector-a 1) 2)
                            (expt (aref vector-a 2) 2)
                            (expt (aref vector-a 3) 2)))))

;;----------------------------------------------------------------

(defn = ((vector-a vec4) (vector-b vec4)) boolean
  "Returns either t if the two vectors are equal.
   Otherwise it returns nil."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (cl:= (aref vector-a 0) (aref vector-b 0))
       (cl:= (aref vector-a 1) (aref vector-b 1))
       (cl:= (aref vector-a 2) (aref vector-b 2))
       (cl:= (aref vector-a 3) (aref vector-b 3))))

;;----------------------------------------------------------------

(defn +s ((vec4 vec4) (scalar single-float)) vec4
  (make (cl:+ (x vec4) scalar)
        (cl:+ (y vec4) scalar)
        (cl:+ (z vec4) scalar)
        (cl:+ (w vec4) scalar)))

;;----------------------------------------------------------------

(defn -s ((vec4 vec4) (scalar single-float)) vec4
  (make (cl:- (x vec4) scalar)
        (cl:- (y vec4) scalar)
        (cl:- (z vec4) scalar)
        (cl:- (w vec4) scalar)))

;;----------------------------------------------------------------

(defn + (&rest (vec4s vec4)) vec4
  "takes any number of vectors and add them all together
   returning a new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if vec4s
      (let ((x 0f0)
            (y 0f0)
            (z 0f0)
            (w 0f0))
        (declare (single-float x y z w))
        (loop :for vec :in vec4s :do
           (let ((vec vec))
             (declare (vec4 vec))
             (incf x (x vec))
             (incf y (y vec))
             (incf z (z vec))
             (incf w (w vec))))
        (make x y z w))
      (make 0f0 0f0 0f0 0f0)))

(define-compiler-macro + (&whole whole &rest vec4s)
  (case= (cl:length vec4s)
    (0 (make 0f0 0f0 0f0 0f0))
    (1 (first vec4s))
    (2 `(%+ ,@vec4s))
    (otherwise whole)))

(defn %+ ((vector-a vec4) (vector-b vec4)) vec4
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:+ (aref vector-a 0) (aref vector-b 0))
        (cl:+ (aref vector-a 1) (aref vector-b 1))
        (cl:+ (aref vector-a 2) (aref vector-b 2))
        (cl:+ (aref vector-a 3) (aref vector-b 3))))

;;----------------------------------------------------------------

(defn - ((vec4 vec4) &rest (vec4s vec4)) vec4
  "takes any number of vectors and add them all together
   returning a new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (assert vec4)
  (if vec4s
      (let ((x (x vec4))
            (y (y vec4))
            (z (z vec4))
            (w (w vec4)))
        (declare (single-float x y z w))
        (loop :for vec :in vec4s :do
           (let ((vec vec))
             (declare (vec4 vec))
             (cl:decf x (x vec))
             (cl:decf y (y vec))
             (cl:decf z (z vec))
             (cl:decf w (w vec))))
        (make x y z w))
      vec4))

(define-compiler-macro - (&whole whole &rest vec4s)
  (case= (cl:length vec4s)
    (2 `(%- ,@vec4s))
    (otherwise whole)))

(defn %- ((vector-a vec4) (vector-b vec4)) vec4
  "Subtract two vectors and return a new vector containing
   the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (aref vector-a 0) (aref vector-b 0))
        (cl:- (aref vector-a 1) (aref vector-b 1))
        (cl:- (aref vector-a 2) (aref vector-b 2))
        (cl:- (aref vector-a 3) (aref vector-b 3))))

;;----------------------------------------------------------------

(defn *s ((vector-a vec4) (a single-float)) vec4
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:* (aref vector-a 0) a)
        (cl:* (aref vector-a 1) a)
        (cl:* (aref vector-a 2) a)
        (cl:* (aref vector-a 3) a)))

;;----------------------------------------------------------------

(defn * (&rest (vec4s vec4)) vec4
  "takes any number of vectors and multiply them all together
   returning a new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if vec4s
      (destructuring-bind (vec4 . vec4s) vec4s
        (declare (vec4 vec4))
        (let ((x (x vec4))
              (y (y vec4))
              (z (z vec4))
              (w (w vec4)))
          (declare (single-float x y z w))
          (loop :for vec :in vec4s :do
             (let ((vec vec))
               (declare (vec4 vec))
               (setf x (cl:* x (x vec)))
               (setf y (cl:* y (y vec)))
               (setf z (cl:* z (z vec)))
               (setf w (cl:* w (w vec)))))
          (make x y z w)))
      (make 1f0 1f0 1f0 1f0)))

(define-compiler-macro * (&whole whole &rest vec4s)
  (case= (cl:length vec4s)
    (0 `(make 1f0 1f0 1f0 1f0))
    (1 (first vec4s))
    (2 `(*v ,@vec4s))
    (otherwise whole)))

(defn *v ((vector-a vec4) (vector-b vec4)) vec4
  "Multiplies components, is not dot product, not sure what
   i'll need this for yet but hey!"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:* (aref vector-a 0) (aref vector-b 0))
        (cl:* (aref vector-a 1) (aref vector-b 1))
        (cl:* (aref vector-a 2) (aref vector-b 2))
        (cl:* (aref vector-a 3) (aref vector-b 3))))

;;----------------------------------------------------------------

(defn /s ((vector-a vec4) (a single-float)) vec4
  "divide vector by scalar and return result as new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((b (cl:/ 1 a)))
    (make (cl:* (aref vector-a 0) b)
          (cl:* (aref vector-a 1) b)
          (cl:* (aref vector-a 2) b)
          (cl:* (aref vector-a 3) b))))

;;----------------------------------------------------------------

(defn / ((vector-a vec4) (vector-b vec4)) vec4
  "Divides components, not sure what, i'll need this for
   yet but hey!"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:/ (aref vector-a 0) (aref vector-b 0))
        (cl:/ (aref vector-a 1) (aref vector-b 1))
        (cl:/ (aref vector-a 2) (aref vector-b 2))
        (cl:/ (aref vector-a 3) (aref vector-b 3))))

;;----------------------------------------------------------------

(defn negate ((vector-a vec4)) vec4
  "Return a vector that is the negative of the vector passed in"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (aref vector-a 0))
        (cl:- (aref vector-a 1))
        (cl:- (aref vector-a 2))
        (cl:- (aref vector-a 3))))

;;----------------------------------------------------------------

(defn dot ((vector-a vec4) (vector-b vec4)) single-float
  "return the dot product of the vector-a and vector-b."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (cl:* (aref vector-a 0) (aref vector-b 0))
        (cl:* (aref vector-a 1) (aref vector-b 1))
        (cl:* (aref vector-a 2) (aref vector-b 2))
        (cl:* (aref vector-a 3) (aref vector-b 3))))

;;----------------------------------------------------------------

(defn face-foreward ((vector-a vec4) (vector-b vec4)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if (> (print (dot vector-a vector-b)) 0)
      vector-a
      (negate vector-a)))

;;----------------------------------------------------------------

(defn length-squared ((vector-a vec4))
    (single-float 0f0 #.most-positive-single-float)
  "Return the squared length of the vector. A regular length
   is the square root of this value. The sqrt function is slow
   so if all thats needs doing is to compare lengths then always
   use the length squared function"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((x (x vector-a))
        (y (y vector-a))
        (z (z vector-a))
        (w (w vector-a)))
    (cl:+ (cl:* x x) (cl:* y y) (cl:* z z) (cl:* w w))))

;;----------------------------------------------------------------

(defn length ((vector-a vec4))
    (single-float 0f0 #.most-positive-single-float)
  "Returns the length of a vector
   If you only need to compare relative lengths then definately
   stick to length-squared as the sqrt is a slow operation."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (sqrt (length-squared vector-a)))

;;----------------------------------------------------------------

(defn distance-squared ((vector-a vec4) (vector-b vec4))
    (single-float 0f0 #.most-positive-single-float)
  "finds the squared distance between 2 points defined by vectors
   vector-a & vector-b"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (length-squared (- vector-b vector-a)))

;;----------------------------------------------------------------

(defn distance ((vector-a vec4) (vector-b vec4))
    (single-float 0f0 #.most-positive-single-float)
  "Return the distance between 2 points defined by vectors
   vector-a & vector-b. If comparing distances, use
   c-distance-squared as it desnt require a sqrt and thus is
   faster."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (sqrt (distance-squared vector-a vector-b)))


;;----------------------------------------------------------------

(defn abs ((vector-a vec4)) vec4
  "Return the vec4 containing the abs of the original vec4's components."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:abs (x vector-a)) (cl:abs (y vector-a))
        (cl:abs (y vector-a)) (cl:abs (z vector-a))))

;;----------------------------------------------------------------

(defn absolute-dot ((vector-a vec4) (vector-b vec4)) single-float
  "Return the absolute dot product of the vector-a and vector-b."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (cl:abs (cl:* (aref vector-a 0) (aref vector-b 0)))
        (cl:abs (cl:* (aref vector-a 1) (aref vector-b 1)))
        (cl:abs (cl:* (aref vector-a 2) (aref vector-b 2)))
        (cl:abs (cl:* (aref vector-a 3) (aref vector-b 3)))))

;;----------------------------------------------------------------

(defn normalize ((vector-a vec4)) vec4
  "This normalizes the vector, it makes sure a zero length
   vector won't throw an error."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((len (length-squared vector-a)))
    (if (cl:= 0f0 len)
        vector-a
        (*s vector-a (inv-sqrt len)))))

;;----------------------------------------------------------------

(defn lerp ((vector-a vec4) (vector-b vec4) (ammount single-float)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%+ (*s vector-a (cl:- 1f0 ammount))
      (*s vector-b ammount)))


(defn stable-lerp ((vector-a vec4) (vector-b vec4) (ammount single-float)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lerp vector-a vector-b ammount))

;;----------------------------------------------------------------

(defn bezier ((a1 vec4) (a2 vec4)
			  (b1 vec4) (b2 vec4)
			  (ammount single-float)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lerp (lerp a1 a2 ammount)
        (lerp b1 b2 ammount)
        ammount))

;;----------------------------------------------------------------

(defun spline (x knots)
  (make (rtg-math.maths:spline x (mapcar #'x knots))
        (rtg-math.maths:spline x (mapcar #'y knots))
        (rtg-math.maths:spline x (mapcar #'z knots))
        (rtg-math.maths:spline x (mapcar #'w knots))))
