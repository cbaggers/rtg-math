(in-package :rtg-math.vector2)

;;----------------------------------------------------------------

(defn make ((x single-float) (y single-float)) vec2
  "This takes 2 floats and give back a vector2, this is just an
   array but it specifies the array type and populates it.
   For speed reasons it will not accept integers so make sure
   you hand it floats."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v2-n:set-components x y (make-array 2 :element-type `single-float)))

;;---------------------------------------------------------------

(defn-inline copy-vec2 ((vec2 vec2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((vec (make-array 2 :element-type `single-float)))
    (setf (aref vec 0) (aref vec2 0)
          (aref vec 1) (aref vec2 1))
    vec))

;;----------------------------------------------------------------

(defn 0p ((vector-a vec2)) boolean
  "Checks if the length of the vector is zero. As this is a
   floating point number it checks to see if the length is
   below a threshold set in the base-maths package"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:= 0f0 (cl:+ (expt (aref vector-a 0) 2)
                  (expt (aref vector-a 1) 2))))

;;----------------------------------------------------------------

(defn unitp ((vector-a vec2)) boolean
  "Checks if the vector is of unit length. As this is a
   floating point number it checks to see if the length is
   within the range of 1 + or - and threshold set in base-maths"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:= 0f0 (cl:- 1.0 (cl:+ (expt (aref vector-a 0) 2)
                            (expt (aref vector-a 1) 2)))))
;;----------------------------------------------------------------

(defn = ((vector-a vec2) (vector-b vec2)) boolean
  "Returns either t if the two vectors are equal.
   Otherwise it returns nil."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (cl:= (aref vector-a 0) (aref vector-b 0))
       (cl:= (aref vector-a 1) (aref vector-b 1))))

;;----------------------------------------------------------------

(defn +s ((vec2 vec2) (scalar single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:+ (x vec2) scalar)
        (cl:+ (y vec2) scalar)))

;;----------------------------------------------------------------

(defn -s ((vec2 vec2) (scalar single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (x vec2) scalar)
        (cl:- (y vec2) scalar)))

;;----------------------------------------------------------------

(defn %+ ((vector-a vec2) (vector-b vec2)) vec2
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:+ (aref vector-a 0) (aref vector-b 0))
        (cl:+ (aref vector-a 1) (aref vector-b 1))))

(defn + (&rest (vec2s vec2)) vec2
  "takes any number of vectors and add them all together
   returning a new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if vec2s
      (let ((x 0f0)
            (y 0f0))
        (declare (single-float x y))
        (loop :for vec :in vec2s :do
           (let ((vec vec))
             (declare (vec2 vec))
             (incf x (x vec))
             (incf y (y vec))))
        (make x y))
      (make 0f0 0f0)))

(define-compiler-macro + (&whole whole &rest vec2s)
  (case= (cl:length vec2s)
    (0 (make 0f0 0f0))
    (1 (first vec2s))
    (2 `(%+ ,@vec2s))
    (otherwise whole)))

;;----------------------------------------------------------------

(defn %- ((vector-a vec2) (vector-b vec2)) vec2
  "Subtract two vectors and return a new vector containing
   the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (aref vector-a 0) (aref vector-b 0))
        (cl:- (aref vector-a 1) (aref vector-b 1))))

(defn - ((vec2 vec2) &rest (vec2s vec2)) vec2
  "takes any number of vectors and add them all together
   returning a new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (assert vec2)
  (if vec2s
      (let ((x (x vec2))
            (y (y vec2)))
        (declare (single-float x y))
        (loop :for vec :in vec2s :do
           (let ((vec vec))
             (declare (vec2 vec))
             (decf x (x vec))
             (decf y (y vec))))
        (make x y))
      vec2))

(define-compiler-macro - (&whole whole &rest vec2s)
  (case= (cl:length vec2s)
    (2 `(%- ,@vec2s))
    (otherwise whole)))

;;----------------------------------------------------------------

(defn * (&rest (vec2s vec2)) vec2
  "takes any number of vectors and multiply them all together
   returning a new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if vec2s
      (destructuring-bind (vec2 . vec2s) vec2s
        (declare (vec2 vec2))
        (let ((x (x vec2))
              (y (y vec2)))
          (declare (single-float x y))
          (loop :for vec :in vec2s :do
             (let ((vec vec))
               (declare (vec2 vec))
               (setf x (cl:* x (x vec)))
               (setf y (cl:* y (y vec)))))
          (make x y)))
      (make 1f0 1f0)))

(define-compiler-macro * (&whole whole &rest vec2s)
  (case= (cl:length vec2s)
    (0 `(make 1f0 1f0))
    (1 (first vec2s))
    (2 `(*v ,@vec2s))
    (otherwise whole)))

(defn *v ((vector-a vec2) (vector-b vec2)) vec2
  "Multiplies components, is not dot product, not sure what
   i'll need this for yet but hey!"
  (declare (vec2 vector-a vector-b))
  (make (cl:* (aref vector-a 0) (aref vector-b 0))
        (cl:* (aref vector-a 1) (aref vector-b 1))))

;;----------------------------------------------------------------

(defn *s ((vector-a vec2) (a single-float)) vec2
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:* (aref vector-a 0) a)
        (cl:* (aref vector-a 1) a)))

;;----------------------------------------------------------------

(defn /s ((vector-a vec2) (a single-float)) vec2
  "divide vector by scalar and return result as new vector"
  (declare (vec2 vector-a)
           (single-float a))
  (let ((b (cl:/ 1 a)))
    (make (cl:* (aref vector-a 0) b) (cl:* (aref vector-a 1) b))))

;;----------------------------------------------------------------

(defn / ((vector-a vec2) (vector-b vec2)) vec2
  "Divides components, not sure what, i'll need this for
   yet but hey!"
  (declare (vec2 vector-a vector-b))
  (make (cl:/ (aref vector-a 0)
              (aref vector-b 0))
        (cl:/ (aref vector-a 1)
              (aref vector-b 1))))

;;----------------------------------------------------------------

(defn negate ((vector-a vec2)) vec2
  "Return a vector that is the negative of the vector passed in"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (aref vector-a 0)) (cl:- (aref vector-a 1))))

;;----------------------------------------------------------------

(defn dot ((vector-a vec2) (vector-b vec2)) single-float
  "Return the dot product of the vector-a and vector-b."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (cl:* (x vector-a) (x vector-b))
        (cl:* (y vector-a) (y vector-b))))

;;----------------------------------------------------------------

(defn face-foreward ((vector-a vec2) (vector-b vec2)) vec2
  (if (> (dot vector-a vector-b) 0)
      vector-a
      (negate vector-a)))

;;----------------------------------------------------------------

(defn length-squared ((vector-a vec2))
    (single-float 0f0 #.most-positive-single-float)
  "Return the squared length of the vector. A regular length
   is the square root of this value. The sqrt function is slow
   so if all thats needs doing is to compare lengths then always
   use the length squared function"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((x (x vector-a))
        (y (y vector-a)))
    (cl:+ (cl:* x x) (cl:* y y))))

;;----------------------------------------------------------------

(defn length ((vector-a vec2))
    (single-float 0f0 #.most-positive-single-float)
  "Returns the length of a vector
   If you only need to compare relative lengths then definately
   stick to length-squared as the sqrt is a slow operation."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (sqrt (length-squared vector-a)))

;;----------------------------------------------------------------

(defn distance-squared ((vector-a vec2) (vector-b vec2))
    (single-float 0f0 #.most-positive-single-float)
  "finds the squared distance between 2 points defined by vectors
   vector-a & vector-b"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (length-squared (- vector-b vector-a)))

;;----------------------------------------------------------------

(defn distance ((vector-a vec2) (vector-b vec2))
    (single-float 0f0 #.most-positive-single-float)
  "Return the distance between 2 points defined by vectors
   vector-a & vector-b. If comparing distances, use
   c-distance-squared as it desnt require a sqrt and thus is
   faster."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (sqrt (distance-squared vector-a vector-b)))

;;----------------------------------------------------------------

(defn abs ((vector-a vec2)) vec2
  "Return the vec2 containing the abs of the original vec2's components."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:abs (x vector-a)) (cl:abs (y vector-a))))

;;----------------------------------------------------------------

(defn absolute-dot ((vector-a vec2) (vector-b vec2)) single-float
  "Return the absolute dot product of the vector-a and vector-b."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (cl:abs (cl:* (aref vector-a 0) (aref vector-b 0)))
        (cl:abs (cl:* (aref vector-a 1) (aref vector-b 1)))))

;;----------------------------------------------------------------


(defn normalize ((vector-a vec2)) vec2
  "This normalizes the vector, it makes sure a zero length
   vector won't throw an error."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((len (length-squared vector-a)))
    (if (cl:= 0f0 len)
        vector-a
        (*s vector-a (inv-sqrt len)))))

;;----------------------------------------------------------------

(defn perp-dot ((vec-a vec2) (vec-b vec2)) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:- (cl:* (x vec-a) (y vec-b)) (cl:* (y vec-a) (x vec-b))))

;;----------------------------------------------------------------

(defn cross ((vec-a vec2) (vec-b vec2)) single-float
  "Calculates the 2 dimensional cross-product of 2 vectors,
   which results in a single floating point value which is
   2 times the area of the triangle."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:- (cl:* (x vec-a) (y vec-b))
        (cl:* (y vec-a) (x vec-b))))

;;----------------------------------------------------------------

(defn lerp ((vector-a vec2) (vector-b vec2) (ammount single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (%+ (*s vector-a (cl:- 1f0 ammount))
      (*s vector-b ammount)))


(defn stable-lerp ((vector-a vec2) (vector-b vec2) (ammount single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lerp vector-a vector-b ammount))

;;----------------------------------------------------------------

(defn bezier ((a1 vec2) (a2 vec2)
               (b1 vec2) (b2 vec2) (ammount single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lerp (lerp a1 a2 ammount)
        (lerp b1 b2 ammount)
        ammount))

;;----------------------------------------------------------------

(defun spline (x knots)
  (make (rtg-math.maths:spline x (mapcar #'x knots))
        (rtg-math.maths:spline x (mapcar #'y knots))))

;;----------------------------------------------------------------

(defn from-complex ((c complex)) vec2
  (make (coerce (realpart c) 'single-float)
        (coerce (imagpart c) 'single-float)))
