(in-package :rtg-math.vector2)

;;----------------------------------------------------------------

(declaim (inline make)
         (ftype (function (single-float
                           single-float)
                          vec2)
                make))
(defun make (x y)
  "This takes 2 floats and give back a vector2, this is just an
   array but it specifies the array type and populates it.
   For speed reasons it will not accept integers so make sure
   you hand it floats."
  (declare (single-float x y))
  (let (( vec (make-array 2 :element-type `single-float)))
    (setf (aref vec 0) x
          (aref vec 1) y)
    vec))

;;----------------------------------------------------------------

;;[TODO] What is faster (cl:* x x) or (expt x 2) ?
(declaim (inline 0p)
         (ftype (function (vec2)
                          (boolean)) 0p))
(defun 0p (vector-a)
  "Checks if the length of the vector is zero. As this is a
   floating point number it checks to see if the length is
   below a threshold set in the base-maths package"
  (declare (vec2 vector-a))
  (cl:= 0f0 (cl:+ (EXPT (AREF VECTOR-A 0) 2) (EXPT (AREF VECTOR-A 1) 2))))

;;----------------------------------------------------------------

(declaim (inline unitp)
         (ftype (function (vec2)
                          (boolean)) unitp))
(defun unitp (vector-a)
  "Checks if the vector is of unit length. As this is a
   floating point number it checks to see if the length is
   within the range of 1 + or - and threshold set in base-maths"
  (declare (vec2 vector-a))
  (cl:= 0f0 (cl:- 1.0 (cl:+ (EXPT (AREF VECTOR-A 0) 2) (EXPT (AREF VECTOR-A 1) 2)))))
;;----------------------------------------------------------------

(declaim (inline =)
         (ftype (function (vec2
                           vec2)
                          (boolean)) =))
(defun = (vector-a vector-b)
  "Returns either t if the two vectors are equal.
   Otherwise it returns nil."
  (declare (vec2 vector-a vector-b))
  (AND (cl:= (AREF VECTOR-A 0) (AREF VECTOR-B 0))
       (cl:= (AREF VECTOR-A 1) (AREF VECTOR-B 1))))

;;----------------------------------------------------------------

;; Not sure how to optomise this
(defun + (&rest vec2s)
  "takes any number of vectors and add them all together
   returning a new vector2"
  (reduce #'%+ vec2s))

;;----------------------------------------------------------------

(declaim (inline %+)
         (ftype (function (vec2
                           vec2)
                          vec2) %+))
(defun %+ (vector-a vector-b)
  "Add two vectors and return a new vector containing the result"
  (declare (vec2 vector-a vector-b))
  (MAKE (cl:+ (AREF VECTOR-A 0) (AREF VECTOR-B 0))
	(cl:+ (AREF VECTOR-A 1) (AREF VECTOR-B 1))))

;;----------------------------------------------------------------

;; Not sure how to optomise this
(defun - (&rest vec2s)
  "takes any number of vectors and subtract them and return
   a new vector4"
  (reduce #'%- vec2s))

;;----------------------------------------------------------------

(declaim (inline %-)
         (ftype (function (vec2
                           vec2)
                          vec2) %-))
(defun %- (vector-a vector-b)
  "Subtract two vectors and return a new vector containing
   the result"
  (declare (vec2 vector-a vector-b))
  (MAKE (cl:- (AREF VECTOR-A 0) (AREF VECTOR-B 0))
	(cl:- (AREF VECTOR-A 1) (AREF VECTOR-B 1))))

;;----------------------------------------------------------------

(declaim (inline *)
         (ftype (function (vec2
                           single-float)
                          vec2) *))
(defun *s (vector-a a)
  "Multiply vector by scalar"
  (declare (vec2 vector-a)
           (single-float a))
  (MAKE (cl:* (AREF VECTOR-A 0) A) (cl:* (AREF VECTOR-A 1) A)))

;;----------------------------------------------------------------

(declaim (inline *)
         (ftype (function (vec2 vec2) vec2)
                *))
(defun * (vector-a vector-b)
  "Multiplies components, is not dot product, not sure what
   i'll need this for yet but hey!"
  (declare (vec2 vector-a vector-b))
  (MAKE (cl:* (AREF VECTOR-A 0) (AREF VECTOR-B 0))
	(cl:* (AREF VECTOR-A 1) (AREF VECTOR-B 1))))

;;----------------------------------------------------------------

(declaim (inline /s)
         (ftype (function (vec2
                           single-float)
                          vec2) /s))
(defun /s (vector-a a)
  "divide vector by scalar and return result as new vector"
  (declare (vec2 vector-a)
           (single-float a))
  (let ((b (cl:/ 1 a)))
    (make (cl:* (aref vector-a 0) b) (cl:* (aref vector-a 1) b))))

;;----------------------------------------------------------------

(declaim (inline /)
         (ftype (function (vec2
                           vec2)
                          vec2)
                /))
(defun / (vector-a vector-b)
  "Divides components, not sure what, i'll need this for
   yet but hey!"
  (declare (vec2 vector-a vector-b))
  (make (cl:/ (aref vector-a 0)
	      (aref vector-b 0))
	(cl:/ (aref vector-a 1)
	      (aref vector-b 1))))

;;----------------------------------------------------------------

(declaim (inline negate)
         (ftype (function (vec2)
                          vec2)
                negate))
(defun negate (vector-a)
  "Return a vector that is the negative of the vector passed in"
  (declare (vec2 vector-a))
  (MAKE (cl:- (AREF VECTOR-A 0)) (- (AREF VECTOR-A 1))))

;;----------------------------------------------------------------

(declaim (inline face-foreward)
         (ftype (function (vec2
                           vec2)
                          vec2)
                face-foreward))
(defun face-foreward (vector-a vector-b)
  (declare (vec2 vector-a vector-b))
  (if (> (print (dot vector-a vector-b)) 0)
      vector-a
      (negate vector-a)))

;;----------------------------------------------------------------

(declaim (inline length-squared)
         (ftype (function (vec2)
                          single-float) length-squared))
(defun length-squared (vector-a)
  "Return the squared length of the vector. A regular length
   is the square root of this value. The sqrt function is slow
   so if all thats needs doing is to compare lengths then always
   use the length squared function"
  (declare (vec2 vector-a))
  (let ((x (x vector-a))
        (y (y vector-a)))
    (cl:+ (cl:* x x) (cl:* y y))))

;;----------------------------------------------------------------

(declaim (inline length)
         (ftype (function (vec2)
                          single-float) length))
(defun length (vector-a)
  "Returns the length of a vector
   If you only need to compare relative lengths then definately
   stick to length-squared as the sqrt is a slow operation."
  (declare (vec2 vector-a))
  (sqrt (length-squared vector-a)))

;;----------------------------------------------------------------

(declaim (inline distance-squared)
         (ftype (function (vec2
                           vec2)
                          single-float)
                distance-squared))
(defun distance-squared (vector-a vector-b)
  "finds the squared distance between 2 points defined by vectors
   vector-a & vector-b"
  (declare (vec2 vector-a vector-b))
  (length-squared (- vector-b vector-a)))

;;----------------------------------------------------------------

(declaim (inline distance)
         (ftype (function (vec2
                           vec2)
                          single-float)
                distance))
(defun distance (vector-a vector-b)
  "Return the distance between 2 points defined by vectors
   vector-a & vector-b. If comparing distances, use
   c-distance-squared as it desnt require a sqrt and thus is
   faster."
  (declare (vec2 vector-a vector-b))
  (sqrt (distance-squared vector-a vector-b)))

;;----------------------------------------------------------------

(declaim (inline dot)
         (ftype (function (vec2
                           vec2)
                          single-float)
                dot))
(defun dot (vector-a vector-b)
  "Return the dot product of the vector-a and vector-b."
  (declare (vec2 vector-a vector-b))
  (cl:+ (cl:* (AREF VECTOR-A 0) (AREF VECTOR-B 0))
        (cl:* (AREF VECTOR-A 1) (AREF VECTOR-B 1))))

;;----------------------------------------------------------------

(declaim (inline absolute-dot)
         (ftype (function (vec2
                           vec2)
                          single-float)
                absolute-dot))
(defun absolute-dot (vector-a vector-b)
  "Return the absolute dot product of the vector-a and vector-b."
  (declare (vec2 vector-a vector-b))
  (cl:+ (ABS (cl:* (AREF VECTOR-A 0) (AREF VECTOR-B 0)))
        (ABS (cl:* (AREF VECTOR-A 1) (AREF VECTOR-B 1)))))

;;----------------------------------------------------------------

;; [TODO] shouldnt this return a zero vector in event of zero
;; length? does it matter?
(declaim (inline normalize)
         (ftype (function (vec2)
                          vec2)
                normalize))
(defun normalize (vector-a)
  "This normalizes the vector, it makes sure a zero length
   vector won't throw an error."
  (declare (vec2 vector-a))
  (let ((len (length-squared vector-a)))
    (if (cl:= 0f0 len)
        vector-a
        (*s vector-a (inv-sqrt len)))))

;;----------------------------------------------------------------

(declaim (inline perp-dot)
         (ftype (function (vec2
                           vec2)
                          float)
                perp-dot))
(defun perp-dot (vec-a vec-b)
  (declare (vec2 vec-a vec-b))
  (cl:- (cl:* (x vec-a) (y vec-b)) (cl:* (y vec-a) (x vec-b))))

;;----------------------------------------------------------------

(declaim (inline cross)
         (ftype (function (vec3
                           vec3)
                          float)
                cross))
(defun cross (vec-a vec-b)
  "Calculates the 2 dimensional cross-product of 2 vectors,
   which results in a single floating point value which is
   2 times the area of the triangle."
  (declare (vec3 vec-a vec-b))
  (cl:- (cl:* (x vec-a) (y vec-b)) (cl:* (y vec-a) (x vec-b))))

;;----------------------------------------------------------------

(declaim (inline lerp)
         (ftype (function (vec2 vec2 single-float) vec2)
                lerp))
(defun lerp (vector-a vector-b ammount)
  (declare (vec2 vector-a vector-b))
  (%+ vector-a (*s (%- vector-b vector-a) ammount)))

;;----------------------------------------------------------------

(declaim (inline bezier)
         (ftype (function (vec2
                           vec2
                           vec2
                           vec2
                           single-float)
                          vec2)
                bezier))
(defun bezier (a1 a2 b1 b2 ammount)
  (declare (vec2 a1 a2 b1 b2)
           (single-float ammount))
  (lerp (lerp a1 a2 ammount)
        (lerp b1 b2 ammount)
        ammount))

;;----------------------------------------------------------------

(defun spline (x knots)
  (make (rtg-math.maths:spline x (mapcar #'x knots))
	(rtg-math.maths:spline x (mapcar #'y knots))))

;;----------------------------------------------------------------

(defun from-complex (c)
  (make (coerce (realpart c) 'single-float)
	(coerce (imagpart c) 'single-float)))
