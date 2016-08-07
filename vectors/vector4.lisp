(in-package #:rtg-math.vector4)

;;----------------------------------------------------------------

(declaim (inline make)
         (ftype (function (single-float single-float single-float single-float)
                          vec4)
                make))
(defun make (x y z w)
  "This takes 4 floats and give back a vector4, this is just an
   array but it specifies the array type and populates it.
   For speed reasons it will not accept integers so make sure
   you hand it floats."
  (declare (single-float x y z w))
  (let ((vec (make-array 4 :element-type `single-float)))
    (setf (aref vec 0) x
          (aref vec 1) y
          (aref vec 2) z
          (aref vec 3) w)
    vec))

;;----------------------------------------------------------------

;;[TODO] What is faster (cl:* x x) or (expt x 2) ?
(declaim (inline 0p)
         (ftype (function (vec4)
                          (boolean)) 0p))
(defun 0p (vector-a)
  "Checks if the length of the vector is zero. As this is a
   floating point number it checks to see if the length is
   below a threshold set in the base-maths package"
  (declare (vec4 vector-a))
  (cl:= 0f0 (cl:+ (EXPT (AREF VECTOR-A 0) 2) (EXPT (AREF VECTOR-A 1) 2)
		  (EXPT (AREF VECTOR-A 2) 2) (EXPT (AREF VECTOR-A 3) 2))))

;;----------------------------------------------------------------

(declaim (inline unitp)
         (ftype (function (vec4)
                          (boolean)) unitp))
(defun unitp (vector-a)
  "Checks if the vector is of unit length. As this is a
   floating point number it checks to see if the length is
   within the range of 1 + or - and threshold set in base-maths"
  (declare (vec4 vector-a))
  (cl:= 0f0 (cl:- 1.0 (cl:+ (EXPT (AREF VECTOR-A 0) 2) (EXPT (AREF VECTOR-A 1) 2)
			    (EXPT (AREF VECTOR-A 2) 2) (EXPT (AREF VECTOR-A 3) 2)))))
;;----------------------------------------------------------------

(declaim (inline =)
         (ftype (function (vec4
                           vec4)
                          (boolean)) =))
(defun = (vector-a vector-b)
  "Returns either t if the two vectors are equal.
   Otherwise it returns nil."
  (declare (vec4 vector-a vector-b))
  (AND (cl:= (AREF VECTOR-A 0) (AREF VECTOR-B 0))
       (cl:= (AREF VECTOR-A 1) (AREF VECTOR-B 1))
       (cl:= (AREF VECTOR-A 2) (AREF VECTOR-B 2))
       (cl:= (AREF VECTOR-A 3) (AREF VECTOR-B 3))))

;;----------------------------------------------------------------

(defun + (&rest vec4s)
  "takes any number of vectors and add them all together
   returning a new vector"
  (reduce #'%+ vec4s))

;;----------------------------------------------------------------

(declaim (inline %+)
         (ftype (function (vec4
                           vec4)
                          vec4) %+))
(defun %+ (vector-a vector-b)
  "Add two vectors and return a new vector containing the result"
  (declare (vec4 vector-a vector-b))
  (MAKE (cl:+ (AREF VECTOR-A 0) (AREF VECTOR-B 0))
	(cl:+ (AREF VECTOR-A 1) (AREF VECTOR-B 1))
	(cl:+ (AREF VECTOR-A 2) (AREF VECTOR-B 2))
	(cl:+ (AREF VECTOR-A 3) (AREF VECTOR-B 3))))

;;----------------------------------------------------------------

(defun - (&rest vec4s)
  "takes any number of vectors and subtract them and return
   a new vector4"
  (reduce #'%- vec4s))

;;----------------------------------------------------------------

(declaim (inline %-)
         (ftype (function (vec4
                           vec4)
                          vec4) %-))
(defun %- (vector-a vector-b)
  "Subtract two vectors and return a new vector containing
   the result"
  (declare (vec4 vector-a vector-b))
  (MAKE (cl:- (AREF VECTOR-A 0) (AREF VECTOR-B 0))
	(cl:- (AREF VECTOR-A 1) (AREF VECTOR-B 1))
	(cl:- (AREF VECTOR-A 2) (AREF VECTOR-B 2))
	(cl:- (AREF VECTOR-A 3) (AREF VECTOR-B 3))))

;;----------------------------------------------------------------

(declaim (inline *s)
         (ftype (function (vec4 single-float) vec4)
		*s))
(defun *s (vector-a a)
  "Multiply vector by scalar"
  (declare (vec4 vector-a)
           (single-float a))
  (MAKE (cl:* (AREF VECTOR-A 0) A) (cl:* (AREF VECTOR-A 1) A)
	(cl:* (AREF VECTOR-A 2) A) (cl:* (AREF VECTOR-A 3) A)))

;;----------------------------------------------------------------

(declaim (inline *v3)
         (ftype (function (vec4
                           single-float)
                          vec4) *v3))
(defun *v3 (vector-a a)
  "Multiply vector by scalar"
  (declare (vec4 vector-a)
           (single-float a))
  (make (cl:* (aref vector-a 0) a) (cl:* (aref vector-a 1) a)
	(cl:* (aref vector-a 2) a) (aref vector-a 3)))

;;----------------------------------------------------------------

(declaim (inline *)
         (ftype (function (vec4 vec4) vec4)
                *))
(defun * (vector-a vector-b)
  "Multiplies components, is not dot product, not sure what
   i'll need this for yet but hey!"
  (declare (vec4 vector-a vector-b))
  (MAKE (cl:* (AREF VECTOR-A 0) (AREF VECTOR-B 0))
	(cl:* (AREF VECTOR-A 1) (AREF VECTOR-B 1))
	(cl:* (AREF VECTOR-A 2) (AREF VECTOR-B 2))
	(cl:* (AREF VECTOR-A 3) (AREF VECTOR-B 3))))

;;----------------------------------------------------------------

(declaim (inline /s)
         (ftype (function (vec4 single-float) vec4)
		/s))
(defun /s (vector-a a)
  "divide vector by scalar and return result as new vector"
  (declare (vec4 vector-a)
           (single-float a))
  (let ((b (cl:/ 1 a)))
    (MAKE (cl:* (AREF VECTOR-A 0) B) (cl:* (AREF VECTOR-A 1) B)
	  (cl:* (AREF VECTOR-A 2) B) (cl:* (AREF VECTOR-A 3) B))))

;;----------------------------------------------------------------

(declaim (inline /)
         (ftype (function (vec4 vec4) vec4)
                /))
(defun / (vector-a vector-b)
  "Divides components, not sure what, i'll need this for
   yet but hey!"
  (declare (vec4 vector-a vector-b))
  (make (cl:/ (aref vector-a 0) (aref vector-b 0))
	(cl:/ (aref vector-a 1) (aref vector-b 1))
	(cl:/ (aref vector-a 2) (aref vector-b 2))
	(cl:/ (aref vector-a 3) (aref vector-b 3))))

;;----------------------------------------------------------------

(declaim (inline negate)
         (ftype (function (vec4)
                          vec4)
                negate))
(defun negate (vector-a)
  "Return a vector that is the negative of the vector passed in"
  (declare (vec4 vector-a))
  (MAKE (cl:- (AREF VECTOR-A 0)) (cl:- (AREF VECTOR-A 1)) (cl:- (AREF VECTOR-A 2))
	(cl:- (AREF VECTOR-A 3))))

;;----------------------------------------------------------------

(declaim (inline dot)
         (ftype (function (vec4
                           vec4)
                          single-float)
                dot))
(defun dot (vector-a vector-b)
  "return the dot product of the vector-a and vector-b."
  (declare (vec4 vector-a vector-b))
  (cl:+ (cl:* (aref vector-a 0) (aref vector-b 0))
        (cl:* (aref vector-a 1) (aref vector-b 1))
        (cl:* (aref vector-a 2) (aref vector-b 2))
        (cl:* (aref vector-a 3) (aref vector-b 3))))

;;----------------------------------------------------------------

(declaim (inline face-foreward)
         (ftype (function (vec4
                           vec4)
                          vec4)
                face-foreward))
(defun face-foreward (vector-a vector-b)
  (declare (vec4 vector-a vector-b))
  (if (> (print (dot vector-a vector-b)) 0)
      vector-a
      (negate vector-a)))

;;----------------------------------------------------------------

(declaim (inline length-squared)
         (ftype (function (vec4)
                          (single-float)) length-squared))
(defun length-squared (vector-a)
  "Return the squared length of the vector. A regular length
   is the square root of this value. The sqrt function is slow
   so if all thats needs doing is to compare lengths then always
   use the length squared function"
  (declare (vec4 vector-a))
  (let ((x (x vector-a))
        (y (y vector-a))
        (z (z vector-a))
        (w (w vector-a)))
    (cl:+ (cl:* x x) (cl:* y y) (cl:* z z) (cl:* w w))))

;;----------------------------------------------------------------

(declaim (inline length)
         (ftype (function (vec4)
                          single-float) length))
(defun length (vector-a)
  "Returns the length of a vector
   If you only need to compare relative lengths then definately
   stick to length-squared as the sqrt is a slow operation."
  (declare (vec4 vector-a))
  (sqrt (length-squared vector-a)))

;;----------------------------------------------------------------

(declaim (inline distance-squared)
         (ftype (function (vec4
                           vec4)
                          single-float)
                distance-squared))
(defun distance-squared (vector-a vector-b)
  "finds the squared distance between 2 points defined by vectors
   vector-a & vector-b"
  (declare (vec4 vector-a vector-b))
  (length-squared (- vector-b vector-a)))

;;----------------------------------------------------------------

(declaim (inline distance)
         (ftype (function (vec4
                           vec4)
                          single-float)
                distance))
(defun distance (vector-a vector-b)
  "Return the distance between 2 points defined by vectors
   vector-a & vector-b. If comparing distances, use
   c-distance-squared as it desnt require a sqrt and thus is
   faster."
  (declare (vec4 vector-a vector-b))
  (sqrt (distance-squared vector-a vector-b)))


;;----------------------------------------------------------------

(declaim (inline absolute-dot)
         (ftype (function (vec4
                           vec4)
                          single-float)
                absolute-dot))
(defun absolute-dot (vector-a vector-b)
  "Return the absolute dot product of the vector-a and vector-b."
  (declare (vec4 vector-a vector-b))
  (cl:+ (ABS (cl:* (AREF VECTOR-A 0) (AREF VECTOR-B 0)))
        (ABS (cl:* (AREF VECTOR-A 1) (AREF VECTOR-B 1)))
        (ABS (cl:* (AREF VECTOR-A 2) (AREF VECTOR-B 2)))
        (ABS (cl:* (AREF VECTOR-A 3) (AREF VECTOR-B 3)))))

;;----------------------------------------------------------------

;; [TODO] shouldnt this return a zero vector in event of zero
;; length? does it matter?
(declaim (inline normalize)
         (ftype (function (vec4)
                          vec4)
                normalize))
(defun normalize (vector-a)
  "This normalizes the vector, it makes sure a zero length
   vector won't throw an error."
  (declare (vec4 vector-a))
  (let ((len (length-squared vector-a)))
    (if (cl:= 0f0 len)
        vector-a
        (*s vector-a (inv-sqrt len)))))

;;----------------------------------------------------------------

(declaim (inline lerp)
         (ftype (function (vec4
                           vec4
                           single-float)
                          vec4)
                lerp))
(defun lerp (vector-a vector-b ammount)
  (declare (vec4 vector-a vector-b))
  (%+ vector-a (*s (%- vector-b vector-a) ammount)))

;;----------------------------------------------------------------

(declaim (inline bezier)
         (ftype (function (vec4
                           vec4
                           vec4
                           vec4
                           single-float)
                          vec4)
                bezier))
(defun bezier (a1 a2 b1 b2 ammount)
  (declare (vec4 a1 a2 b1 b2)
           (single-float ammount))
  (lerp (lerp a1 a2 ammount)
        (lerp b1 b2 ammount)
        ammount))

;;----------------------------------------------------------------

(defun spline (x knots)
  (make (rtg-math.maths:spline x (mapcar #'x knots))
	(rtg-math.maths:spline x (mapcar #'y knots))
	(rtg-math.maths:spline x (mapcar #'z knots))
	(rtg-math.maths:spline x (mapcar #'w knots))))
