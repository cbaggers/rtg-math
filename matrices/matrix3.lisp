(in-package :rtg-math.matrix3)

;; All matrices are stored in column-major format, but these functions
;; expect row major format. The reason is that row-major is easier to
;; read in code and matches with lots of examples in maths text books.


(declaim (inline melm)
	 (ftype (function (mat3 (integer 0 3) (integer 0 3))
			  single-float)
		melm))
(defun melm (mat-a row col)
  "Provides access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (declare (mat3 mat-a)
           (type (integer 0 3) row col))
  (aref mat-a (cl:+ row (cl:* col 3))))

(defun (setf melm) (value mat-a row col)
  "Provides access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (declare (mat3 mat-a)
           (type (integer 0 3) row col)
           (single-float value))
  (setf (aref mat-a (cl:+ row (cl:* col 3))) value))

(define-compiler-macro melm (mat-a row col)
  "Provide access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (cond ((and (numberp row) (numberp col))
         `(aref ,mat-a ,(cl:+ row (cl:* col 3))))
        ((numberp col)
         `(aref ,mat-a (cl:+ ,row ,(cl:* col 3))))
        (t `(aref ,mat-a (cl:+ ,row (cl:* ,col 3))))))

;;----------------------------------------------------------------

(declaim
 (inline identity)
 (ftype (function () mat3)
        identity))
(defun identity ()
  "Return a 3x3 identity matrix"
  (make
   1.0 0.0 0.0
   0.0 1.0 0.0
   0.0 0.0 1.0))

(declaim
 (inline 0!)
 (ftype (function () mat3)
        0!))
(defun 0! ()
  "Return a 3x3 zero matrix"
  (make-array 9 :element-type `single-float :initial-element 0.0))

;;----------------------------------------------------------------

(declaim
 (inline make)
 (ftype (function
         (single-float single-float single-float
                       single-float single-float single-float
                       single-float single-float single-float)
         mat3)
        make))
(defun make (a b c d e f g h i)
  "Make a 3x3 matrix. Data must be provided in row major order"
  (let ((result (0!)))
    (setf (melm result 0 0) a)
    (setf (melm result 0 1) b)
    (setf (melm result 0 2) c)
    (setf (melm result 1 0) d)
    (setf (melm result 1 1) e)
    (setf (melm result 1 2) f)
    (setf (melm result 2 0) g)
    (setf (melm result 2 1) h)
    (setf (melm result 2 2) i)
    result))

;;----------------------------------------------------------------

(declaim
 (inline from-rows)
 (ftype (function (vec3
                   vec3
                   vec3)
                  mat3)
        from-rows))
(defun from-rows (row-1 row-2 row-3)
  "Make a 3x3 matrix using the data in the 3 vector3s provided
   to populate the rows"
  (declare (vec3 row-1 row-2 row-3))
  (make (x row-1) (y row-1) (z row-1)
	(x row-2) (y row-2)	(z row-2)
	(x row-3) (y row-3) (z row-3)))

;;----------------------------------------------------------------

(defun get-rows (mat-a)
  "Return the rows of the matrix a 3 vector3s"
  (declare (mat3 mat-a))
  (list (v3:make (melm mat-a 0 0)
		 (melm mat-a 0 1)
		 (melm mat-a 0 2))
        (v3:make (melm mat-a 1 0)
		 (melm mat-a 1 1)
		 (melm mat-a 1 2))
        (v3:make (melm mat-a 2 0)
		 (melm mat-a 2 1)
		 (melm mat-a 2 2))))

;;----------------------------------------------------------------

(declaim
 (inline get-rows)
 (ftype (function (mat3 (integer 0 3))
                  vec3)
        get-row))
(defun get-row (mat-a row-num)
  "Return the specified row of the matrix a vector3"
  (declare (mat3 mat-a)
           (type (integer 0 3) row-num))
  (v3:make (melm mat-a row-num 0)
	   (melm mat-a row-num 1)
	   (melm mat-a row-num 2)))

;;----------------------------------------------------------------

(declaim
 (inline from-columns)
 (ftype (function (vec3
                   vec3
                   vec3)
                  mat3)
        from-columns))
(defun from-columns (col-1 col-2 col-3)
  "Make a 3x3 matrix using the data in the 3 vector3s provided
   to populate the columns"
  (declare (vec3 col-1 col-2 col-3))
  (make (x col-1) (x col-2) (x col-3)
	(y col-1) (y col-2) (y col-3)
	(z col-1) (z col-2) (z col-3)))

;;----------------------------------------------------------------


(defun get-columns (mat-a)
  "Return the columns of the matrix as 3 vector3s"
  (declare (mat3 mat-a))
  (list (v! (melm mat-a 0 0)
	    (melm mat-a 1 0)
	    (melm mat-a 2 0))
        (v! (melm mat-a 0 1)
	    (melm mat-a 1 1)
	    (melm mat-a 2 1))
        (v! (melm mat-a 0 2)
	    (melm mat-a 1 2)
	    (melm mat-a 2 2))))

;;----------------------------------------------------------------

(declaim
 (inline get-column)
 (ftype (function (mat3 (integer 0 3))
                  vec3)
        get-column))
(defun get-column (mat-a col-num)
  "Return the specified column of the matrix a vector3"
  (declare (mat3 mat-a)
           (type (integer 0 3) col-num))
  (v! (melm mat-a 0 col-num)
      (melm mat-a 1 col-num)
      (melm mat-a 2 col-num)))

;;----------------------------------------------------------------

(declaim
 (inline 0p)
 (ftype (function (mat3)
                  boolean)
        0p))
(defun 0p (mat-a)
  "Returns 't' if this is a zero matrix (as contents of the
   matrix are floats the values have an error bound as defined
   in base-maths"
  (declare (mat3 mat-a))
  (loop :for i :below 9 :always (not (cl:= 0f0 (aref mat-a i)))))

;;----------------------------------------------------------------

;;[TODO] should the checks for '1.0' also have the error bounds?
(declaim
 (inline identityp)
 (ftype (function (mat3)
                  boolean)
        identityp))
(defun identityp (mat-a)
  "Returns 't' if this is an identity matrix (as contents of the
   matrix are floats the values have an error bound as defined
   in base-maths"
  (declare (mat3 mat-a))
  (and (cl:= 0f0 (cl:- (melm mat-a 0 0) 1.0))
       (cl:= 0f0 (cl:- (melm mat-a 1 1) 1.0))
       (cl:= 0f0 (cl:- (melm mat-a 2 2) 1.0))
       (cl:= 0f0 (melm mat-a 0 1))
       (cl:= 0f0 (melm mat-a 0 2))
       (cl:= 0f0 (melm mat-a 1 0))
       (cl:= 0f0 (melm mat-a 1 2))
       (cl:= 0f0 (melm mat-a 2 0))
       (cl:= 0f0 (melm mat-a 2 1))))

;;----------------------------------------------------------------

(declaim
 (inline =)
 (ftype (function (mat3 mat3) boolean)
        =))
(defun = (mat-a mat-b)
  "Returns t if all elements of both matrices provided are
   equal"
  (declare (mat3 mat-a mat-b))
  (loop :for i :below 9 :always (cl:= (aref mat-a i) (aref mat-b i))))

;;----------------------------------------------------------------

(defun determinate (mat-a)
  "Returns the determinate of the matrix (uses the cramer method)"
  (declare (mat3 mat-a))
  (let* ((cofactor-0 (cl:- (cl:* (melm mat-a 1 1) (melm mat-a 2 2))
			   (cl:* (melm mat-a 2 1) (melm mat-a 1 2))))

	 (cofactor-3 (cl:- (cl:* (melm mat-a 2 0) (melm mat-a 1 2))
			   (cl:* (melm mat-a 1 0) (melm mat-a 2 2))))

	 (cofactor-6 (cl:- (cl:* (melm mat-a 1 0) (melm mat-a 2 1))
			   (cl:* (melm mat-a 2 0) (melm mat-a 1 1)))))
    (cl:+ (cl:* (melm mat-a 0 0) cofactor-0)
	  (cl:* (melm mat-a 0 1) cofactor-3)
	  (cl:* (melm mat-a 0 2) cofactor-6))))

;;----------------------------------------------------------------

(declaim
 (inline affine-inverse)
 (ftype (function (mat3)
                  mat3)
        affine-inverse))
(defun affine-inverse (mat-a)
  "returns the inverse of the matrix"
  (declare (mat3 mat-a))
  (let* ((cofactor-0 (cl:- (cl:* (melm mat-a 1 1) (melm mat-a 2 2))
			   (cl:* (melm mat-a 2 1) (melm mat-a 1 2))))

         (cofactor-3 (cl:- (cl:* (melm mat-a 2 0) (melm mat-a 1 2))
			   (cl:* (melm mat-a 1 0) (melm mat-a 2 2))))

         (cofactor-6 (cl:- (cl:* (melm mat-a 1 0) (melm mat-a 2 1))
			   (cl:* (melm mat-a 2 0) (melm mat-a 1 1))))
         (det (cl:+ (cl:* (melm mat-a 0 0) cofactor-0)
		    (cl:* (melm mat-a 0 1) cofactor-3)
		    (cl:* (melm mat-a 0 2) cofactor-6))))
    (if (cl:= 0f0 det)
	(error "Matrix4 Inverse: Singular Matrix")
	(let*
	    ((inv-det (cl:/ 1.0 det))
	     (r00 (cl:* inv-det cofactor-0))
	     (r10 (cl:* inv-det cofactor-3))
	     (r20 (cl:* inv-det cofactor-6))
	     (r01 (cl:* inv-det (cl:- (cl:* (melm mat-a 2 1) (melm mat-a 0 2))
				      (cl:* (melm mat-a 0 1) (melm mat-a 2 2)))))
	     (r11 (cl:* inv-det (cl:- (cl:* (melm mat-a 0 0) (melm mat-a 2 2))
				      (cl:* (melm mat-a 2 0) (melm mat-a 0 2)))))
	     (r21 (cl:* inv-det (cl:- (cl:* (melm mat-a 2 0) (melm mat-a 0 1))
				      (cl:* (melm mat-a 0 0) (melm mat-a 2 1)))))
	     (r02 (cl:* inv-det (cl:- (cl:* (melm mat-a 0 1) (melm mat-a 1 2))
				      (cl:* (melm mat-a 1 1) (melm mat-a 0 2)))))
	     (r12 (cl:* inv-det (cl:- (cl:* (melm mat-a 1 0) (melm mat-a 0 2))
				      (cl:* (melm mat-a 0 0) (melm mat-a 1 2)))))
	     (r22 (cl:* inv-det (cl:- (cl:* (melm mat-a 0 0) (melm mat-a 1 1))
				      (cl:* (melm mat-a 1 0) (melm mat-a 0 1))))))
	  (make r00 r01 r02
		r10 r11 r12
		r20 r21 r22)))))

;;----------------------------------------------------------------

(declaim
 (inline transpose)
 (ftype (function (mat3)
                  mat3)
        transpose))
(defun transpose (mat-a)
  "Returns the transpose of the provided matrix"
  (declare (mat3))
  (make
   (melm mat-a 0 0) (melm mat-a 1 0) (melm mat-a 2 0)
   (melm mat-a 0 1) (melm mat-a 1 1) (melm mat-a 2 1)
   (melm mat-a 0 2) (melm mat-a 1 2) (melm mat-a 2 2)))

;;----------------------------------------------------------------

;;This is taken straight from 'Essential Mathematics for Game..'
(declaim
 (inline adjoint)
 (ftype (function (mat3)
                  mat3)
        adjoint))
(defun adjoint (mat-a)
  "Returns the adjoint of the matrix"
  (make  (cl:- (cl:* (melm mat-a 1 1) (melm mat-a 2 2))
	       (cl:* (melm mat-a 1 2) (melm mat-a 2 1)))
	 (cl:- (cl:* (melm mat-a 0 2) (melm mat-a 2 1))
	       (cl:* (melm mat-a 0 1) (melm mat-a 2 2)))
	 (cl:- (cl:* (melm mat-a 0 1) (melm mat-a 1 2))
	       (cl:* (melm mat-a 0 2) (melm mat-a 1 1)))
	 (cl:- (cl:* (melm mat-a 1 2) (melm mat-a 2 0))
	       (cl:* (melm mat-a 1 0) (melm mat-a 2 2)))
	 (cl:- (cl:* (melm mat-a 0 0) (melm mat-a 2 2))
	       (cl:* (melm mat-a 0 2) (melm mat-a 2 0)))
	 (cl:- (cl:* (melm mat-a 0 2) (melm mat-a 1 0))
	       (cl:* (melm mat-a 0 0) (melm mat-a 1 2)))
	 (cl:- (cl:* (melm mat-a 1 0) (melm mat-a 2 1))
	       (cl:* (melm mat-a 1 1) (melm mat-a 2 0)))
	 (cl:- (cl:* (melm mat-a 0 1) (melm mat-a 2 0))
	       (cl:* (melm mat-a 0 0) (melm mat-a 2 1)))
	 (cl:- (cl:* (melm mat-a 0 0) (melm mat-a 1 1))
	       (cl:* (melm mat-a 0 1) (melm mat-a 1 0)))))

;;----------------------------------------------------------------


(defun trace (mat-a)
  "Returns the trace of the matrix (That is the diagonal values)"
  (declare (mat3 mat-a))
  (cl:+ (melm mat-a 0 0) (melm mat-a 1 1) (melm mat-a 2 2)))

;;----------------------------------------------------------------

;;Rotation goes here, requires quaternion

;;----------------------------------------------------------------

(declaim
 (inline rotation-from-euler)
 (ftype (function (vec3)
                  mat3)
        rotation-from-euler))
(defun rotation-from-euler (vec3-a)
  (declare (vec3 vec3-a))
  (let ((x (x vec3-a)) (y (y vec3-a)) (z (z vec3-a)))
    (let ((sx (sin x)) (cx (cos x))
          (sy (sin y)) (cy (cos y))
          (sz (sin z)) (cz (cos z)))
      (make (cl:* cy cz)
	    (cl:- (cl:* cy sz))
	    sy

	    (cl:+ (cl:* sx sy cz) (cl:* cx sz))
	    (cl:- (cl:* cx cz) (cl:* sx sy sz))
	    (cl:- (cl:* sx cy))

	    (cl:- (cl:* sx sz) (cl:* cx sy cz))
	    (cl:+ (cl:* cx sy sz) (cl:* sx cz))
	    (cl:* cx cy)))))

;;----------------------------------------------------------------

(declaim
 (inline rotation-from-axis-angle)
 (ftype (function (vec3 single-float)
                  mat3)
        rotation-from-axis-angle))
(defun rotation-from-axis-angle (axis3 angle)
  "Returns a matrix which will rotate a point about the axis
   specified by the angle provided"
  (cond ((v3:= axis3 (v! 1f0 0f0 0f0)) (rotation-x angle))
        ((v3:= axis3 (v! 0f0 1f0 0f0)) (rotation-y angle))
        ((v3:= axis3 (v! 0f0 0f0 1f0)) (rotation-z angle))
        (t
         (let ((c (cos angle))
               (s (sin angle))
               (g (cl:- 1f0 (cos angle))))
           (let* ((x (x axis3))
                  (y (y axis3))
                  (z (z axis3))
                  (gxx (cl:* g x x)) (gxy (cl:* g x y)) (gxz (cl:* g x z))
                  (gyy (cl:* g y y)) (gyz (cl:* g y z)) (gzz (cl:* g z z)))
             (make
              (cl:+ gxx c)        (cl:- gxy (cl:* s z))  (cl:+ gxz (cl:* s y))
              (cl:+ gxy (cl:* s z))  (cl:+ gyy c)        (cl:- gyz (cl:* s x))
              (cl:- gxz (cl:* s y))  (cl:+ gyz (cl:* s x))  (cl:+ gzz c)))))))

;;----------------------------------------------------------------

(declaim
 (inline scale)
 (ftype (function (vec3)
                  mat3)
        scale))
(defun scale (scale-vec3)
  "Returns a matrix which will scale by the amounts specified"
  (declare (vec3 scale-vec3))
  (make (x scale-vec3)  0.0               0.0
	0.0               (y scale-vec3)  0.0
	0.0               0.0               (z scale-vec3)))

;;----------------------------------------------------------------

(declaim
 (inline rotation-x)
 (ftype (function (single-float)
                  mat3)
        rotation-x))
(defun rotation-x (angle)
  "Returns a matrix which would rotate a point around the x axis
   by the specified amount"
  (declare (single-float angle))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make 1.0  0.0  0.0
	  0.0  c-a  (cl:- s-a)
	  0.0  s-a  c-a)))

;;----------------------------------------------------------------

(declaim
 (inline rotation-y)
 (ftype (function (single-float)
                  mat3)
        rotation-y))
(defun rotation-y (angle)
  "Returns a matrix which would rotate a point around the y axis
   by the specified amount"
  (declare (single-float angle))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make c-a      0.0    s-a
	  0.0      1.0    0.0
	  (cl:- s-a)  0.0    c-a)))

;;----------------------------------------------------------------

(declaim
 (inline rotation-z)
 (ftype (function (single-float)
                  mat3)
        rotation-z))
(defun rotation-z (angle)
  "Returns a matrix which would rotate a point around the z axis
   by the specified amount"
  (declare (single-float angle))
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make c-a  (cl:- s-a)  0.0
	  s-a  c-a      0.0
	  0.0  0.0      1.0)))

;;----------------------------------------------------------------

(declaim
 (inline get-fixed-angles)
 (ftype (function (mat3) vec3)
        get-fixed-angles))
(defun get-fixed-angles (mat-a)
  "Gets one set of possible z-y-x fixed angles that will generate
   this matrix. Assumes that this is a rotation matrix"
  (declare (mat3 mat-a))
  (let* ((sy (melm mat-a 0 2))
         (cy (sqrt (cl:- 1.0 (cl:* sy sy)))))
    (if (not (cl:= 0f0 cy)) ; [TODO: not correct PI-epsilon]
        (let* ((factor (cl:/ 1.0 cy))
               (sx (cl:* factor (cl:- (melm mat-a 2 1))))
               (cx (cl:* factor (melm mat-a 2 2)))
               (sz (cl:* factor (cl:- (melm mat-a 1 0))))
               (cz (cl:* factor (melm mat-a 0 0))))
          (v! (atan sx cx) (atan sy cy) (atan sz cz)))
        (let* ((sz 0.0)
               (cx 1.0)
               (sx (melm mat-a 1 2))
               (cz (melm mat-a 1 1)))
          (v! (atan sx cx) (atan sy cy) (atan sz cz))))))

;;----------------------------------------------------------------

;; [TODO] find out how we can declaim angle to be float
;; [TODO] Comment the fuck out of this and work out how it works
(declaim
 (inline get-axis-angle)
 (ftype (function (mat3)
                  vec3)
        get-axis-angle))
(defun get-axis-angle (mat-a)
  "Gets one possible axis-angle pair that will generate this
   matrix. Assumes that this is a rotation matrix"
  (declare (mat3 mat-a))
  (let* ((c-a (cl:* 0.5 (cl:- (trace mat-a) 1.0)))
         (angle (acos c-a)))
    (cond ((cl:= 0f0 angle) ;; <-angle is zero so axis can be anything
           (v! 1.0 0.0 0.0))
          ((< angle rtg-math.base-maths:+pi+)
                                        ;its not 180 degrees
           (let ((axis (v! (cl:- (melm mat-a 1 2) (melm mat-a 2 1))
			   (cl:- (melm mat-a 2 0) (melm mat-a 0 2))
			   (cl:- (melm mat-a 0 1) (melm mat-a 1 0)))))
             (v3:normalize axis)))
          (t (let* ((i (if (> (melm mat-a 1 1) (melm mat-a 0 0))
                           1
                           (if (> (melm mat-a 2 2)
                                  (melm mat-a 0 0))
                               2
                               0)))
                    (j (mod (cl:+ i 1) 3))
                    (k (mod (cl:+ j 1) 3))
                    (s (sqrt (cl:+ 1.0 (cl:- (melm mat-a i i)
					     (melm mat-a j j)
					     (melm mat-a k k)))))
                    (recip (cl:/ 1.0 s))
                    (result (v! 0.0 0.0 0.0)))
               (setf (aref result i) (cl:* 0.5 s))
               (setf (aref result j) (cl:* recip (melm mat-a i j)))
               (setf (aref result j) (cl:* recip (melm mat-a k i)))
               result)))))

;;----------------------------------------------------------------

(declaim
 (inline +)
 (ftype (function (mat3
                   mat3)
                  mat3)
        +))
(defun + (mat-a mat-b)
  "Adds the 2 matrices component wise and returns the result as
   a new matrix"
  (declare (mat3 mat-a mat-b))
  (let ((r (0!)))
    (declare (mat3 r))
    (loop :for i :below 9 :do
       (setf (aref r i) (cl:+ (aref mat-a i) (aref mat-b i))))
    r))

;;----------------------------------------------------------------

(declaim
 (inline -)
 (ftype (function (mat3 mat3) mat3)
        -))
(defun - (mat-a mat-b)
  "Subtracts the 2 matrices component wise and returns the result
   as a new matrix"
  (declare (mat3 mat-a mat-b))
  (let ((r (0!)))
    (declare (mat3 r))
    (loop :for i :below 9 :do
       (setf (aref r i) (cl:- (aref mat-a i) (aref mat-b i))))
    r))

;;----------------------------------------------------------------

(declaim
 (inline negate)
 (ftype (function (mat3)
                  mat3)
        negate))
(defun negate (mat-a)
  "Negates the components of the matrix"
  (let ((result (0!)))
    (loop :for i :below 9 :do
       (setf (aref result i) (cl:- (aref mat-a i))))
    result))

;;----------------------------------------------------------------


(defun *v (mat-a vec-a)
  "Multiplies the vector3 by the matrix and returns the result
   as a new vector3"
  (v! (cl:+ (cl:* (x vec-a) (melm mat-a 0 0))
	    (cl:* (y vec-a) (melm mat-a 0 1))
	    (cl:* (z vec-a) (melm mat-a 0 2)))
      (cl:+ (cl:* (x vec-a) (melm mat-a 1 0))
	    (cl:* (y vec-a) (melm mat-a 1 1))
	    (cl:* (z vec-a) (melm mat-a 1 2)))
      (cl:+ (cl:* (x vec-a) (melm mat-a 2 0))
	    (cl:* (y vec-a) (melm mat-a 2 1))
	    (cl:* (z vec-a) (melm mat-a 2 2)))))

;;----------------------------------------------------------------

(defun mrow*vec3 (vec mat-a)
  (v! (cl:+ (cl:* (x vec) (melm mat-a 0 0)) (cl:* (y vec) (melm mat-a 1 0))
	    (cl:* (z vec) (melm mat-a 2 0)))

      (cl:+ (cl:* (x vec) (melm mat-a 0 1)) (cl:* (y vec) (melm mat-a 1 1))
	    (cl:* (z vec) (melm mat-a 2 1)))

      (cl:+ (cl:* (x vec) (melm mat-a 0 2)) (cl:* (y vec) (melm mat-a 1 2))
	    (cl:* (z vec) (melm mat-a 2 2)))))

;;----------------------------------------------------------------
(defun * (mat-a mat-b)
  "Multiplies 2 matrices and returns the result as a new
   matrix"
  (make (cl:+ (cl:* (melm mat-a 0 0) (melm mat-b 0 0))
	      (cl:* (melm mat-a 0 1) (melm mat-b 1 0))
	      (cl:* (melm mat-a 0 2) (melm mat-b 2 0)))
	(cl:+ (cl:* (melm mat-a 1 0) (melm mat-b 0 0))
	      (cl:* (melm mat-a 1 1) (melm mat-b 1 0))
	      (cl:* (melm mat-a 1 2) (melm mat-b 2 0)))
	(cl:+ (cl:* (melm mat-a 2 0) (melm mat-b 0 0))
	      (cl:* (melm mat-a 2 1) (melm mat-b 1 0))
	      (cl:* (melm mat-a 2 2) (melm mat-b 2 0)))
	(cl:+ (cl:* (melm mat-a 0 0) (melm mat-b 0 1))
	      (cl:* (melm mat-a 0 1) (melm mat-b 1 1))
	      (cl:* (melm mat-a 0 2) (melm mat-b 2 1)))
	(cl:+ (cl:* (melm mat-a 1 0) (melm mat-b 0 1))
	      (cl:* (melm mat-a 1 1) (melm mat-b 1 1))
	      (cl:* (melm mat-a 1 2) (melm mat-b 2 1)))
	(cl:+ (cl:* (melm mat-a 2 0) (melm mat-b 0 1))
	      (cl:* (melm mat-a 2 1) (melm mat-b 1 1))
	      (cl:* (melm mat-a 2 2) (melm mat-b 2 1)))
	(cl:+ (cl:* (melm mat-a 0 0) (melm mat-b 0 2))
	      (cl:* (melm mat-a 0 1) (melm mat-b 1 2))
	      (cl:* (melm mat-a 0 2) (melm mat-b 2 2)))
	(cl:+ (cl:* (melm mat-a 0 1) (melm mat-b 0 2))
	      (cl:* (melm mat-a 1 1) (melm mat-b 1 2))
	      (cl:* (melm mat-a 1 2) (melm mat-b 2 2)))
	(cl:+ (cl:* (melm mat-a 2 0) (melm mat-b 0 2))
	      (cl:* (melm mat-a 2 1) (melm mat-b 1 2))
	      (cl:* (melm mat-a 2 2) (melm mat-b 2 2)))))

;;----------------------------------------------------------------

(defun *s (mat-a scalar)
  "Multiplies the components of the matrix by the scalar
   provided"
  (let ((result (0!)))
    (loop :for i :below 9 :do (setf (aref result i) (cl:* scalar (aref mat-a i))))
    result))
