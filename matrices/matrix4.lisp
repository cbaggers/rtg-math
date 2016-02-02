(in-package :rtg-math.matrix4)

;;----------------------------------------------------------------

(declaim (inline melm)
	 (ftype (function (mat4 (integer 0 4) (integer 0 4))
			  single-float)
		melm))
(defun melm (mat-a row col)
  "Provides access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (declare (mat4 mat-a)
           ((integer 0 4) row col))
  (svref mat-a (+ row (* col 4))))

(defun (setf melm) (value mat-a row col)
  "Provides access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (declare (mat4 mat-a)
           ((integer 0 4) row col)
           (single-float value))
  (setf (svref mat-a (+ row (* col 4))) value))

(define-compiler-macro melm (mat-a row col)
  "Provide access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (cond ((and (numberp row) (numberp col))
         `(svref ,mat-a ,(+ row (* col 4))))
        ((numberp col)
         `(svref ,mat-a (+ ,row ,(* col 4))))
        (t `(svref ,mat-a (+ ,row (* ,col 4))))))

;;----------------------------------------------------------------

(declaim (inline identity)
	 (ftype (function ()
			  mat4)
		identity))
(defun identity ()
  "Return a 4x4 identity matrix"
  (make-array 16 :element-type 'single-float
              :initial-contents '(1.0 0.0 0.0 0.0
                                  0.0 1.0 0.0 0.0
                                  0.0 0.0 1.0 0.0
                                  0.0 0.0 0.0 1.0)))

(defun zero-matrix4 ()
  "Return a 4x4 zero matrix"
  (make-array 16 :element-type 'single-float
              :initial-element 0.0))

;;----------------------------------------------------------------

(declaim
 (inline make)
 (ftype (function
         (single-float single-float single-float single-float
                       single-float single-float single-float single-float
                       single-float single-float single-float single-float
                       single-float single-float single-float single-float)
         mat4)
        make))
(defun make (a b c d e f g h i j k l m n o p)
  "Make a 4x4 matrix. Data must be provided in row major order"
  (declare (single-float a b c d e f g h i j k l m n o p))
  ;; as you can see it is stored in column major order
  (let ((result (zero-matrix4)))
    (setf (melm result 0 0) a)
    (setf (melm result 0 1) b)
    (setf (melm result 0 2) c)
    (setf (melm result 0 3) d)
    (setf (melm result 1 0) e)
    (setf (melm result 1 1) f)
    (setf (melm result 1 2) g)
    (setf (melm result 1 3) h)
    (setf (melm result 2 0) i)
    (setf (melm result 2 1) j)
    (setf (melm result 2 2) k)
    (setf (melm result 2 3) l)
    (setf (melm result 3 0) m)
    (setf (melm result 3 1) n)
    (setf (melm result 3 2) o)
    (setf (melm result 3 3) p)
    result))

;;----------------------------------------------------------------

(declaim
 (inline !)
 (ftype (function
         (single-float single-float single-float single-float
                       single-float single-float single-float single-float
                       single-float single-float single-float single-float
                       single-float single-float single-float single-float)
         mat4)
        !))
(defun ! (a b c d e f g h i j k l m n o p)
  "! a 4x4 matrix. Data must be provided in row major order"
  (declare (single-float a b c d e f g h i j k l m n o p))
  (let ((result (zero-matrix4)))
    (setf (melm result 0 0) a)
    (setf (melm result 0 1) b)
    (setf (melm result 0 2) c)
    (setf (melm result 0 3) d)
    (setf (melm result 1 0) e)
    (setf (melm result 1 1) f)
    (setf (melm result 1 2) g)
    (setf (melm result 1 3) h)
    (setf (melm result 2 0) i)
    (setf (melm result 2 1) j)
    (setf (melm result 2 2) k)
    (setf (melm result 2 3) l)
    (setf (melm result 3 0) m)
    (setf (melm result 3 1) n)
    (setf (melm result 3 2) o)
    (setf (melm result 3 3) p)
    result))

;;----------------------------------------------------------------

(declaim
 (inline to-matrix3)
 (ftype (function
         (mat4)
         mat3)
        to-matrix3))
(defun to-matrix3 (mat4)
  (m3:make (melm mat4 0 0) (melm mat4 0 1) (melm mat4 0 2)
	   (melm mat4 1 0) (melm mat4 1 1) (melm mat4 1 2)
	   (melm mat4 2 0) (melm mat4 2 1) (melm mat4 2 2)))

;;----------------------------------------------------------------

(defun from-rows (row-1 row-2 row-3 row-4)
  "Make a 4x4 matrix using the data in the 4 vector4s provided
   to populate the rows"
  (make (x row-1) (y row-1) (z row-1) (w row-1)
	(x row-2) (y row-2)	(z row-2) (w row-2)
	(x row-3) (y row-3) (z row-3) (w row-3)
	(x row-4) (y row-4) (z row-4) (w row-4)))

;;----------------------------------------------------------------

(defun get-rows (mat-a)
  "Return the rows of the matrix as 4 vector4s"
  (list (make (melm mat-a 0 0)
	      (melm mat-a 0 1)
	      (melm mat-a 0 2)
	      (melm mat-a 0 3))
        (make (melm mat-a 1 0)
	      (melm mat-a 1 1)
	      (melm mat-a 1 2)
	      (melm mat-a 1 3))
        (make (melm mat-a 2 0)
	      (melm mat-a 2 1)
	      (melm mat-a 2 2)
	      (melm mat-a 2 3))
        (make (melm mat-a 3 0)
	      (melm mat-a 3 1)
	      (melm mat-a 3 2)
	      (melm mat-a 3 3))))

;;----------------------------------------------------------------

(defun get-row (mat-a row-num)
  "Return the specified row of the matrix a vector4"
  (make (melm mat-a row-num 0)
	(melm mat-a row-num 1)
	(melm mat-a row-num 2)
	(melm mat-a row-num 3)))


;;----------------------------------------------------------------

(defun from-columns (col-1 col-2 col-3 col-4)
  "Make a 4x4 matrix using the data in the 4 vector4s provided
   to populate the columns"
  (make (x col-1)
	(x col-2)
	(x col-3)
	(x col-4)
	(y col-1)
	(y col-2)
	(y col-3)
	(y col-4)
	(z col-1)
	(z col-2)
	(z col-3)
	(z col-4)
	(w col-1)
	(w col-2)
	(w col-3)
	(w col-4)))

;;----------------------------------------------------------------

(defun get-columns (mat-a)
  "Return the columns of the matrix as 4 vector4s"
  (list (make (melm mat-a 0 0)
	      (melm mat-a 1 0)
	      (melm mat-a 2 0)
	      (melm mat-a 3 0))
        (make (melm mat-a 0 1)
	      (melm mat-a 1 1)
	      (melm mat-a 2 1)
	      (melm mat-a 3 1))
        (make (melm mat-a 0 2)
	      (melm mat-a 1 2)
	      (melm mat-a 2 2)
	      (melm mat-a 3 2))
        (make (melm mat-a 0 3)
	      (melm mat-a 1 3)
	      (melm mat-a 2 3)
	      (melm mat-a 3 3))))

;;----------------------------------------------------------------

(defun get-column (mat-a col-num)
  "Return the specified column of the matrix a vector4"
  (make (melm mat-a 0 col-num)
	(melm mat-a 1 col-num)
	(melm mat-a 2 col-num)
	(melm mat-a 3 col-num)))

;;----------------------------------------------------------------

(defun 0p (mat-a)
  "Returns 't' if this is a zero matrix (as contents of the
   matrix are floats the values have an error bound as defined
   in base-maths"
  (loop :for i :below 16 :always (cl:= 0f0 (svref mat-a i))))

;;----------------------------------------------------------------

					;[TODO] should the checks for '1.0' also have the error bounds?
(defun identityp (mat-a)
  "Returns 't' if this is an identity matrix (as contents of the
   matrix are floats the values have an error bound as defined
   in base-maths"
  (and (cl:= 0f0 (- (melm mat-a 0 0) 1.0))
       (cl:= 0f0 (- (melm mat-a 1 1) 1.0))
       (cl:= 0f0 (- (melm mat-a 2 2) 1.0))
       (cl:= 0f0 (- (melm mat-a 3 3) 1.0))
       (cl:= 0f0 (melm mat-a 0 1))
       (cl:= 0f0 (melm mat-a 0 2))
       (cl:= 0f0 (melm mat-a 0 3))
       (cl:= 0f0 (melm mat-a 1 0))
       (cl:= 0f0 (melm mat-a 1 2))
       (cl:= 0f0 (melm mat-a 1 3))
       (cl:= 0f0 (melm mat-a 2 0))
       (cl:= 0f0 (melm mat-a 2 1))
       (cl:= 0f0 (melm mat-a 2 3))
       (cl:= 0f0 (melm mat-a 3 0))
       (cl:= 0f0 (melm mat-a 3 1))
       (cl:= 0f0 (melm mat-a 3 2))))

;;----------------------------------------------------------------

(defun = (mat-a mat-b)
  "Returns t if all elements of both matrices provided are
   equal"
  (loop :for i :below 16 :always (= (svref mat-a i) (svref mat-b i))))

;;----------------------------------------------------------------

;; Ok so the determinant is of an element in the matrix is the
;; value of that element multiplied by the determinant of the
;; submatrix formed when you remove the row and column the element
;; lie in.
;; The minor is the 'determinant of the submatrix' mentioned above
;; If you create a matrix B from Matrix A where evey element in
;; B is the minor of the corrosponding element in A then
;; Matrix B is the matrix of cofactors.
;; If Matrix B is tranposed then the new Matrix (we will call
;; Matrix C) is known as the adjoint matrix
;; The Inverse of a matrix can be calculated as:
;; (* (adjoint matrix-a) (/ 1.0 (determinant matrix-a)))
;; This method is known as cramer's method and is fast enough
;; for 3x3 and 4x4 matrices. Thus we can use it for games.

(defun minor (mat-a row-0 row-1 row-2 col-0 col-1 col-2)
  (+ (* (melm mat-a row-0 col-0)
        (- (* (melm mat-a row-1 col-1) (melm mat-a row-2 col-2))
           (* (melm mat-a row-2 col-1) (melm mat-a row-1 col-2))))

     (* (melm mat-a row-0 col-2)
        (- (* (melm mat-a row-1 col-0) (melm mat-a row-2 col-1))
           (* (melm mat-a row-2 col-0) (melm mat-a row-1 col-1))))

     (- (* (melm mat-a row-0 col-1)
           (- (* (melm mat-a row-1 col-0) (melm mat-a row-2 col-2))
              (* (melm mat-a row-2 col-0) (melm mat-a row-1 col-2)))))))

;;----------------------------------------------------------------

(defun adjoint (mat-a)
  "Returns the adjoint of the matrix"
  (make (minor mat-a 1 2 3 1 2 3)
	(- (minor mat-a 0 2 3 1 2 3))
	(minor mat-a 0 1 3 1 2 3)
	(- (minor mat-a 0 1 2 1 2 3))

	(- (minor mat-a 1 2 3 0 2 3))
	(minor mat-a 0 2 3 0 2 3)
	(- (minor mat-a 0 1 3 0 2 3))
	(minor mat-a 0 1 2 0 2 3)

	(minor mat-a 1 2 3 0 1 3)
	(- (minor mat-a 0 2 3 0 1 3))
	(minor mat-a 0 1 3 0 1 3)
	(- (minor mat-a 0 1 2 0 1 3))

	(- (minor mat-a 1 2 3 0 1 2))
	(minor mat-a 0 2 3 0 1 2)
	(- (minor mat-a 0 1 3 0 1 2))
	(minor mat-a 0 1 2 0 1 2)))

;;----------------------------------------------------------------

(defun determinant (mat-a)
  "Returns the determinant of the matrix"
  (+ (* (melm mat-a 0 0) (minor mat-a 1 2 3 1 2 3))
     (- (* (melm mat-a 0 1) (minor mat-a 1 2 3 0 2 3)))
     (* (melm mat-a 0 2) (minor mat-a 1 2 3 0 1 3))
     (- (* (melm mat-a 0 3) (minor mat-a 1 2 3 0 1 2)))))

;;----------------------------------------------------------------

;;this one is from 'Essential Maths'
(declaim
 (inline affine-inverse)
 (ftype (function (mat4)
                  mat4)
        affine-inverse))
(defun affine-inverse (mat-a)
  "Returns the affine inverse of the matrix"
  (declare (mat4 mat-a))
  ;;calculate upper left 3x3 matrix determinant
  (let* ((cofac-0 (- (* (melm mat-a 1 1) (melm mat-a 2 2))
                     (* (melm mat-a 2 1) (melm mat-a 1 2))))

         (cofac-4 (- (* (melm mat-a 2 0) (melm mat-a 1 2))
                     (* (melm mat-a 1 0) (melm mat-a 2 2))))

         (cofac-8 (- (* (melm mat-a 1 0) (melm mat-a 2 1))
                     (* (melm mat-a 2 0) (melm mat-a 1 1))))
         (det (+ (* (melm mat-a 0 0) cofac-0)
                 (* (melm mat-a 0 1) cofac-4)
                 (* (melm mat-a 0 2) cofac-8))))
    (if
     (cl:= 0f0 det)
     (error "Matrix4 Inverse: Singular Matrix")
     (let*
         ((inv-det (/ 1.0 det))
          (r00 (* inv-det cofac-0))
          (r10 (* inv-det cofac-4))
          (r20 (* inv-det cofac-8))
          (r01 (* inv-det (- (* (melm mat-a 2 1) (melm mat-a 0 2))
                             (* (melm mat-a 0 1) (melm mat-a 2 2)))))
          (r11 (* inv-det (- (* (melm mat-a 0 0) (melm mat-a 2 2))
                             (* (melm mat-a 2 0) (melm mat-a 0 2)))))
          (r21 (* inv-det (- (* (melm mat-a 2 0) (melm mat-a 0 1))
                             (* (melm mat-a 0 0) (melm mat-a 2 1)))))
          (r02 (* inv-det (- (* (melm mat-a 0 1) (melm mat-a 1 2))
                             (* (melm mat-a 1 1) (melm mat-a 0 2)))))
          (r12 (* inv-det (- (* (melm mat-a 1 0) (melm mat-a 0 2))
                             (* (melm mat-a 0 0) (melm mat-a 1 2)))))
          (r22 (* inv-det (- (* (melm mat-a 0 0) (melm mat-a 1 1))
                             (* (melm mat-a 1 0) (melm mat-a 0 1))))))
       (make r00 r01 r02
	     (- 0.0
		(* (melm mat-a 0 0) (melm mat-a 0 3))
		(* (melm mat-a 0 1) (melm mat-a 1 3))
		(* (melm mat-a 0 2) (melm mat-a 2 3)))
	     r10 r11 r12
	     (- 0.0
		(* (melm mat-a 1 0) (melm mat-a 0 3))
		(* (melm mat-a 1 1) (melm mat-a 1 3))
		(* (melm mat-a 1 2) (melm mat-a 2 3)))
	     r20 r21 r22
	     (- 0.0
		(* (melm mat-a 2 0) (melm mat-a 0 3))
		(* (melm mat-a 2 1) (melm mat-a 1 3))
		(* (melm mat-a 2 2) (melm mat-a 2 3)))
	     0.0 0.0 0.0 1.0)))))

;;----------------------------------------------------------------
;; could just feed straight from array into make
(defun transpose (m-a)
  "Returns the transpose of the provided matrix"
  (make
   (melm m-a 0 0) (melm m-a 1 0) (melm m-a 2 0) (melm m-a 3 0)
   (melm m-a 0 1) (melm m-a 1 1) (melm m-a 2 1) (melm m-a 3 1)
   (melm m-a 0 2) (melm m-a 1 2) (melm m-a 2 2) (melm m-a 3 2)
   (melm m-a 0 3) (melm m-a 1 3) (melm m-a 2 3) (melm m-a 3 3)))

;;----------------------------------------------------------------

(defun translation (vec3-a)
  "Takes a vector3 and returns a matrix4 which will translate
   by the specified amount"
  (make
   1.0  0.0  0.0  (x vec3-a)
   0.0  1.0  0.0  (y vec3-a)
   0.0  0.0  1.0  (z vec3-a)
   0.0  0.0  0.0  1.0))

;;----------------------------------------------------------------

;;need quaternion
;;(defun make-rotation)

;;----------------------------------------------------------------

(defun rotation-from-matrix3 (m-a)
  "Takes a 3x3 rotation matrix and returns a 4x4 rotation matrix
   with the same values. The 4th component is filled as an
   identity matrix would be."
  (make
   (m3:melm m-a 0 0)  (m3:melm m-a 0 1)  (m3:melm m-a 0 2)  0.0
   (m3:melm m-a 1 0)  (m3:melm m-a 1 1)  (m3:melm m-a 1 2)  0.0
   (m3:melm m-a 2 0)  (m3:melm m-a 2 1)  (m3:melm m-a 2 2)  0.0
   0.0                0.0                0.0                1.0))

;;----------------------------------------------------------------

(defun rotation-from-euler (vec3-a)
  "This is an unrolled contatenation of rotation matrices x
   y & z. The arguments in the originl were in reverse order"
  (let ((x (x vec3-a)) (y (y vec3-a)) (z (z vec3-a)))
    (let ((sx (sin x)) (cx (cos x))
          (sy (sin y)) (cy (cos y))
          (sz (sin z)) (cz (cos z)))
      (make (* cy cz)
	    (- (* cy sz))
	    sy
	    0.0

	    (+ (* sx sy cz) (* cx sz))
	    (- (* cx cz) (* sx sy sz))
	    (- (* sx cy))
	    0.0

	    (- (* sx sz) (* cx sy cz))
	    (+ (* cx sy sz) (* sx cz))
	    (* cx cy)

	    0.0 0.0 0.0 0.0 1.0))))

;;----------------------------------------------------------------

(defun rotation-from-axis-angle (axis3 angle)
  "Returns a matrix which will rotate a point about the axis
   specified by the angle provided"
  (cond ((v3:= axis3 (v3:make 1f0 0f0 0f0)) (rotation-x angle))
        ((v3:= axis3 (v3:make 0f0 1f0 0f0)) (rotation-y angle))
        ((v3:= axis3 (v3:make 0f0 0f0 1f0)) (rotation-z angle))
        (t
         (let ((c (cos angle))
               (s (sin angle))
               (g (- 1f0 (cos angle))))
           (let* ((x (x axis3))
                  (y (y axis3))
                  (z (z axis3))
                  (gxx (* g x x)) (gxy (* g x y)) (gxz (* g x z))
                  (gyy (* g y y)) (gyz (* g y z)) (gzz (* g z z)))
             (m4:make
              (+ gxx c)        (- gxy (* s z))  (+ gxz (* s y)) 0f0
              (+ gxy (* s z))  (+ gyy c)        (- gyz (* s x)) 0f0
              (- gxz (* s y))  (+ gyz (* s x))  (+ gzz c)       0f0
              0f0              0f0              0f0             1f0))))))

;;----------------------------------------------------------------

(defun scale (scale-vec3)
  "Returns a matrix which will scale by the amounts specified"
  (make
   (x scale-vec3)  0.0               0.0               0.0
   0.0               (y scale-vec3)  0.0               0.0
   0.0               0.0               (z scale-vec3)  0.0
   0.0               0.0               0.0               1.0))

;;----------------------------------------------------------------

(defun rotation-x (angle)
  "Returns a matrix which would rotate a point around the x axis
   by the specified amount"
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make 1.0  0.0  0.0     0.0
	  0.0  c-a  (- s-a) 0.0
	  0.0  s-a  c-a     0.0
	  0.0  0.0  0.0     1.0)))

;;----------------------------------------------------------------

(defun rotation-y (angle)
  "Returns a matrix which would rotate a point around the y axis
   by the specified amount"
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make c-a      0.0  s-a  0.0
	  0.0      1.0  0.0  0.0
	  (- s-a)  0.0  c-a  0.0
	  0.0      0.0  0.0  1.0)))

;;----------------------------------------------------------------

(defun rotation-z (angle)
  "Returns a matrix which would rotate a point around the z axis
   by the specified amount"
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make c-a  (- s-a)  0.0  0.0
	  s-a  c-a      0.0  0.0
	  0.0  0.0      1.0  0.0
	  0.0  0.0      0.0  1.0)))

;;----------------------------------------------------------------

;; [TODO] returned as vector x-y-z

(defun get-fixed-angles (mat-a)
  "Gets one set of possible z-y-x fixed angles that will generate
   this matrix. Assumes that this is a rotation matrix. Result
   is returned as vector3"
  (let* ((sy (melm mat-a 0 2))
         (cy (sqrt (- 1.0 (* sy sy)))))
    (if (cl:= 0f0 cy)
        (let ((sz 0.0)
              (cz 1.0)
              (sx (melm mat-a 2 1))
              (cx (melm mat-a 1 1)))
          (make (atan sx cx) (atan sy cy) (atan sz cz)))
        (let* ((factor (/ 1.0 cy)) ; normal case
               (sx (- (* factor (melm mat-a 1 2))))
               (cx (* factor (melm mat-a 2 2)))
               (sz (- (* factor (melm mat-a 0 1))))
               (cz (* factor (melm mat-a 0 0))))
          (make (atan sx cx) (atan sy cy) (atan sz cz))))))

;;----------------------------------------------------------------

(defun mtrace (mat-a)
  "Returns the trace of the matrix (That is the diagonal values)"
  (+ (melm mat-a 0 0) (melm mat-a 1 1)
     (melm mat-a 2 2) (melm mat-a 3 3)))

;;----------------------------------------------------------------

;; [TODO] find out how we can declaim angle to be float
;; [TODO] Comment the fuck out of this and work out how it works

(defun get-axis-angle (mat-a)
  "Gets one possible axis-angle pair that will generate this
   matrix. Assumes that this is a rotation matrix"
  (let* ((trace-a (+ (melm mat-a 0 0) (melm mat-a 1 1)
                     (melm mat-a 2 2)))
         (cos-theta (* 0.5 (- trace-a 1.0)))
         (angle (acos cos-theta)))
    (cond ((cl:= 0f0 angle) (values (make 1f0 0f0 0f0) angle))
          ((cl:= 0f0 (- rtg-math.base-maths:+pi+ angle))
           (values
            (v3:normalize
             (make (- (melm mat-a 2 1) (melm mat-a 1 2))
		   (- (melm mat-a 0 2) (melm mat-a 2 0))
		   (- (melm mat-a 1 0) (melm mat-a 0 1))))
            angle))
          (t (labels ((biggest-trace (matr)
                        (let ((x 0))
                          (if (> (melm matr 1 1) (melm matr 0 0))
                              (setf x 1))
                          (if (> (melm matr 2 2) (melm matr x x))
                              (setf x 2))
                          x)))
               (let* ((i (biggest-trace mat-a))
                      (j (mod (+ i 1) 3))
                      (k (mod (+ i 1) 3))
                      (s (sqrt (+ 1.0 (- (melm mat-a i i)
					 (melm mat-a j j)
					 (melm mat-a k k)))))
                      (recip (/ 1.0 s)))
                 (values (make
                          (* 0.5 s)
                          (* recip (svref mat-a (+ i (* 4 j))))
                          (* recip (svref mat-a (+ k (* 4 i)))))
                         angle)))))))


;;----------------------------------------------------------------

(defun m+ (mat-a mat-b)
  "Adds the 2 matrices component wise and returns the result as
   a new matrix"
  (let ((r (zero-matrix4)))
    (loop :for i :below 16
       :do (setf (svref r i) (+ (svref mat-a i) (svref mat-b i))))
    r))

;;----------------------------------------------------------------

(defun m- (mat-a mat-b)
  "Subtracts the 2 matrices component wise and returns the result
   as a new matrix"
  (let ((r (zero-matrix4)))
    (loop :for i :below 16
       :do (setf (svref r i) (- (svref mat-a i) (svref mat-b i))))
    r))

;;----------------------------------------------------------------

(defun negate (mat-a)
  "Negates the components of the matrix"
  (let ((r (zero-matrix4)))
    (loop :for i :below 16 :do (setf (svref r i) (- (svref mat-a i))))
    r))

;;----------------------------------------------------------------

(defun m*scalar (mat-a scalar)
  "Multiplies the components of the matrix by the scalar
   provided"
  (let ((result (zero-matrix4)))
    (loop :for i :below 16
       :do (setf (svref result i) (* scalar (svref mat-a i))))
    result))

;;----------------------------------------------------------------

(defun mcol*vec4 (mat-a vec)
  (make
   (+ (* (x vec) (melm mat-a 0 0)) (* (y vec) (melm mat-a 0 1))
      (* (z vec) (melm mat-a 0 2)) (* (w vec) (melm mat-a 0 3)))

   (+ (* (x vec) (melm mat-a 1 0)) (* (y vec) (melm mat-a 1 1))
      (* (z vec) (melm mat-a 1 2)) (* (w vec) (melm mat-a 1 3)))

   (+ (* (x vec) (melm mat-a 2 0)) (* (y vec) (melm mat-a 2 1))
      (* (z vec) (melm mat-a 2 2)) (* (w vec) (melm mat-a 2 3)))

   (+ (* (x vec) (melm mat-a 3 0)) (* (y vec) (melm mat-a 3 1))
      (* (z vec) (melm mat-a 3 2)) (* (w vec) (melm mat-a 3 3)))
   ))

;;----------------------------------------------------------------

(defun mrow*vec4 (vec mat-a)
  (make
   (+ (* (x vec) (melm mat-a 0 0)) (* (y vec) (melm mat-a 1 0))
      (* (z vec) (melm mat-a 2 0)) (* (w vec) (melm mat-a 3 0)))

   (+ (* (x vec) (melm mat-a 0 1)) (* (y vec) (melm mat-a 1 1))
      (* (z vec) (melm mat-a 2 1)) (* (w vec) (melm mat-a 3 1)))

   (+ (* (x vec) (melm mat-a 0 2)) (* (y vec) (melm mat-a 1 2))
      (* (z vec) (melm mat-a 2 2)) (* (w vec) (melm mat-a 3 2)))

   (+ (* (x vec) (melm mat-a 0 3)) (* (y vec) (melm mat-a 1 3))
      (* (z vec) (melm mat-a 2 3)) (* (w vec) (melm mat-a 3 3)))
   ))

;;----------------------------------------------------------------

(defun m* (mat-a mat-b)
  "Multiplies 2 matrices and returns the result as a new
   matrix"
  (make (+ (* (melm mat-a 0 0) (melm mat-b 0 0))
	   (* (melm mat-a 0 1) (melm mat-b 1 0))
	   (* (melm mat-a 0 2) (melm mat-b 2 0))
	   (* (melm mat-a 0 3) (melm mat-b 3 0)))
	(+ (* (melm mat-a 0 0) (melm mat-b 0 1))
	   (* (melm mat-a 0 1) (melm mat-b 1 1))
	   (* (melm mat-a 0 2) (melm mat-b 2 1))
	   (* (melm mat-a 0 3) (melm mat-b 3 1)))
	(+ (* (melm mat-a 0 0) (melm mat-b 0 2))
	   (* (melm mat-a 0 1) (melm mat-b 1 2))
	   (* (melm mat-a 0 2) (melm mat-b 2 2))
	   (* (melm mat-a 0 3) (melm mat-b 3 2)))
	(+ (* (melm mat-a 0 0) (melm mat-b 0 3))
	   (* (melm mat-a 0 1) (melm mat-b 1 3))
	   (* (melm mat-a 0 2) (melm mat-b 2 3))
	   (* (melm mat-a 0 3) (melm mat-b 3 3)))
	(+ (* (melm mat-a 1 0) (melm mat-b 0 0))
	   (* (melm mat-a 1 1) (melm mat-b 1 0))
	   (* (melm mat-a 1 2) (melm mat-b 2 0))
	   (* (melm mat-a 1 3) (melm mat-b 3 0)))
	(+ (* (melm mat-a 1 0) (melm mat-b 0 1))
	   (* (melm mat-a 1 1) (melm mat-b 1 1))
	   (* (melm mat-a 1 2) (melm mat-b 2 1))
	   (* (melm mat-a 1 3) (melm mat-b 3 1)))
	(+ (* (melm mat-a 1 0) (melm mat-b 0 2))
	   (* (melm mat-a 1 1) (melm mat-b 1 2))
	   (* (melm mat-a 1 2) (melm mat-b 2 2))
	   (* (melm mat-a 1 3) (melm mat-b 3 2)))
	(+ (* (melm mat-a 1 0) (melm mat-b 0 3))
	   (* (melm mat-a 1 1) (melm mat-b 1 3))
	   (* (melm mat-a 1 2) (melm mat-b 2 3))
	   (* (melm mat-a 1 3) (melm mat-b 3 3)))
	(+ (* (melm mat-a 2 0) (melm mat-b 0 0))
	   (* (melm mat-a 2 1) (melm mat-b 1 0))
	   (* (melm mat-a 2 2) (melm mat-b 2 0))
	   (* (melm mat-a 2 3) (melm mat-b 3 0)))
	(+ (* (melm mat-a 2 0) (melm mat-b 0 1))
	   (* (melm mat-a 2 1) (melm mat-b 1 1))
	   (* (melm mat-a 2 2) (melm mat-b 2 1))
	   (* (melm mat-a 2 3) (melm mat-b 3 1)))
	(+ (* (melm mat-a 2 0) (melm mat-b 0 2))
	   (* (melm mat-a 2 1) (melm mat-b 1 2))
	   (* (melm mat-a 2 2) (melm mat-b 2 2))
	   (* (melm mat-a 2 3) (melm mat-b 3 2)))
	(+ (* (melm mat-a 2 0) (melm mat-b 0 3))
	   (* (melm mat-a 2 1) (melm mat-b 1 3))
	   (* (melm mat-a 2 2) (melm mat-b 2 3))
	   (* (melm mat-a 2 3) (melm mat-b 3 3)))
	(+ (* (melm mat-a 3 0) (melm mat-b 0 0))
	   (* (melm mat-a 3 1) (melm mat-b 1 0))
	   (* (melm mat-a 3 2) (melm mat-b 2 0))
	   (* (melm mat-a 3 3) (melm mat-b 3 0)))
	(+ (* (melm mat-a 3 0) (melm mat-b 0 1))
	   (* (melm mat-a 3 1) (melm mat-b 1 1))
	   (* (melm mat-a 3 2) (melm mat-b 2 1))
	   (* (melm mat-a 3 3) (melm mat-b 3 1)))
	(+ (* (melm mat-a 3 0) (melm mat-b 0 2))
	   (* (melm mat-a 3 1) (melm mat-b 1 2))
	   (* (melm mat-a 3 2) (melm mat-b 2 2))
	   (* (melm mat-a 3 3) (melm mat-b 3 2)))
	(+ (* (melm mat-a 3 0) (melm mat-b 0 3))
	   (* (melm mat-a 3 1) (melm mat-b 1 3))
	   (* (melm mat-a 3 2) (melm mat-b 2 3))
	   (* (melm mat-a 3 3) (melm mat-b 3 3)))))

;;----------------------------------------------------------------

(defun transform (mat-a vec)
  "Returns the transform of a matrix"
  (make (+ (* (melm mat-a 0 0) (x vec))
	   (* (melm mat-a 0 1) (y vec))
	   (* (melm mat-a 0 2) (z vec))
	   (melm mat-a 0 3))
	(+ (* (melm mat-a 1 0) (x vec))
	   (* (melm mat-a 1 1) (y vec))
	   (* (melm mat-a 1 2) (z vec))
	   (melm mat-a 1 3))
	(+ (* (melm mat-a 2 0) (x vec))
	   (* (melm mat-a 2 1) (y vec))
	   (* (melm mat-a 2 2) (z vec))
	   (melm mat-a 2 3))))

;;----------------------------------------------------------------

(defun print-m4 (m4)
  (apply #'format t
	 "~%(m! ~s ~s ~s ~s~%    ~s ~s ~s ~s~%    ~s ~s ~s ~s~%    ~s ~s ~s ~s)"
	 (concatenate 'list (transpose m4)))
  m4)
