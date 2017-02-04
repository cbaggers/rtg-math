(in-package :rtg-math.matrix4)

;;----------------------------------------------------------------

(declaim (inline identity)
         (ftype (function ()
                          mat4)
                identity))
(defun identity ()
  "Return a 4x4 identity matrix"
  (make-array 16 :element-type 'single-float
              :initial-contents '(1f0 0f0 0f0 0f0
                                  0f0 1f0 0f0 0f0
                                  0f0 0f0 1f0 0f0
                                  0f0 0f0 0f0 1f0)))

(defun 0! ()
  "Return a 4x4 zero matrix"
  (make-array 16 :element-type 'single-float :initial-element 0f0))

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
  (let ((result (0!)))
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
 (inline to-mat3)
 (ftype (function (mat4) mat3)
        to-mat3))
(defun to-mat3 (mat4)
  (m3:make (melm mat4 0 0) (melm mat4 0 1) (melm mat4 0 2)
           (melm mat4 1 0) (melm mat4 1 1) (melm mat4 1 2)
           (melm mat4 2 0) (melm mat4 2 1) (melm mat4 2 2)))

;;----------------------------------------------------------------

(defun from-mat3 (m-a)
  "Takes a 3x3 matrix and returns a 4x4 rotation matrix
   with the same values. The 4th component is filled as an
   identity matrix would be."
  (make
   (m3:melm m-a 0 0)  (m3:melm m-a 0 1)  (m3:melm m-a 0 2)  0f0
   (m3:melm m-a 1 0)  (m3:melm m-a 1 1)  (m3:melm m-a 1 2)  0f0
   (m3:melm m-a 2 0)  (m3:melm m-a 2 1)  (m3:melm m-a 2 2)  0f0
   0f0                0f0                0f0                1f0))

;;----------------------------------------------------------------

(defun from-rows (row-1 row-2 row-3 row-4)
  "Make a 4x4 matrix using the data in the 4 vector4s provided
   to populate the rows"
  (make (x row-1) (y row-1) (z row-1) (w row-1)
        (x row-2) (y row-2) (z row-2) (w row-2)
        (x row-3) (y row-3) (z row-3) (w row-3)
        (x row-4) (y row-4) (z row-4) (w row-4)))

(defun from-rows-v3 (row-1 row-2 row-3)
  "Make a 4x4 matrix using the data in the 4 vector4s provided
   to populate the rows"
  (make (x row-1) (y row-1) (z row-1) 0s0
        (x row-2) (y row-2) (z row-2) 0s0
        (x row-3) (y row-3) (z row-3) 0s0
        0s0       0s0       0s0       1s0))

;;----------------------------------------------------------------

(defun get-rows (mat-a)
  "Return the rows of the matrix as 4 vector4s"
  (list (v! (melm mat-a 0 0)
            (melm mat-a 0 1)
            (melm mat-a 0 2)
            (melm mat-a 0 3))
        (v! (melm mat-a 1 0)
            (melm mat-a 1 1)
            (melm mat-a 1 2)
            (melm mat-a 1 3))
        (v! (melm mat-a 2 0)
            (melm mat-a 2 1)
            (melm mat-a 2 2)
            (melm mat-a 2 3))
        (v! (melm mat-a 3 0)
            (melm mat-a 3 1)
            (melm mat-a 3 2)
            (melm mat-a 3 3))))

;;----------------------------------------------------------------

(defun get-row (mat-a row-num)
  "Return the specified row of the matrix a vector4"
  (v! (melm mat-a row-num 0)
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

(defun from-columns-v3 (col-1 col-2 col-3)
  "Make a 4x4 matrix using the data in the 3 vector3s provided
   to populate the columns"
  (make (x col-1)
        (x col-2)
        (x col-3)
        0s0
        (y col-1)
        (y col-2)
        (y col-3)
        0s0
        (z col-1)
        (z col-2)
        (z col-3)
        0s0
        0s0
        0s0
        0s0
        1s0))

;;----------------------------------------------------------------

(defun get-columns (mat-a)
  "Return the columns of the matrix as 4 vector4s"
  (list (v! (melm mat-a 0 0)
            (melm mat-a 1 0)
            (melm mat-a 2 0)
            (melm mat-a 3 0))
        (v! (melm mat-a 0 1)
            (melm mat-a 1 1)
            (melm mat-a 2 1)
            (melm mat-a 3 1))
        (v! (melm mat-a 0 2)
            (melm mat-a 1 2)
            (melm mat-a 2 2)
            (melm mat-a 3 2))
        (v! (melm mat-a 0 3)
            (melm mat-a 1 3)
            (melm mat-a 2 3)
            (melm mat-a 3 3))))

;;----------------------------------------------------------------

(defun get-column (mat-a col-num)
  "Return the specified column of the matrix a vector4"
  (v! (melm mat-a 0 col-num)
      (melm mat-a 1 col-num)
      (melm mat-a 2 col-num)
      (melm mat-a 3 col-num)))

;;----------------------------------------------------------------

(defun 0p (mat-a)
  "Returns 't' if this is a zero matrix (as contents of the
   matrix are floats the values have an error bound as defined
   in base-maths"
  (loop :for i :below 16 :always (cl:= 0f0 (aref mat-a i))))

;;----------------------------------------------------------------

(defun identityp (mat-a)
  "Returns 't' if this is an identity matrix (as contents of the
   matrix are floats the values have an error bound as defined
   in base-maths"
  (and (cl:= 0f0 (cl:- (melm mat-a 0 0) 1f0))
       (cl:= 0f0 (cl:- (melm mat-a 1 1) 1f0))
       (cl:= 0f0 (cl:- (melm mat-a 2 2) 1f0))
       (cl:= 0f0 (cl:- (melm mat-a 3 3) 1f0))
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
  (loop :for i :below 16 :always (= (aref mat-a i) (aref mat-b i))))

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
;; (cl:* (adjoint matrix-a) (cl:/ 1f0 (determinant matrix-a)))
;; This method is known as cramer's method and is fast enough
;; for 3x3 and 4x4 matrices. Thus we can use it for games.

(defun minor (mat-a row-0 row-1 row-2 col-0 col-1 col-2)
  (cl:+ (cl:* (melm mat-a row-0 col-0)
              (cl:- (cl:* (melm mat-a row-1 col-1) (melm mat-a row-2 col-2))
                    (cl:* (melm mat-a row-2 col-1) (melm mat-a row-1 col-2))))

        (cl:* (melm mat-a row-0 col-2)
              (cl:- (cl:* (melm mat-a row-1 col-0) (melm mat-a row-2 col-1))
                    (cl:* (melm mat-a row-2 col-0) (melm mat-a row-1 col-1))))

        (cl:- (cl:* (melm mat-a row-0 col-1)
                    (cl:- (cl:* (melm mat-a row-1 col-0) (melm mat-a row-2 col-2))
                          (cl:* (melm mat-a row-2 col-0) (melm mat-a row-1 col-2)))))))

;;----------------------------------------------------------------

(defun adjoint (mat-a)
  "Returns the adjoint of the matrix"
  (make (minor mat-a 1 2 3 1 2 3)
        (cl:- (minor mat-a 0 2 3 1 2 3))
        (minor mat-a 0 1 3 1 2 3)
        (cl:- (minor mat-a 0 1 2 1 2 3))

        (cl:- (minor mat-a 1 2 3 0 2 3))
        (minor mat-a 0 2 3 0 2 3)
        (cl:- (minor mat-a 0 1 3 0 2 3))
        (minor mat-a 0 1 2 0 2 3)

        (minor mat-a 1 2 3 0 1 3)
        (cl:- (minor mat-a 0 2 3 0 1 3))
        (minor mat-a 0 1 3 0 1 3)
        (cl:- (minor mat-a 0 1 2 0 1 3))

        (cl:- (minor mat-a 1 2 3 0 1 2))
        (minor mat-a 0 2 3 0 1 2)
        (cl:- (minor mat-a 0 1 3 0 1 2))
        (minor mat-a 0 1 2 0 1 2)))

;;----------------------------------------------------------------

(defun determinant (mat-a)
  "Returns the determinant of the matrix"
  (cl:+ (cl:* (melm mat-a 0 0) (minor mat-a 1 2 3 1 2 3))
        (cl:- (cl:* (melm mat-a 0 1) (minor mat-a 1 2 3 0 2 3)))
        (cl:* (melm mat-a 0 2) (minor mat-a 1 2 3 0 1 3))
        (cl:- (cl:* (melm mat-a 0 3) (minor mat-a 1 2 3 0 1 2)))))

;;----------------------------------------------------------------

(declaim (ftype (function (mat4) mat4) inverse-matrix))
(defun inverse (matrix)
  (let ((det (m4:determinant matrix)))
    (if (cl:= det 0s0)
        (error "Cannot invert matrix with zero determinant:~%  ~S"
               matrix)
        (macrolet ((a (x y z)
                     (multiple-value-bind (r1 c1) (truncate (cl:- x 11) 10)
                       (multiple-value-bind (r2 c2) (truncate (cl:- y 11) 10)
                         (multiple-value-bind (r3 c3) (truncate (cl:- z 11) 10)
                           `(cl:* (melm matrix ,r1 ,c1)
                                  (melm matrix ,r2 ,c2)
                                  (melm matrix ,r3 ,c3)))))))
          (let ((m
                 (rtg-math:m!
                  ;; row 1
                  (cl:- (cl:+ (a 22 33 44) (a 23 34 42) (a 24 32 43))
                        (a 22 34 43) (a 23 32 44) (a 24 33 42))
                  (cl:- (cl:+ (a 12 34 43) (a 13 32 44) (a 14 33 42))
                        (a 12 33 44) (a 13 34 42) (a 14 32 43))
                  (cl:- (cl:+ (a 12 23 44) (a 13 24 42) (a 14 22 43))
                        (a 12 24 43) (a 13 22 44) (a 14 23 42))
                  (cl:- (cl:+ (a 12 24 33) (a 13 22 34) (a 14 23 32))
                        (a 12 23 34) (a 13 24 32) (a 14 22 33))
                  ;; row 2
                  (cl:- (cl:+ (a 21 34 43) (a 23 31 44) (a 24 33 41))
                        (a 21 33 44) (a 23 34 41) (a 24 31 43))
                  (cl:- (cl:+ (a 11 33 44) (a 13 34 41) (a 14 31 43))
                        (a 11 34 43) (a 13 31 44) (a 14 33 41))
                  (cl:- (cl:+ (a 11 24 43) (a 13 21 44) (a 14 23 41))
                        (a 11 23 44) (a 13 24 41) (a 14 21 43))
                  (cl:- (cl:+ (a 11 23 34) (a 13 24 31) (a 14 21 33))
                        (a 11 24 33) (a 13 21 34) (a 14 23 31))
                  ;; row 3
                  (cl:- (cl:+ (a 21 32 44) (a 22 34 41) (a 24 31 42))
                        (a 21 34 42) (a 22 31 44) (a 24 32 41))
                  (cl:- (cl:+ (a 11 34 42) (a 12 31 44) (a 14 32 41))
                        (a 11 32 44) (a 12 34 41) (a 14 31 42))
                  (cl:- (cl:+ (a 11 22 44) (a 12 24 41) (a 14 21 42))
                        (a 11 24 42) (a 12 21 44) (a 14 22 41))
                  (cl:- (cl:+ (a 11 24 32) (a 12 21 34) (a 14 22 31))
                        (a 11 22 34) (a 12 24 31) (a 14 21 32))
                  ;; row 4
                  (cl:- (cl:+ (a 21 33 42) (a 22 31 43) (a 23 32 41))
                        (a 21 32 43) (a 22 33 41) (a 23 31 42))
                  (cl:- (cl:+ (a 11 32 43) (a 12 33 41) (a 13 31 42))
                        (a 11 33 42) (a 12 31 43) (a 13 32 41))
                  (cl:- (cl:+ (a 11 23 42) (a 12 21 43) (a 13 22 41))
                        (a 11 22 43) (a 12 23 41) (a 13 21 42))
                  (cl:- (cl:+ (a 11 22 33) (a 12 23 31) (a 13 21 32))
                        (a 11 23 32) (a 12 21 33) (a 13 22 31)))))
            (dotimes (i 4)
              (dotimes (j 4)
                (setf (melm m i j) (cl:/ (melm m i j) det))))
            m)))))

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
  (let* ((cofac-0 (cl:- (cl:* (melm mat-a 1 1) (melm mat-a 2 2))
                        (cl:* (melm mat-a 2 1) (melm mat-a 1 2))))

         (cofac-4 (cl:- (cl:* (melm mat-a 2 0) (melm mat-a 1 2))
                        (cl:* (melm mat-a 1 0) (melm mat-a 2 2))))

         (cofac-8 (cl:- (cl:* (melm mat-a 1 0) (melm mat-a 2 1))
                        (cl:* (melm mat-a 2 0) (melm mat-a 1 1))))
         (det (cl:+ (cl:* (melm mat-a 0 0) cofac-0)
                    (cl:* (melm mat-a 0 1) cofac-4)
                    (cl:* (melm mat-a 0 2) cofac-8))))
    (if
     (cl:= 0f0 det)
     (error "Matrix4 Inverse: Singular Matrix")
     (let*
         ((inv-det (cl:/ 1f0 det))
          (r00 (cl:* inv-det cofac-0))
          (r10 (cl:* inv-det cofac-4))
          (r20 (cl:* inv-det cofac-8))
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
             (cl:- 0f0
                   (cl:* (melm mat-a 0 0) (melm mat-a 0 3))
                   (cl:* (melm mat-a 0 1) (melm mat-a 1 3))
                   (cl:* (melm mat-a 0 2) (melm mat-a 2 3)))
             r10 r11 r12
             (cl:- 0f0
                   (cl:* (melm mat-a 1 0) (melm mat-a 0 3))
                   (cl:* (melm mat-a 1 1) (melm mat-a 1 3))
                   (cl:* (melm mat-a 1 2) (melm mat-a 2 3)))
             r20 r21 r22
             (cl:- 0f0
                   (cl:* (melm mat-a 2 0) (melm mat-a 0 3))
                   (cl:* (melm mat-a 2 1) (melm mat-a 1 3))
                   (cl:* (melm mat-a 2 2) (melm mat-a 2 3)))
             0f0 0f0 0f0 1f0)))))

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
   1f0  0f0  0f0  (x vec3-a)
   0f0  1f0  0f0  (y vec3-a)
   0f0  0f0  1f0  (z vec3-a)
   0f0  0f0  0f0  1f0))

;;----------------------------------------------------------------

;;need quaternion
;;(defun make-rotation)

;;----------------------------------------------------------------

(defun rotation-from-mat3 (m-a)
  "Takes a 3x3 rotation matrix and returns a 4x4 rotation matrix
   with the same values. The 4th component is filled as an
   identity matrix would be."
  (make
   (m3:melm m-a 0 0)  (m3:melm m-a 0 1)  (m3:melm m-a 0 2)  0f0
   (m3:melm m-a 1 0)  (m3:melm m-a 1 1)  (m3:melm m-a 1 2)  0f0
   (m3:melm m-a 2 0)  (m3:melm m-a 2 1)  (m3:melm m-a 2 2)  0f0
   0f0                0f0                0f0                1f0))

;;----------------------------------------------------------------

(defun rotation-from-euler (vec3-a)
  "This is an unrolled contatenation of rotation matrices x
   y & z. The arguments in the originl were in reverse order"
  (let ((x (x vec3-a)) (y (y vec3-a)) (z (z vec3-a)))
    (let ((sx (sin x)) (cx (cos x))
          (sy (sin y)) (cy (cos y))
          (sz (sin z)) (cz (cos z)))
      (make (cl:* cy cz)
            (cl:- (cl:* cy sz))
            sy
            0f0

            (cl:+ (cl:* sx sy cz) (cl:* cx sz))
            (cl:- (cl:* cx cz) (cl:* sx sy sz))
            (cl:- (cl:* sx cy))
            0f0

            (cl:- (cl:* sx sz) (cl:* cx sy cz))
            (cl:+ (cl:* cx sy sz) (cl:* sx cz))
            (cl:* cx cy)

            0f0 0f0 0f0 0f0 1f0))))

;;----------------------------------------------------------------

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
             (m4:make
              (cl:+ gxx c)        (cl:- gxy (cl:* s z))  (cl:+ gxz (cl:* s y)) 0f0
              (cl:+ gxy (cl:* s z))  (cl:+ gyy c)        (cl:- gyz (cl:* s x)) 0f0
              (cl:- gxz (cl:* s y))  (cl:+ gyz (cl:* s x))  (cl:+ gzz c)       0f0
              0f0              0f0              0f0             1f0))))))

;;----------------------------------------------------------------

(defun scale (scale-vec3)
  "Returns a matrix which will scale by the amounts specified"
  (make
   (x scale-vec3)  0f0               0f0               0f0
   0f0               (y scale-vec3)  0f0               0f0
   0f0               0f0               (z scale-vec3)  0f0
   0f0               0f0               0f0               1f0))

;;----------------------------------------------------------------

(defun rotation-x (angle)
  "Returns a matrix which would rotate a point around the x axis
   by the specified amount"
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make 1f0  0f0  0f0     0f0
          0f0  c-a  (cl:- s-a) 0f0
          0f0  s-a  c-a     0f0
          0f0  0f0  0f0     1f0)))

;;----------------------------------------------------------------

(defun rotation-y (angle)
  "Returns a matrix which would rotate a point around the y axis
   by the specified amount"
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make c-a      0f0  s-a  0f0
          0f0      1f0  0f0  0f0
          (cl:- s-a)  0f0  c-a  0f0
          0f0      0f0  0f0  1f0)))

;;----------------------------------------------------------------

(defun rotation-z (angle)
  "Returns a matrix which would rotate a point around the z axis
   by the specified amount"
  (let ((s-a (sin angle))
        (c-a (cos angle)))
    (make c-a  (cl:- s-a)  0f0  0f0
          s-a  c-a      0f0  0f0
          0f0  0f0      1f0  0f0
          0f0  0f0      0f0  1f0)))

;;----------------------------------------------------------------

;; [TODO] returned as vector x-y-z

(defun get-fixed-angles (mat-a)
  "Gets one set of possible z-y-x fixed angles that will generate
   this matrix. Assumes that this is a rotation matrix. Result
   is returned as vector3"
  (let* ((sy (melm mat-a 0 2))
         (cy (sqrt (cl:- 1f0 (cl:* sy sy)))))
    (if (cl:= 0f0 cy)
        (let ((sz 0f0)
              (cz 1f0)
              (sx (melm mat-a 2 1))
              (cx (melm mat-a 1 1)))
          (v! (atan sx cx) (atan sy cy) (atan sz cz)))
        (let* ((factor (cl:/ 1f0 cy)) ; normal case
               (sx (cl:- (cl:* factor (melm mat-a 1 2))))
               (cx (cl:* factor (melm mat-a 2 2)))
               (sz (cl:- (cl:* factor (melm mat-a 0 1))))
               (cz (cl:* factor (melm mat-a 0 0))))
          (v! (atan sx cx) (atan sy cy) (atan sz cz))))))

;;----------------------------------------------------------------

(defun trace (mat-a)
  "Returns the trace of the matrix (That is the diagonal values)"
  (cl:+ (melm mat-a 0 0) (melm mat-a 1 1)
        (melm mat-a 2 2) (melm mat-a 3 3)))

;;----------------------------------------------------------------

;; [TODO] find out how we can declaim angle to be float
;; [TODO] Comment the fuck out of this and work out how it works

(defun get-axis-angle (mat-a)
  "Gets one possible axis-angle pair that will generate this
   matrix. Assumes that this is a rotation matrix"
  (let* ((trace-a (cl:+ (melm mat-a 0 0) (melm mat-a 1 1)
                        (melm mat-a 2 2)))
         (cos-theta (cl:* 0.5 (cl:- trace-a 1f0)))
         (angle (acos cos-theta)))
    (cond ((cl:= 0f0 angle) (values (v! 1f0 0f0 0f0) angle))
          ((cl:= 0f0 (cl:- rtg-math.base-maths:+pi+ angle))
           (values
            (v3:normalize
             (v! (cl:- (melm mat-a 2 1) (melm mat-a 1 2))
                 (cl:- (melm mat-a 0 2) (melm mat-a 2 0))
                 (cl:- (melm mat-a 1 0) (melm mat-a 0 1))))
            angle))
          (t (labels ((biggest-trace (matr)
                        (let ((x 0))
                          (if (> (melm matr 1 1) (melm matr 0 0))
                              (setf x 1))
                          (if (> (melm matr 2 2) (melm matr x x))
                              (setf x 2))
                          x)))
               (let* ((i (biggest-trace mat-a))
                      (j (mod (cl:+ i 1) 3))
                      (k (mod (cl:+ i 1) 3))
                      (s (sqrt (cl:+ 1f0 (cl:- (melm mat-a i i)
                                               (melm mat-a j j)
                                               (melm mat-a k k)))))
                      (recip (cl:/ 1f0 s)))
                 (values (v! (cl:* 0.5 s)
                             (cl:* recip (aref mat-a (cl:+ i (cl:* 4 j))))
                             (cl:* recip (aref mat-a (cl:+ k (cl:* 4 i)))))
                         angle)))))))


;;----------------------------------------------------------------

(defun + (mat-a mat-b)
  "Adds the 2 matrices component wise and returns the result as
   a new matrix"
  (let ((r (0!)))
    (loop :for i :below 16
       :do (setf (aref r i) (cl:+ (aref mat-a i) (aref mat-b i))))
    r))

;;----------------------------------------------------------------

(defun - (mat-a mat-b)
  "Subtracts the 2 matrices component wise and returns the result
   as a new matrix"
  (let ((r (0!)))
    (loop :for i :below 16
       :do (setf (aref r i) (cl:- (aref mat-a i) (aref mat-b i))))
    r))

;;----------------------------------------------------------------

(defun negate (mat-a)
  "Negates the components of the matrix"
  (let ((r (0!)))
    (loop :for i :below 16 :do (setf (aref r i) (cl:- (aref mat-a i))))
    r))

;;----------------------------------------------------------------

(defun *s (mat-a scalar)
  "Multiplies the components of the matrix by the scalar
   provided"
  (let ((result (0!)))
    (loop :for i :below 16
       :do (setf (aref result i) (cl:* scalar (aref mat-a i))))
    result))

;;----------------------------------------------------------------

(defun *v (mat-a vec)
  (v! (cl:+ (cl:* (x vec) (melm mat-a 0 0))
            (cl:* (y vec) (melm mat-a 0 1))
            (cl:* (z vec) (melm mat-a 0 2))
            (cl:* (w vec) (melm mat-a 0 3)))
      (cl:+ (cl:* (x vec) (melm mat-a 1 0))
            (cl:* (y vec) (melm mat-a 1 1))
            (cl:* (z vec) (melm mat-a 1 2))
            (cl:* (w vec) (melm mat-a 1 3)))
      (cl:+ (cl:* (x vec) (melm mat-a 2 0))
            (cl:* (y vec) (melm mat-a 2 1))
            (cl:* (z vec) (melm mat-a 2 2))
            (cl:* (w vec) (melm mat-a 2 3)))
      (cl:+ (cl:* (x vec) (melm mat-a 3 0))
            (cl:* (y vec) (melm mat-a 3 1))
            (cl:* (z vec) (melm mat-a 3 2))
            (cl:* (w vec) (melm mat-a 3 3)))))

(defun *v3 (mat-a vec)
  "Returns the transform of a matrix"
  (v3:make (cl:+ (cl:* (melm mat-a 0 0) (x vec))
                 (cl:* (melm mat-a 0 1) (y vec))
                 (cl:* (melm mat-a 0 2) (z vec))
                 (melm mat-a 0 3))
           (cl:+ (cl:* (melm mat-a 1 0) (x vec))
                 (cl:* (melm mat-a 1 1) (y vec))
                 (cl:* (melm mat-a 1 2) (z vec))
                 (melm mat-a 1 3))
           (cl:+ (cl:* (melm mat-a 2 0) (x vec))
                 (cl:* (melm mat-a 2 1) (y vec))
                 (cl:* (melm mat-a 2 2) (z vec))
                 (melm mat-a 2 3))))

;;----------------------------------------------------------------

(defun mrow*vec4 (vec mat-a)
  (v! (cl:+ (cl:* (x vec) (melm mat-a 0 0))
            (cl:* (y vec) (melm mat-a 1 0))
            (cl:* (z vec) (melm mat-a 2 0))
            (cl:* (w vec) (melm mat-a 3 0)))

      (cl:+ (cl:* (x vec) (melm mat-a 0 1))
            (cl:* (y vec) (melm mat-a 1 1))
            (cl:* (z vec) (melm mat-a 2 1))
            (cl:* (w vec) (melm mat-a 3 1)))

      (cl:+ (cl:* (x vec) (melm mat-a 0 2))
            (cl:* (y vec) (melm mat-a 1 2))
            (cl:* (z vec) (melm mat-a 2 2))
            (cl:* (w vec) (melm mat-a 3 2)))

      (cl:+ (cl:* (x vec) (melm mat-a 0 3))
            (cl:* (y vec) (melm mat-a 1 3))
            (cl:* (z vec) (melm mat-a 2 3))
            (cl:* (w vec) (melm mat-a 3 3)))))

;;----------------------------------------------------------------

(defun * (&rest matrices)
  (if matrices
      (reduce #'rtg-math.matrix4.destructive:* matrices
              :initial-value (identity))
      (identity)))

(define-compiler-macro * (&whole whole &rest matrices)
  (case= (length matrices)
    (0 `(identity))
    (1 (first matrices))
    (2 `(*m ,@matrices))
    (otherwise whole)))

;;----------------------------------------------------------------

(defun *m (mat-a mat-b)
  "Multiplies 2 matrices and returns the result as a new
   matrix"
  (make (cl:+ (cl:* (melm mat-a 0 0) (melm mat-b 0 0))
              (cl:* (melm mat-a 0 1) (melm mat-b 1 0))
              (cl:* (melm mat-a 0 2) (melm mat-b 2 0))
              (cl:* (melm mat-a 0 3) (melm mat-b 3 0)))
        (cl:+ (cl:* (melm mat-a 0 0) (melm mat-b 0 1))
              (cl:* (melm mat-a 0 1) (melm mat-b 1 1))
              (cl:* (melm mat-a 0 2) (melm mat-b 2 1))
              (cl:* (melm mat-a 0 3) (melm mat-b 3 1)))
        (cl:+ (cl:* (melm mat-a 0 0) (melm mat-b 0 2))
              (cl:* (melm mat-a 0 1) (melm mat-b 1 2))
              (cl:* (melm mat-a 0 2) (melm mat-b 2 2))
              (cl:* (melm mat-a 0 3) (melm mat-b 3 2)))
        (cl:+ (cl:* (melm mat-a 0 0) (melm mat-b 0 3))
              (cl:* (melm mat-a 0 1) (melm mat-b 1 3))
              (cl:* (melm mat-a 0 2) (melm mat-b 2 3))
              (cl:* (melm mat-a 0 3) (melm mat-b 3 3)))
        (cl:+ (cl:* (melm mat-a 1 0) (melm mat-b 0 0))
              (cl:* (melm mat-a 1 1) (melm mat-b 1 0))
              (cl:* (melm mat-a 1 2) (melm mat-b 2 0))
              (cl:* (melm mat-a 1 3) (melm mat-b 3 0)))
        (cl:+ (cl:* (melm mat-a 1 0) (melm mat-b 0 1))
              (cl:* (melm mat-a 1 1) (melm mat-b 1 1))
              (cl:* (melm mat-a 1 2) (melm mat-b 2 1))
              (cl:* (melm mat-a 1 3) (melm mat-b 3 1)))
        (cl:+ (cl:* (melm mat-a 1 0) (melm mat-b 0 2))
              (cl:* (melm mat-a 1 1) (melm mat-b 1 2))
              (cl:* (melm mat-a 1 2) (melm mat-b 2 2))
              (cl:* (melm mat-a 1 3) (melm mat-b 3 2)))
        (cl:+ (cl:* (melm mat-a 1 0) (melm mat-b 0 3))
              (cl:* (melm mat-a 1 1) (melm mat-b 1 3))
              (cl:* (melm mat-a 1 2) (melm mat-b 2 3))
              (cl:* (melm mat-a 1 3) (melm mat-b 3 3)))
        (cl:+ (cl:* (melm mat-a 2 0) (melm mat-b 0 0))
              (cl:* (melm mat-a 2 1) (melm mat-b 1 0))
              (cl:* (melm mat-a 2 2) (melm mat-b 2 0))
              (cl:* (melm mat-a 2 3) (melm mat-b 3 0)))
        (cl:+ (cl:* (melm mat-a 2 0) (melm mat-b 0 1))
              (cl:* (melm mat-a 2 1) (melm mat-b 1 1))
              (cl:* (melm mat-a 2 2) (melm mat-b 2 1))
              (cl:* (melm mat-a 2 3) (melm mat-b 3 1)))
        (cl:+ (cl:* (melm mat-a 2 0) (melm mat-b 0 2))
              (cl:* (melm mat-a 2 1) (melm mat-b 1 2))
              (cl:* (melm mat-a 2 2) (melm mat-b 2 2))
              (cl:* (melm mat-a 2 3) (melm mat-b 3 2)))
        (cl:+ (cl:* (melm mat-a 2 0) (melm mat-b 0 3))
              (cl:* (melm mat-a 2 1) (melm mat-b 1 3))
              (cl:* (melm mat-a 2 2) (melm mat-b 2 3))
              (cl:* (melm mat-a 2 3) (melm mat-b 3 3)))
        (cl:+ (cl:* (melm mat-a 3 0) (melm mat-b 0 0))
              (cl:* (melm mat-a 3 1) (melm mat-b 1 0))
              (cl:* (melm mat-a 3 2) (melm mat-b 2 0))
              (cl:* (melm mat-a 3 3) (melm mat-b 3 0)))
        (cl:+ (cl:* (melm mat-a 3 0) (melm mat-b 0 1))
              (cl:* (melm mat-a 3 1) (melm mat-b 1 1))
              (cl:* (melm mat-a 3 2) (melm mat-b 2 1))
              (cl:* (melm mat-a 3 3) (melm mat-b 3 1)))
        (cl:+ (cl:* (melm mat-a 3 0) (melm mat-b 0 2))
              (cl:* (melm mat-a 3 1) (melm mat-b 1 2))
              (cl:* (melm mat-a 3 2) (melm mat-b 2 2))
              (cl:* (melm mat-a 3 3) (melm mat-b 3 2)))
        (cl:+ (cl:* (melm mat-a 3 0) (melm mat-b 0 3))
              (cl:* (melm mat-a 3 1) (melm mat-b 1 3))
              (cl:* (melm mat-a 3 2) (melm mat-b 2 3))
              (cl:* (melm mat-a 3 3) (melm mat-b 3 3)))))

;;----------------------------------------------------------------


(defun print-m4 (m4)
  (apply #'format t
         "~%(m! ~s ~s ~s ~s~%    ~s ~s ~s ~s~%    ~s ~s ~s ~s~%    ~s ~s ~s ~s)"
         (concatenate 'list (transpose m4)))
  m4)
