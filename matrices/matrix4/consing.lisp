(in-package :rtg-math.matrix4)

;;----------------------------------------------------------------

(defn identity () mat4
  "Return a 4x4 identity matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make-array 16 :element-type 'single-float
              :initial-contents '(1f0 0f0 0f0 0f0
                                  0f0 1f0 0f0 0f0
                                  0f0 0f0 1f0 0f0
                                  0f0 0f0 0f0 1f0)))

(defn-inline 0! () mat4
  "Return a 4x4 zero matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make-array 16 :element-type 'single-float :initial-element 0f0))

;;----------------------------------------------------------------

(defn make ((a single-float) (b single-float) (c single-float) (d single-float)
            (e single-float) (f single-float) (g single-float) (h single-float)
            (i single-float) (j single-float) (k single-float) (l single-float)
            (m single-float) (n single-float) (o single-float) (p single-float)) mat4
  "Make a 4x4 matrix. Data must be provided in row major order"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline m4-n:set-components 0!))
  ;; as you can see it is stored in column major order
  (m4-n:set-components a b c d e f g h i j k l m n o p (0!)))

;;----------------------------------------------------------------

(defn-inline copy-mat4 ((mat4 mat4)) mat4
  (declare (optimize (speed 3) (safety 0) (debug 1)))
  (let ((result (make-array 16 :element-type 'single-float)))
    (dotimes (i 16)
      (setf (aref result i) (aref mat4 i)))
    result))

;;----------------------------------------------------------------

(defn to-mat3 ((mat4 mat4)) mat3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (m3:make (melm mat4 0 0) (melm mat4 0 1) (melm mat4 0 2)
           (melm mat4 1 0) (melm mat4 1 1) (melm mat4 1 2)
           (melm mat4 2 0) (melm mat4 2 1) (melm mat4 2 2)))

;;----------------------------------------------------------------

(defn from-mat3 ((m-a mat3)) mat4
  "Takes a 3x3 matrix and returns a 4x4 rotation matrix
   with the same values. The 4th component is filled as an
   identity matrix would be."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (m3:melm m-a 0 0)  (m3:melm m-a 0 1)  (m3:melm m-a 0 2)  0f0
        (m3:melm m-a 1 0)  (m3:melm m-a 1 1)  (m3:melm m-a 1 2)  0f0
        (m3:melm m-a 2 0)  (m3:melm m-a 2 1)  (m3:melm m-a 2 2)  0f0
        0f0                0f0                0f0                1f0))

;;----------------------------------------------------------------

(defn from-rows ((row-1 vec4) (row-2 vec4) (row-3 vec4) (row-4 vec4)) mat4
  "Make a 4x4 matrix using the data in the 4 vector4s provided
   to populate the rows"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (x row-1) (y row-1) (z row-1) (w row-1)
        (x row-2) (y row-2) (z row-2) (w row-2)
        (x row-3) (y row-3) (z row-3) (w row-3)
        (x row-4) (y row-4) (z row-4) (w row-4)))

(defn from-rows-v3 ((row-1 vec3) (row-2 vec3) (row-3 vec3)) mat4
  "Make a 4x4 matrix using the data in the 4 vector4s provided
   to populate the rows"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (x row-1) (y row-1) (z row-1) 0f0
        (x row-2) (y row-2) (z row-2) 0f0
        (x row-3) (y row-3) (z row-3) 0f0
        0f0       0f0       0f0       1f0))

;;----------------------------------------------------------------

(defn get-rows ((mat-a mat4)) list
  "Return the rows of the matrix as 4 vector4s"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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

(defn get-row ((mat-a mat4) (row-num (integer 0 3))) vec4
  "Return the specified row of the matrix a vector4"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v4:make (melm mat-a row-num 0)
           (melm mat-a row-num 1)
           (melm mat-a row-num 2)
           (melm mat-a row-num 3)))


;;----------------------------------------------------------------

(defn from-columns ((col-1 vec4) (col-2 vec4) (col-3 vec4) (col-4 vec4)) mat4
  "Make a 4x4 matrix using the data in the 4 vector4s provided
   to populate the columns"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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

(defn from-columns-v3 ((col-1 vec3) (col-2 vec3) (col-3 vec3)) mat4
  "Make a 4x4 matrix using the data in the 3 vector3s provided
   to populate the columns"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (x col-1)
        (x col-2)
        (x col-3)
        0f0
        (y col-1)
        (y col-2)
        (y col-3)
        0f0
        (z col-1)
        (z col-2)
        (z col-3)
        0f0
        0f0
        0f0
        0f0
        1f0))

;;----------------------------------------------------------------

(defn get-columns ((mat-a mat4)) list
  "Return the columns of the matrix as 4 vector4s"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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

(defn get-column ((mat-a mat4) (col-num (integer 0 3))) vec4
  "Return the specified column of the matrix a vector4"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v4:make (melm mat-a 0 col-num)
           (melm mat-a 1 col-num)
           (melm mat-a 2 col-num)
           (melm mat-a 3 col-num)))

;;----------------------------------------------------------------

(defn 0p ((mat-a mat4)) boolean
  "Returns 't' if this is a zero matrix (as contents of the
   matrix are floats the values have an error bound as defined
   in base-maths"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for i :below 16 :always (cl:= 0f0 (aref mat-a i))))

;;----------------------------------------------------------------

(defn identityp ((mat-a mat4)) boolean
  "Returns 't' if this is an identity matrix (as contents of the
   matrix are floats the values have an error bound as defined
   in base-maths"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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

(defn = ((mat-a mat4) (mat-b mat4)) boolean
  "Returns t if all elements of both matrices provided are
   equal"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (cl:= (aref mat-a 0) (aref mat-b 0))
       (cl:= (aref mat-a 1) (aref mat-b 1))
       (cl:= (aref mat-a 2) (aref mat-b 2))
       (cl:= (aref mat-a 3) (aref mat-b 3))
       (cl:= (aref mat-a 4) (aref mat-b 4))
       (cl:= (aref mat-a 5) (aref mat-b 5))
       (cl:= (aref mat-a 6) (aref mat-b 6))
       (cl:= (aref mat-a 7) (aref mat-b 7))
       (cl:= (aref mat-a 8) (aref mat-b 8))
       (cl:= (aref mat-a 9) (aref mat-b 9))
       (cl:= (aref mat-a 10) (aref mat-b 10))
       (cl:= (aref mat-a 11) (aref mat-b 11))
       (cl:= (aref mat-a 12) (aref mat-b 12))
       (cl:= (aref mat-a 13) (aref mat-b 13))
       (cl:= (aref mat-a 14) (aref mat-b 14))
       (cl:= (aref mat-a 15) (aref mat-b 15))))

;;----------------------------------------------------------------

(defn adjoint ((mat-a mat4)) mat4
  "Returns the adjoint of the matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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

(defn determinant ((mat-a mat4)) single-float
  "Returns the determinant of the matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (cl:* (melm mat-a 0 0) (minor mat-a 1 2 3 1 2 3))
        (cl:- (cl:* (melm mat-a 0 1) (minor mat-a 1 2 3 0 2 3)))
        (cl:* (melm mat-a 0 2) (minor mat-a 1 2 3 0 1 3))
        (cl:- (cl:* (melm mat-a 0 3) (minor mat-a 1 2 3 0 1 2)))))

;;----------------------------------------------------------------

(defn inverse ((matrix mat4)) mat4
  ;;(declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((det (m4:determinant matrix)))
    (if (cl:= det 0f0)
        (error "Cannot invert matrix with zero determinant:~%  ~S"
               matrix)
        (macrolet ((a (x y z)
                     (declare (optimize (safety 3) (speed 0)))
                     (multiple-value-bind (r1 c1) (truncate (cl:- x 11) 10)
                       (multiple-value-bind (r2 c2) (truncate (cl:- y 11) 10)
                         (multiple-value-bind (r3 c3) (truncate (cl:- z 11) 10)
                           `(cl:* (melm matrix ,r1 ,c1)
                                  (melm matrix ,r2 ,c2)
                                  (melm matrix ,r3 ,c3)))))))
          (let ((m
                 (make
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
(defn affine-inverse ((mat-a mat4)) mat4
  "Returns the affine inverse of the matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline copy-mat4))
  (m4-n:affine-inverse (copy-mat4 mat-a)))

;;----------------------------------------------------------------
;; {TODO} could just feed straight from array into make

(defn transpose ((m-a mat4)) mat4
  "Returns the transpose of the provided matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline copy-mat4))
  (m4-n:transpose (copy-mat4 m-a)))

;;----------------------------------------------------------------

(defn translation ((vec-a (simple-array single-float))) mat4
  "Takes a vector3 and returns a matrix4 which will translate
   by the specified amount"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (m4-n:set-from-translation (0!) vec-a))

;;----------------------------------------------------------------

(defn rotation-from-mat3 ((m-a mat3)) mat4
  "Takes a 3x3 rotation matrix and returns a 4x4 rotation matrix
   with the same values. The 4th component is filled as an
   identity matrix would be."
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (m4-n:set-from-mat3 (0!) m-a))

;;----------------------------------------------------------------

(defn rotation-from-euler ((vec3-a vec3)) mat4
  "This is an unrolled contatenation of rotation matrices x y & z."
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (m4-n:set-rotation-from-euler (0!) vec3-a))

;;----------------------------------------------------------------

(defn scale ((scale-vec3 vec3)) mat4
  "Returns a matrix which will scale by the amounts specified"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (m4-n:set-from-scale (0!) scale-vec3))

;;----------------------------------------------------------------

(defn rotation-x ((angle single-float)) mat4
  "Returns a matrix which would rotate a point around the x axis
   by the specified amount"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (m4-n:set-from-rotation-x (0!) angle))

;;----------------------------------------------------------------

(defn rotation-y ((angle single-float)) mat4
  "Returns a matrix which would rotate a point around the y axis
   by the specified amount"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (m4-n:set-from-rotation-y (0!) angle))

;;----------------------------------------------------------------

(defn rotation-z ((angle single-float)) mat4
  "Returns a matrix which would rotate a point around the z axis
   by the specified amount"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (m4-n:set-from-rotation-z (0!) angle))

;;----------------------------------------------------------------

(defn rotation-from-axis-angle ((axis3 vec3) (angle single-float)) mat4
  "Returns a matrix which will rotate a point about the axis
   specified by the angle provided"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (m4-n:set-rotation-from-axis-angle (0!) axis3 angle))

;;----------------------------------------------------------------

;; [TODO] returned as vector x-y-z

(defn get-fixed-angles ((mat-a mat4)) vec3
  "Gets one set of possible z-y-x fixed angles that will generate
   this matrix. Result is returned as vector3

   Assumes that this is a rotation matrix. It is critical that this
   is true (and elements are between -1f0 and 1f0) as otherwise you will
   at best get a runtime error, and most likely a silently incorrect result."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((sy (melm mat-a 0 2)))
    (declare ((single-float -1f0 1f0) sy))
    (let ((cy (sqrt (cl:- 1f0 (cl:* sy sy)))))
      (if (cl:= 0f0 cy)
          (let ((sz 0f0)
                (cz 1f0)
                (sx (melm mat-a 2 1))
                (cx (melm mat-a 1 1)))
            (v3:make (atan sx cx) (atan sy cy) (atan sz cz)))
          (let* ((factor (cl:/ 1f0 cy)) ; normal case
                 (sx (cl:- (cl:* factor (melm mat-a 1 2))))
                 (cx (cl:* factor (melm mat-a 2 2)))
                 (sz (cl:- (cl:* factor (melm mat-a 0 1))))
                 (cz (cl:* factor (melm mat-a 0 0))))
            (v3:make (atan sx cx) (atan sy cy) (atan sz cz)))))))

;;----------------------------------------------------------------

(defn trace ((mat-a mat4)) single-float
  "Returns the trace of the matrix (That is the diagonal values)"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (melm mat-a 0 0)
        (melm mat-a 1 1)
        (melm mat-a 2 2)
        (melm mat-a 3 3)))

;;----------------------------------------------------------------

;; [TODO] find out how we can declaim angle to be float
;; [TODO] Comment the fuck out of this and work out how it works

(defn get-axis-angle ((mat-a mat4)) vec3
  "Gets one possible axis-angle pair that will generate this
   matrix. Assumes that this is a rotation matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((trace-a (cl:+ (melm mat-a 0 0) (melm mat-a 1 1)
                        (melm mat-a 2 2)))
         (cos-theta (cl:* 0.5 (cl:- trace-a 1f0))))
    (declare (type (single-float -1f0 1f0) cos-theta))
    (let ((angle (acos cos-theta)))
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
                        (tmp-s (cl:+ 1f0 (cl:- (melm mat-a i i)
                                               (melm mat-a j j)
                                               (melm mat-a k k))))
                        (s (the (single-float 0f0 #.most-positive-single-float)
                                tmp-s))
                        (recip (cl:/ 1f0 s)))
                   (values (v! (cl:* 0.5 s)
                               (cl:* recip (aref mat-a (cl:+ i (cl:* 4 j))))
                               (cl:* recip (aref mat-a (cl:+ k (cl:* 4 i)))))
                           angle))))))))


;;----------------------------------------------------------------

(defn + ((mat-a mat4) (mat-b mat4)) mat4
  "Adds the 2 matrices component wise and returns the result as
   a new matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (let ((r (0!)))
    (loop :for i :below 16
       :do (setf (aref r i) (cl:+ (aref mat-a i) (aref mat-b i))))
    r))

;;----------------------------------------------------------------

(defn - ((mat-a mat4) (mat-b mat4)) mat4
  "Subtracts the 2 matrices component wise and returns the result
   as a new matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (let ((r (0!)))
    (loop :for i :below 16
       :do (setf (aref r i) (cl:- (aref mat-a i) (aref mat-b i))))
    r))

;;----------------------------------------------------------------

(defn negate ((mat-a mat4)) mat4
  "Negates the components of the matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (let ((r (0!)))
    (loop :for i :below 16 :do (setf (aref r i) (cl:- (aref mat-a i))))
    r))

;;----------------------------------------------------------------

(defn *s ((mat-a mat4) (scalar single-float)) mat4
  "Multiplies the components of the matrix by the scalar
   provided"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (let ((result (0!)))
    (loop :for i :below 16
       :do (setf (aref result i) (cl:* scalar (aref mat-a i))))
    result))

;;----------------------------------------------------------------

(defn *v ((mat-a mat4) (vec4 vec4)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (m4-n:*v mat-a (v4:copy-vec4 vec4)))

(defn *v3 ((mat-a mat4) (vec3 vec3)) vec3
  "Returns the transform of a matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (m4-n:*v3 mat-a (v3:copy-vec3 vec3)))

;;----------------------------------------------------------------

(defn mrow*vec4 ((vec vec4) (mat-a mat4)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (m4-n:mrow*vec4 (v4:copy-vec4 vec) mat-a))

;;----------------------------------------------------------------

(defn %* ((mat-a mat4) (mat-b mat4)) mat4
  "Multiplies 2 matrices and returns the result as a new
   matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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

(defn * (&rest (matrices mat4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if matrices
      (reduce #'rtg-math.matrix4.non-consing:* matrices
              :initial-value (identity))
      (identity)))

(define-compiler-macro * (&whole whole &rest matrices)
  (case= (length matrices)
    (0 `(identity))
    (1 (first matrices))
    (2 `(%* ,@matrices))
    (otherwise whole)))

;;----------------------------------------------------------------

#-ecl
(defn print-m4 ((m4 mat4)) mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (format t "~%(m! ~s ~s ~s ~s~%    ~s ~s ~s ~s~%    ~s ~s ~s ~s~%    ~s ~s ~s ~s)"
          (melm m4 0 0) (melm m4 0 1) (melm m4 0 2) (melm m4 0 3)
          (melm m4 1 0) (melm m4 1 1) (melm m4 1 2) (melm m4 1 3)
          (melm m4 2 0) (melm m4 2 1) (melm m4 2 2) (melm m4 2 3)
          (melm m4 3 0) (melm m4 3 1) (melm m4 3 2) (melm m4 3 3))
  m4)

#+ecl
(DEFUN PRINT-M4 (M4)
  (FORMAT T
          "~%(m! ~s ~s ~s ~s~%    ~s ~s ~s ~s~%    ~s ~s ~s ~s~%    ~s ~s ~s ~s)"
          (MELM M4 0 0) (MELM M4 0 1) (MELM M4 0 2) (MELM M4 0 3)
          (MELM M4 1 0) (MELM M4 1 1) (MELM M4 1 2) (MELM M4 1 3)
          (MELM M4 2 0) (MELM M4 2 1) (MELM M4 2 2) (MELM M4 2 3)
          (MELM M4 3 0) (MELM M4 3 1) (MELM M4 3 2) (MELM M4 3 3))
  M4)
