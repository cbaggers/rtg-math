(in-package :rtg-math.matrix2)

;; All matrices are stored in column-major format, but when you
;; write them (like in m!) then you write them in row-major format.

;;----------------------------------------------------------------

(defn-inline 0! () mat2
  "Return a 2x2 zero matrix"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (make-array 4 :element-type `single-float :initial-element 0.0))

;;----------------------------------------------------------------

(defn make ((a single-float) (b single-float)
            (c single-float) (d single-float))
    mat2
  "Make a 2x2 matrix. Data must be provided in row major order"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (m2-n:set-components a b c d (0!)))

;;----------------------------------------------------------------

(defn-inline copy-mat2 ((mat2 mat2)) mat2
  (declare (optimize (speed 3) (safety 0) (debug 1)))
  (let ((result (make-array 4 :element-type 'single-float)))
    (setf (aref result 0) (aref mat2 0))
    (setf (aref result 1) (aref mat2 1))
    (setf (aref result 2) (aref mat2 2))
    (setf (aref result 3) (aref mat2 3))
    result))

;;----------------------------------------------------------------

(defn identity () mat2
  "Return a 2x2 identity matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make
   1.0 0.0
   0.0 1.0))

;;----------------------------------------------------------------

(defn from-rows ((row-1 vec2) (row-2 vec2)) mat2
  "Make a 2x2 matrix using the data in the 2 vector2s provided
   to populate the rows"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (x row-1) (y row-1)
        (x row-2) (y row-2)))

;;----------------------------------------------------------------

(defn get-rows ((mat-a mat2)) list
  "Return the rows of the matrix a 2 vector2s"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (list (v2:make (melm mat-a 0 0)
                 (melm mat-a 0 1))
        (v2:make (melm mat-a 1 0)
                 (melm mat-a 1 1))))

;;----------------------------------------------------------------

(defn get-row ((mat-a mat2) (row-num (integer 0 1))) vec2
  "Return the specified row of the matrix a vector2"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v2:make (melm mat-a row-num 0)
           (melm mat-a row-num 1)))

;;----------------------------------------------------------------

(defn from-columns ((col-1 vec2) (col-2 vec2)) mat2
  "Make a 2x2 matrix using the data in the 2 vector2s provided
   to populate the columns"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (x col-1) (x col-2)
        (y col-1) (y col-2)))

;;----------------------------------------------------------------


(defn get-columns ((mat-a mat2)) list
  "Return the columns of the matrix as 2 vector2s"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (list (v! (melm mat-a 0 0)
            (melm mat-a 1 0))
        (v! (melm mat-a 0 1)
            (melm mat-a 1 1))))

;;----------------------------------------------------------------

(defn get-column ((mat-a mat2) (col-num (integer 0 1))) vec2
  "Return the specified column of the matrix a vector2"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (v2:make (melm mat-a 0 col-num)
           (melm mat-a 1 col-num)))

;;----------------------------------------------------------------

(defn 0p ((mat-a mat2)) boolean
  "Returns 't' if this is a zero matrix (as contents of the
   matrix are floats the values have an error bound as defined
   in base-maths"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (cl:= 0f0 (aref mat-a 0))
       (cl:= 0f0 (aref mat-a 1))
       (cl:= 0f0 (aref mat-a 2))
       (cl:= 0f0 (aref mat-a 3))))

;;----------------------------------------------------------------

(defn identityp ((mat-a mat2)) boolean
  "Returns 't' if this is an identity matrix (as contents of the
   matrix are floats the values have an error bound as defined
   in base-maths"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (cl:= 0f0 (cl:- (melm mat-a 0 0) 1.0))
       (cl:= 0f0 (cl:- (melm mat-a 1 1) 1.0))
       (cl:= 0f0 (melm mat-a 0 1))
       (cl:= 0f0 (melm mat-a 1 0))))

;;----------------------------------------------------------------

(defn = ((mat-a mat2) (mat-b mat2)) boolean
  "Returns t if all elements of both matrices provided are
   equal"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (and (cl:= (aref mat-a 0) (aref mat-b 0))
       (cl:= (aref mat-a 1) (aref mat-b 1))
       (cl:= (aref mat-a 2) (aref mat-b 2))
       (cl:= (aref mat-a 3) (aref mat-b 3))))

;;----------------------------------------------------------------

;; (defn determinant ((mat-a mat3)) single-float
;;   "Returns the determinant of the matrix (uses the cramer method)"
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (let* ((cofactor-0 (cl:- (cl:* (melm mat-a 1 1) (melm mat-a 2 2))
;;                            (cl:* (melm mat-a 2 1) (melm mat-a 1 2))))

;;          (cofactor-3 (cl:- (cl:* (melm mat-a 2 0) (melm mat-a 1 2))
;;                            (cl:* (melm mat-a 1 0) (melm mat-a 2 2))))

;;          (cofactor-6 (cl:- (cl:* (melm mat-a 1 0) (melm mat-a 2 1))
;;                            (cl:* (melm mat-a 2 0) (melm mat-a 1 1)))))
;;     (cl:+ (cl:* (melm mat-a 0 0) cofactor-0)
;;           (cl:* (melm mat-a 0 1) cofactor-3)
;;           (cl:* (melm mat-a 0 2) cofactor-6))))

;;----------------------------------------------------------------

;; (defn affine-inverse ((mat-a mat3)) mat3
;;   "returns the inverse of the matrix"
;;   (declare (optimize (speed 3) (safety 1) (debug 1))
;;            (inline copy-mat3))
;;   (m3-n:affine-inverse (copy-mat3 mat-a)))

;;----------------------------------------------------------------

(defn transpose ((mat-a mat2)) mat2
  "Returns the transpose of the provided matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline copy-mat2))
  (m2-n:transpose (copy-mat2 mat-a)))

;;----------------------------------------------------------------

;; (defn adjoint ((mat-a mat2)) mat2
;;   "Returns the adjoint of the provided matrix"
;;   (declare (optimize (speed 3) (safety 1) (debug 1))
;;            (inline copy-mat2))
;;   (m2-n:adjoint (copy-mat2 mat-a)))

;;----------------------------------------------------------------

(defn trace ((mat-a mat2)) single-float
  "Returns the trace of the matrix (That is the diagonal values)"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (melm mat-a 0 0) (melm mat-a 1 1)))

;;----------------------------------------------------------------

;; (defn rotation-from-euler ((vec2-a vec2)) mat2
;;   "Returns the transpose of the provided matrix"
;;   (declare (optimize (speed 3) (safety 1) (debug 1))
;;            (inline 0!))
;;   (m2-n:set-rotation-from-euler (0!) vec2-a))

;;----------------------------------------------------------------

(defn scale ((scale-vec2 vec2)) mat2
  "Returns a matrix which will scale by the amounts specified"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (m2-n:set-from-scale (0!) scale-vec2))

;;----------------------------------------------------------------

;; (defn rotation-x ((angle single-float)) mat2
;;   "Returns a matrix which would rotate a point around the x axis
;;    by the specified amount"
;;   (declare (optimize (speed 3) (safety 1) (debug 1))
;;            (inline 0!))
;;   (m2-n:set-from-rotation-x (0!) angle))

;;----------------------------------------------------------------

;; (defn rotation-y ((angle single-float)) mat2
;;   "Returns a matrix which would rotate a point around the y axis
;;    by the specified amount"
;;   (declare (optimize (speed 3) (safety 1) (debug 1))
;;            (inline 0!))
;;   (m2-n:set-from-rotation-y (0!) angle))

;;----------------------------------------------------------------

;; (defn get-fixed-angles ((mat-a mat3)) vec3
;;   "Gets one set of possible z-y-x fixed angles that will generate
;;    this matrix.

;;    Assumes that this is a rotation matrix. It is critical that this
;;    is true (and elements are between -1f0 and 1f0) as otherwise you will
;;    at best get a runtime error, and most likely a silently incorrect result."
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (let* ((sy (melm mat-a 0 2)))
;;     (declare ((single-float -1f0 1f0) sy))
;;     (let ((cy (sqrt (cl:- 1f0 (cl:* sy sy)))))
;;       (declare (single-float cy))
;;       (if (not (cl:= 0f0 cy)) ; [TODO: not correct PI-epsilon]
;;           (let* ((factor (cl:/ 1.0 cy))
;;                  (sx (cl:* factor (cl:- (melm mat-a 2 1))))
;;                  (cx (cl:* factor (melm mat-a 2 2)))
;;                  (sz (cl:* factor (cl:- (melm mat-a 1 0))))
;;                  (cz (cl:* factor (melm mat-a 0 0))))
;;             (v3:make (atan sx cx) (atan sy cy) (atan sz cz)))
;;           (let* ((sz 0.0)
;;                  (cx 1.0)
;;                  (sx (melm mat-a 1 2))
;;                  (cz (melm mat-a 1 1)))
;;             (v3:make (atan sx cx) (atan sy cy) (atan sz cz)))))))

;;----------------------------------------------------------------

(defn + (&rest (mat2s mat2)) mat2
  "takes any number of vectors and add them all together
   returning a new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if mat2s
      (let ((x 0f0)
            (y 0f0)
            (z 0f0)
            (w 0f0))
        (declare (single-float x y z w))
        (loop :for vec :in mat2s :do
           (let ((vec vec))
             (declare (mat2 vec))
             (cl:incf x (x vec))
             (cl:incf y (y vec))
             (cl:incf z (z vec))
             (cl:incf w (w vec))))
        (make x y z w))
      (make 0f0 0f0 0f0 0f0)))

(define-compiler-macro + (&whole whole &rest mat2s)
  (case= (cl:length mat2s)
    (0 (make 0f0 0f0 0f0 0f0))
    (1 (first mat2s))
    (2 `(%+ ,@mat2s))
    (otherwise whole)))

(defn %+ ((vector-a mat2) (vector-b mat2)) mat2
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:+ (aref vector-a 0) (aref vector-b 0))
        (cl:+ (aref vector-a 1) (aref vector-b 1))
        (cl:+ (aref vector-a 2) (aref vector-b 2))
        (cl:+ (aref vector-a 3) (aref vector-b 3))))

;;----------------------------------------------------------------

(defn - ((mat2 mat2) &rest (mat2s mat2)) mat2
  "takes any number of vectors and add them all together
   returning a new vector"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (assert mat2)
  (if mat2s
      (let ((x (x mat2))
            (y (y mat2))
            (z (z mat2))
            (w (w mat2)))
        (declare (single-float x y z w))
        (loop :for vec :in mat2s :do
           (let ((vec vec))
             (declare (mat2 vec))
             (cl:decf x (x vec))
             (cl:decf y (y vec))
             (cl:decf z (z vec))
             (cl:decf w (w vec))))
        (make x y z w))
      mat2))

(define-compiler-macro - (&whole whole &rest mat2s)
  (case= (cl:length mat2s)
    (2 `(%- ,@mat2s))
    (otherwise whole)))

(defn %- ((vector-a mat2) (vector-b mat2)) mat2
  "Subtract two vectors and return a new vector containing
   the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (aref vector-a 0) (aref vector-b 0))
        (cl:- (aref vector-a 1) (aref vector-b 1))
        (cl:- (aref vector-a 2) (aref vector-b 2))
        (cl:- (aref vector-a 3) (aref vector-b 3))))

;;----------------------------------------------------------------

(defn negate ((mat-a mat2)) mat2
  "Negates the components of the matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline copy-mat2))
  (m2-n:negate (copy-mat2 mat-a)))

;;----------------------------------------------------------------

(defn *v ((mat-a mat2) (vec-a vec2)) vec2
  "Multiplies the vector2 by the matrix and returns the result
   as a new vector2"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (m2-n:*v mat-a (v2:copy-vec2 vec-a)))

;;----------------------------------------------------------------

(defn mrow*vec2 ((vec vec2) (mat-a mat2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (m2-n:mrow*vec2 (v2:copy-vec2 vec) mat-a))

;;----------------------------------------------------------------

;; (defn %* ((mat-a mat3) (mat-b mat3)) mat3
;;   "Multiplies 2 matrices and returns the result as a new
;;    matrix"
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (make (cl:+ (cl:* (melm mat-a 0 0) (melm mat-b 0 0))
;;               (cl:* (melm mat-a 0 1) (melm mat-b 1 0))
;;               (cl:* (melm mat-a 0 2) (melm mat-b 2 0)))
;;         (cl:+ (cl:* (melm mat-a 1 0) (melm mat-b 0 0))
;;               (cl:* (melm mat-a 1 1) (melm mat-b 1 0))
;;               (cl:* (melm mat-a 1 2) (melm mat-b 2 0)))
;;         (cl:+ (cl:* (melm mat-a 2 0) (melm mat-b 0 0))
;;               (cl:* (melm mat-a 2 1) (melm mat-b 1 0))
;;               (cl:* (melm mat-a 2 2) (melm mat-b 2 0)))
;;         (cl:+ (cl:* (melm mat-a 0 0) (melm mat-b 0 1))
;;               (cl:* (melm mat-a 0 1) (melm mat-b 1 1))
;;               (cl:* (melm mat-a 0 2) (melm mat-b 2 1)))
;;         (cl:+ (cl:* (melm mat-a 1 0) (melm mat-b 0 1))
;;               (cl:* (melm mat-a 1 1) (melm mat-b 1 1))
;;               (cl:* (melm mat-a 1 2) (melm mat-b 2 1)))
;;         (cl:+ (cl:* (melm mat-a 2 0) (melm mat-b 0 1))
;;               (cl:* (melm mat-a 2 1) (melm mat-b 1 1))
;;               (cl:* (melm mat-a 2 2) (melm mat-b 2 1)))
;;         (cl:+ (cl:* (melm mat-a 0 0) (melm mat-b 0 2))
;;               (cl:* (melm mat-a 0 1) (melm mat-b 1 2))
;;               (cl:* (melm mat-a 0 2) (melm mat-b 2 2)))
;;         (cl:+ (cl:* (melm mat-a 0 1) (melm mat-b 0 2))
;;               (cl:* (melm mat-a 1 1) (melm mat-b 1 2))
;;               (cl:* (melm mat-a 1 2) (melm mat-b 2 2)))
;;         (cl:+ (cl:* (melm mat-a 2 0) (melm mat-b 0 2))
;;               (cl:* (melm mat-a 2 1) (melm mat-b 1 2))
;;               (cl:* (melm mat-a 2 2) (melm mat-b 2 2)))))

;; (defn * (&rest (matrices mat3)) mat3
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (if matrices
;;       (reduce #'rtg-math.matrix2.non-consing:* matrices
;;               :initial-value (identity))
;;       (identity)))

;; (define-compiler-macro * (&whole whole &rest matrices)
;;   (case= (length matrices)
;;     (0 `(identity))
;;     (1 (first matrices))
;;     (2 `(%* ,@matrices))
;;     (otherwise whole)))

;;----------------------------------------------------------------

(defn *s ((mat-a mat2) (scalar single-float)) mat2
  "Multiplies the components of the matrix by the scalar
   provided"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline copy-mat2))
  (m2-n:*s (copy-mat2 mat-a) scalar))

;;----------------------------------------------------------------
;; makes a matrix to orient something towards a point

;; (defn-inline from-direction ((up3 vec3) (dir3 vec3)) mat3
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (let* ((dir (v3:normalize dir3))
;;          (w-up (v3:normalize up3))
;;          (right (v3:cross dir w-up))
;;          (up (v3:cross (v3:normalize right) dir)))
;;     (m3:make (v:x right) (v:x up) (cl:- (v:x dir))
;;              (v:y right) (v:y up) (cl:- (v:y dir))
;;              (v:z right) (v:z up) (cl:- (v:z dir)))))

;;----------------------------------------------------------------
;; makes a matrix to orient something at a point towards another point

;; (defn point-at ((up vec3) (from3 vec3) (to3 vec3)) mat3
;;   (declare (optimize (speed 3) (safety 1) (debug 1))
;;            (inline from-direction))
;;   (from-direction up (v3:- to3 from3)))

;;----------------------------------------------------------------
;; Makes the rotation portion of a world->view look-at matrix

;; (defn-inline look-at ((up3 vec3) (from3 vec3) (to3 vec3)) mat3
;;   (declare (optimize (speed 3) (safety 1) (debug 1)))
;;   (let* ((dir (v3:normalize (v3:- to3 from3)))
;;          (w-up (v3:normalize up3))
;;          (right (v3:cross dir w-up))
;;          (up (v3:cross (v3:normalize right) dir)))
;;     (m3:make (v:x right)       (v:y right)       (v:z right)
;;              (v:x up)          (v:y up)          (v:z up)
;;              (cl:- (v:x dir))  (cl:- (v:y dir))  (cl:- (v:z dir)))))

;;----------------------------------------------------------------
