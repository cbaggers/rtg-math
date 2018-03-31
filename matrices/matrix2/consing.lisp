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
            (c single-float) (d single-float)) mat2
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
  (and (cl:= 1f0 (melm mat-a 0 0)) (cl:= 0f0 (melm mat-a 0 1))
       (cl:= 0f0 (melm mat-a 1 0)) (cl:= 1f0 (melm mat-a 1 1))))

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

(defn transpose ((mat-a mat2)) mat2
  "Returns the transpose of the provided matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline copy-mat2))
  (m2-n:transpose (copy-mat2 mat-a)))

;;----------------------------------------------------------------

(defn adjoint ((mat-a mat2)) mat2
  "Returns the adjoint of the provided matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline copy-mat2))
  (m2-n:adjoint (copy-mat2 mat-a)))

;;----------------------------------------------------------------

(defn trace ((mat-a mat2)) single-float
  "Returns the trace of the matrix (That is the diagonal values)"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:+ (melm mat-a 0 0) (melm mat-a 1 1)))

;;----------------------------------------------------------------

(defn rotation-from-euler ((angle single-float)) mat2
  "Returns the transpose of the provided matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (m2-n:set-rotation-from-euler (0!) angle))

;;----------------------------------------------------------------

(defn scale ((scale-vec2 vec2)) mat2
  "Returns a matrix which will scale by the amounts specified"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline 0!))
  (m2-n:set-from-scale (0!) scale-vec2))

;;----------------------------------------------------------------

(defn + ((mat-a mat2) (mat-b mat2)) mat2
  "Adds the 2 matrices component wise and returns the result as
   a new matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:+ (aref mat-a 0) (aref mat-b 0))
        (cl:+ (aref mat-a 1) (aref mat-b 1))
        (cl:+ (aref mat-a 2) (aref mat-b 2))
        (cl:+ (aref mat-a 3) (aref mat-b 3))))

;;----------------------------------------------------------------

(defn - ((mat-a mat2) (mat-b mat2)) mat2
  "Subtracts the 2 matrices component wise and returns the result
   as a new matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:- (aref mat-a 0) (aref mat-b 0))
        (cl:- (aref mat-a 1) (aref mat-b 1))
        (cl:- (aref mat-a 2) (aref mat-b 2))
        (cl:- (aref mat-a 3) (aref mat-b 3))))

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

;; (a b   (e f
;;  c d)   h i)
;;
;; ae+bc  af+bi
;; ce+dc  cf+di
(defn %* ((mat-a mat2) (mat-b mat2)) mat2
  "Multiplies 2 matrices and returns the result as a new
   matrix"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (make (cl:+ (cl:* (melm mat-a 0 0) (melm mat-b 0 0))
              (cl:* (melm mat-a 0 1) (melm mat-b 1 0)))
        (cl:+ (cl:* (melm mat-a 0 0) (melm mat-b 0 1))
              (cl:* (melm mat-a 0 1) (melm mat-b 1 1)))
        (cl:+ (cl:* (melm mat-a 1 0) (melm mat-b 0 0))
              (cl:* (melm mat-a 1 1) (melm mat-b 1 0)))
        (cl:+ (cl:* (melm mat-a 1 0) (melm mat-b 0 1))
              (cl:* (melm mat-a 1 1) (melm mat-b 1 1)))))

(defn * (&rest (matrices mat2)) mat2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (if matrices
      (reduce #'rtg-math.matrix2.non-consing:* matrices
              :initial-value (identity))
      (identity)))

(define-compiler-macro * (&whole whole &rest matrices)
  (case= (length matrices)
    (0 `(identity))
    (1 (first matrices))
    (2 `(%* ,@matrices))
    (otherwise whole)))

;;----------------------------------------------------------------

(defn *s ((mat-a mat2) (scalar single-float)) mat2
  "Multiplies the components of the matrix by the scalar
   provided"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (inline copy-mat2))
  (m2-n:*s (copy-mat2 mat-a) scalar))

;;----------------------------------------------------------------
