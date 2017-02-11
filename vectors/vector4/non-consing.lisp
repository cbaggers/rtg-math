(in-package #:rtg-math.vector4.non-consing)

;;----------------------------------------------------------------

(defn-inline set-components ((x single-float) (y single-float)
                             (z single-float) (w single-float)
                             (vec vec4)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x vec) x
        (y vec) y
        (z vec) z
        (w vec) w)
  vec)

;;----------------------------------------------------------------

(defn +s ((vec4 vec4) (scalar single-float)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:incf (x vec4) scalar)
  (cl:incf (y vec4) scalar)
  (cl:incf (z vec4) scalar)
  (cl:incf (w vec4) scalar)
  vec4)

;;----------------------------------------------------------------

(defn -s ((vec4 vec4) (scalar single-float)) vec4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:decf (x vec4) scalar)
  (cl:decf (y vec4) scalar)
  (cl:decf (z vec4) scalar)
  (cl:decf (w vec4) scalar)
  vec4)

;;---------------------------------------------------------------

(defn %+ ((accum-vec vec4) (to-add-vec vec4)) vec4
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:incf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:incf (aref accum-vec 1) (aref to-add-vec 1))
  (cl:incf (aref accum-vec 2) (aref to-add-vec 2))
  (cl:incf (aref accum-vec 3) (aref to-add-vec 3))
  accum-vec)

(defn + ((accum-vec vec4) &rest (vec4s vec4)) vec4
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for vec :in vec4s :do (%+ accum-vec vec))
  accum-vec)

(define-compiler-macro + (&whole whole accum-vec &rest vec4s)
  (assert accum-vec)
  (case= (cl:length vec4s)
    (0 accum-vec)
    (1 `(%+ ,accum-vec ,(first vec4s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(defn %- ((accum-vec vec4) (to-add-vec vec4)) vec4
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:decf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:decf (aref accum-vec 1) (aref to-add-vec 1))
  (cl:decf (aref accum-vec 2) (aref to-add-vec 2))
  (cl:decf (aref accum-vec 2) (aref to-add-vec 3))
  accum-vec)

(defn - ((accum-vec vec4) &rest (vec4s vec4)) vec4
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for vec :in vec4s :do (%- accum-vec vec))
  accum-vec)

(define-compiler-macro - (&whole whole accum-vec &rest vec4s)
  (assert accum-vec)
  (case= (cl:length vec4s)
    (0 accum-vec)
    (1 `(%- ,accum-vec ,(first vec4s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(defn %* ((accum-vec vec4) (to-mult-vec vec4)) vec4
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:setf (aref accum-vec 0) (cl:* (aref accum-vec 0) (aref to-mult-vec 0)))
  (cl:setf (aref accum-vec 1) (cl:* (aref accum-vec 1) (aref to-mult-vec 1)))
  (cl:setf (aref accum-vec 2) (cl:* (aref accum-vec 2) (aref to-mult-vec 2)))
  (cl:setf (aref accum-vec 3) (cl:* (aref accum-vec 2) (aref to-mult-vec 3)))
  accum-vec)

(defn * ((accum-vec vec4) &rest (vec4s vec4)) vec4
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for vec :in vec4s :do (%* accum-vec vec))
  accum-vec)

(define-compiler-macro * (&whole whole accum-vec &rest vec4s)
  (assert accum-vec)
  (case= (cl:length vec4s)
    (0 accum-vec)
    (1 `(%* ,accum-vec ,(first vec4s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(defn *s ((vec4 vec4) (a single-float)) vec4
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x vec4) (cl:* (x vec4) a))
  (setf (y vec4) (cl:* (y vec4) a))
  (setf (z vec4) (cl:* (z vec4) a))
  (setf (w vec4) (cl:* (w vec4) a))
  vec4)

;;---------------------------------------------------------------

(defn /s ((vec4 vec4) (a single-float)) vec4
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x vec4) (cl:/ (x vec4) a))
  (setf (y vec4) (cl:/ (y vec4) a))
  (setf (z vec4) (cl:/ (z vec4) a))
  (setf (w vec4) (cl:/ (w vec4) a))
  vec4)

;;---------------------------------------------------------------

(defn / ((vec4-a vec4) (vec4-b vec4)) vec4
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x vec4-a) (cl:/ (x vec4-a) (x vec4-b)))
  (setf (y vec4-a) (cl:/ (y vec4-a) (y vec4-b)))
  (setf (z vec4-a) (cl:/ (z vec4-a) (z vec4-b)))
  (setf (w vec4-a) (cl:/ (w vec4-a) (w vec4-b)))
  vec4-a)

;;---------------------------------------------------------------

(defn negate ((vector-a vec4)) vec4
  "Return a vector that is the negative of the vector passed in"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (cl:- (x vector-a))
                  (cl:- (y vector-a))
                  (cl:- (z vector-a))
                  (cl:- (w vector-a))
                  vector-a))

;;---------------------------------------------------------------

(defn normalize ((vector-a vec4)) vec4
  "This normalizes the vector, it makes sure a zero length
   vector won't throw an error."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((x (x vector-a))
         (y (y vector-a))
         (z (z vector-a))
         (w (w vector-a))
         (len-sqr (cl:+ (cl:* x x) (cl:* y y) (cl:* z z) (cl:* w w))))
    (if (cl:= 0f0 len-sqr)
        vector-a
        (*s vector-a (inv-sqrt len-sqr)))))

;;---------------------------------------------------------------
