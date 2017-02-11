(in-package #:rtg-math.vector3.non-consing)

;;----------------------------------------------------------------

(defn-inline set-components ((x single-float) (y single-float) (z single-float)
                             (vec vec3)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x vec) x
        (y vec) y
        (z vec) z)
  vec)

;;----------------------------------------------------------------

(defn +s ((vec3 vec3) (scalar single-float)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:incf (x vec3) scalar)
  (cl:incf (y vec3) scalar)
  (cl:incf (z vec3) scalar)
  vec3)

;;----------------------------------------------------------------

(defn -s ((vec3 vec3) (scalar single-float)) vec3
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:decf (x vec3) scalar)
  (cl:decf (y vec3) scalar)
  (cl:decf (z vec3) scalar)
  vec3)

;;---------------------------------------------------------------

(defn %+ ((accum-vec vec3) (to-add-vec vec3)) vec3
  "Add two vectors and return a new vector containing the result"
  (declare (vec3 accum-vec to-add-vec))
  (cl:incf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:incf (aref accum-vec 1) (aref to-add-vec 1))
  (cl:incf (aref accum-vec 2) (aref to-add-vec 2))
  accum-vec)

(defn + ((accum-vec vec3) &rest (vec3s vec3)) vec3
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for vec :in vec3s :do (%+ accum-vec vec))
  accum-vec)

(define-compiler-macro + (&whole whole accum-vec &rest vec3s)
  (assert accum-vec)
  (case= (cl:length vec3s)
    (0 accum-vec)
    (1 `(%+ ,accum-vec ,(first vec3s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(defn %- ((accum-vec vec3) (to-add-vec vec3)) vec3
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:decf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:decf (aref accum-vec 1) (aref to-add-vec 1))
  (cl:decf (aref accum-vec 2) (aref to-add-vec 2))
  accum-vec)

(defn - ((accum-vec vec3) &rest (vec3s vec3)) vec3
  "Add two vectors and return a new vector containing the result"
  (loop :for vec :in vec3s :do (%- accum-vec vec))
  accum-vec)

(define-compiler-macro - (&whole whole accum-vec &rest vec3s)
  (assert accum-vec)
  (case= (cl:length vec3s)
    (0 accum-vec)
    (1 `(%- ,accum-vec ,(first vec3s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(defn %* ((accum-vec vec3) (to-mult-vec vec3)) vec3
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:setf (aref accum-vec 0) (cl:* (aref accum-vec 0) (aref to-mult-vec 0)))
  (cl:setf (aref accum-vec 1) (cl:* (aref accum-vec 1) (aref to-mult-vec 1)))
  (cl:setf (aref accum-vec 2) (cl:* (aref accum-vec 2) (aref to-mult-vec 2)))
  accum-vec)

(defn * ((accum-vec vec3) &rest (vec3s vec3)) vec3
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for vec :in vec3s :do (%* accum-vec vec))
  accum-vec)

(define-compiler-macro * (&whole whole accum-vec &rest vec3s)
  (assert accum-vec)
  (case= (cl:length vec3s)
    (0 accum-vec)
    (1 `(%* ,accum-vec ,(first vec3s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(defn *s ((vec3 vec3) (a single-float)) vec3
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x vec3) (cl:* (x vec3) a))
  (setf (y vec3) (cl:* (y vec3) a))
  (setf (z vec3) (cl:* (z vec3) a))
  vec3)

;;---------------------------------------------------------------

(defn /s ((vec3 vec3) (a single-float)) vec3
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x vec3) (cl:/ (x vec3) a))
  (setf (y vec3) (cl:/ (y vec3) a))
  (setf (z vec3) (cl:/ (z vec3) a))
  vec3)

;;---------------------------------------------------------------

(defn / ((vec3-a vec3) (vec3-b vec3)) vec3
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x vec3-a) (cl:/ (x vec3-a) (x vec3-b)))
  (setf (y vec3-a) (cl:/ (y vec3-a) (y vec3-b)))
  (setf (z vec3-a) (cl:/ (z vec3-a) (z vec3-b)))
  vec3-a)

;;---------------------------------------------------------------

(defn negate ((vector-a vec3)) vec3
  "Return a vector that is the negative of the vector passed in"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (cl:- (x vector-a))
                  (cl:- (y vector-a))
                  (cl:- (z vector-a))
                  vector-a))

;;---------------------------------------------------------------

(defn normalize ((vector-a vec3)) vec3
  "This normalizes the vector, it makes sure a zero length
   vector won't throw an error."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((x (x vector-a))
         (y (y vector-a))
         (z (z vector-a))
         (len-sqr (cl:+ (cl:* x x) (cl:* y y) (cl:* z z))))
    (if (cl:= 0f0 len-sqr)
        vector-a
        (*s vector-a (inv-sqrt len-sqr)))))

;;---------------------------------------------------------------
