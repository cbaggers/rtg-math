(in-package #:rtg-math.vector2.non-consing)

;;----------------------------------------------------------------

(defn-inline set-components ((x single-float) (y single-float) (vec vec2)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x vec) x
        (y vec) y)
  vec)

;;----------------------------------------------------------------

(defn +s ((vec2 vec2) (scalar single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:incf (x vec2) scalar)
  (cl:incf (y vec2) scalar)
  vec2)

;;----------------------------------------------------------------

(defn -s ((vec2 vec2) (scalar single-float)) vec2
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:decf (x vec2) scalar)
  (cl:decf (y vec2) scalar)
  vec2)

;;---------------------------------------------------------------

(defn %+ ((accum-vec vec2) (to-add-vec vec2)) vec2
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:incf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:incf (aref accum-vec 1) (aref to-add-vec 1))
  accum-vec)

(defn + ((accum-vec vec2) &rest (vec2s vec2)) vec2
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for vec :in vec2s :do (%+ accum-vec vec))
  accum-vec)

(define-compiler-macro + (&whole whole accum-vec &rest vec2s)
  (assert accum-vec)
  (case= (cl:length vec2s)
    (0 accum-vec)
    (1 `(%+ ,accum-vec ,(first vec2s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(defn %- ((accum-vec vec2) (to-add-vec vec2)) vec2
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:decf (aref accum-vec 0) (aref to-add-vec 0))
  (cl:decf (aref accum-vec 1) (aref to-add-vec 1))
  accum-vec)

(defn - ((accum-vec vec2) &rest (vec2s vec2)) vec2
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for vec :in vec2s :do (%- accum-vec vec))
  accum-vec)

(define-compiler-macro - (&whole whole accum-vec &rest vec2s)
  (assert accum-vec)
  (case= (cl:length vec2s)
    (0 accum-vec)
    (1 `(%- ,accum-vec ,(first vec2s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(defn %* ((accum-vec vec2) (to-mult-vec vec2)) vec2
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (cl:setf (aref accum-vec 0) (cl:* (aref accum-vec 0) (aref to-mult-vec 0)))
  (cl:setf (aref accum-vec 1) (cl:* (aref accum-vec 1) (aref to-mult-vec 1)))
  accum-vec)

(defn * ((accum-vec vec2) &rest (vec2s vec2)) vec2
  "Add two vectors and return a new vector containing the result"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (loop :for vec :in vec2s :do (%* accum-vec vec))
  accum-vec)

(define-compiler-macro * (&whole whole accum-vec &rest vec2s)
  (assert accum-vec)
  (case= (cl:length vec2s)
    (0 accum-vec)
    (1 `(%* ,accum-vec ,(first vec2s)))
    (otherwise whole)))

;;---------------------------------------------------------------

(defn *s ((vec2 vec2) (a single-float)) vec2
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x vec2) (cl:* (x vec2) a))
  (setf (y vec2) (cl:* (y vec2) a))
  vec2)

;;---------------------------------------------------------------

(defn /s ((vec2 vec2) (a single-float)) vec2
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x vec2) (cl:/ (x vec2) a))
  (setf (y vec2) (cl:/ (y vec2) a))
  vec2)

;;---------------------------------------------------------------

(defn / ((vec2-a vec2) (vec2-b vec2)) vec2
  "Multiply vector by scalar"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (x vec2-a) (cl:/ (x vec2-a) (x vec2-b)))
  (setf (y vec2-a) (cl:/ (y vec2-a) (y vec2-b)))
  vec2-a)

;;---------------------------------------------------------------

(defn negate ((vector-a vec2)) vec2
  "Return a vector that is the negative of the vector passed in"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (set-components (cl:- (x vector-a))
                  (cl:- (y vector-a))
                  vector-a))

;;---------------------------------------------------------------

(defn normalize ((vector-a vec2)) vec2
  "This normalizes the vector, it makes sure a zero length
   vector won't throw an error."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((x (x vector-a))
         (y (y vector-a))
         (len-sqr (cl:+ (cl:* x x) (cl:* y y))))
    (if (cl:= 0f0 len-sqr)
        vector-a
        (*s vector-a (inv-sqrt len-sqr)))))

;;---------------------------------------------------------------
