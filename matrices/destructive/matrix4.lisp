(in-package :rtg-math.matrix4.destructive)

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
           (type (integer 0 4) row col))
  (aref mat-a (cl:+ row (cl:* col 4))))

(defun (setf melm) (value mat-a row col)
  "Provides access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (declare (mat4 mat-a)
           (type (integer 0 4) row col)
           (single-float value))
  (setf (aref mat-a (cl:+ row (cl:* col 4))) value))

(define-compiler-macro melm (mat-a row col)
  "Provide access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (cond ((and (numberp row) (numberp col))
         `(aref ,mat-a ,(cl:+ row (cl:* col 4))))
        ((numberp col)
         `(aref ,mat-a (cl:+ ,row ,(cl:* col 4))))
        (t `(aref ,mat-a (cl:+ ,row (cl:* ,col 4))))))

;;----------------------------------------------------------------

(defun %* (mat-accum to-multiply-mat)
  "Multiplies 2 matrices and returns the result as a new
   matrix"
  (declare (type mat4 mat-accum to-multiply-mat)
           (optimize (speed 3) (safety 1) (debug 1)))
  (let ((a (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 0))
                 (cl:* (melm mat-accum 0 2) (melm to-multiply-mat 2 0))
                 (cl:* (melm mat-accum 0 3) (melm to-multiply-mat 3 0))))
        (b (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 1))
                 (cl:* (melm mat-accum 0 2) (melm to-multiply-mat 2 1))
                 (cl:* (melm mat-accum 0 3) (melm to-multiply-mat 3 1))))
        (c (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 2))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 2))
                 (cl:* (melm mat-accum 0 2) (melm to-multiply-mat 2 2))
                 (cl:* (melm mat-accum 0 3) (melm to-multiply-mat 3 2))))
        (d (cl:+ (cl:* (melm mat-accum 0 0) (melm to-multiply-mat 0 3))
                 (cl:* (melm mat-accum 0 1) (melm to-multiply-mat 1 3))
                 (cl:* (melm mat-accum 0 2) (melm to-multiply-mat 2 3))
                 (cl:* (melm mat-accum 0 3) (melm to-multiply-mat 3 3))))
        (e (cl:+ (cl:* (melm mat-accum 1 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 0))
                 (cl:* (melm mat-accum 1 2) (melm to-multiply-mat 2 0))
                 (cl:* (melm mat-accum 1 3) (melm to-multiply-mat 3 0))))
        (f (cl:+ (cl:* (melm mat-accum 1 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 1))
                 (cl:* (melm mat-accum 1 2) (melm to-multiply-mat 2 1))
                 (cl:* (melm mat-accum 1 3) (melm to-multiply-mat 3 1))))
        (g (cl:+ (cl:* (melm mat-accum 1 0) (melm to-multiply-mat 0 2))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 2))
                 (cl:* (melm mat-accum 1 2) (melm to-multiply-mat 2 2))
                 (cl:* (melm mat-accum 1 3) (melm to-multiply-mat 3 2))))
        (h (cl:+ (cl:* (melm mat-accum 1 0) (melm to-multiply-mat 0 3))
                 (cl:* (melm mat-accum 1 1) (melm to-multiply-mat 1 3))
                 (cl:* (melm mat-accum 1 2) (melm to-multiply-mat 2 3))
                 (cl:* (melm mat-accum 1 3) (melm to-multiply-mat 3 3))))
        (i (cl:+ (cl:* (melm mat-accum 2 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 2 1) (melm to-multiply-mat 1 0))
                 (cl:* (melm mat-accum 2 2) (melm to-multiply-mat 2 0))
                 (cl:* (melm mat-accum 2 3) (melm to-multiply-mat 3 0))))
        (j (cl:+ (cl:* (melm mat-accum 2 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 2 1) (melm to-multiply-mat 1 1))
                 (cl:* (melm mat-accum 2 2) (melm to-multiply-mat 2 1))
                 (cl:* (melm mat-accum 2 3) (melm to-multiply-mat 3 1))))
        (k (cl:+ (cl:* (melm mat-accum 2 0) (melm to-multiply-mat 0 2))
                 (cl:* (melm mat-accum 2 1) (melm to-multiply-mat 1 2))
                 (cl:* (melm mat-accum 2 2) (melm to-multiply-mat 2 2))
                 (cl:* (melm mat-accum 2 3) (melm to-multiply-mat 3 2))))
        (l (cl:+ (cl:* (melm mat-accum 2 0) (melm to-multiply-mat 0 3))
                 (cl:* (melm mat-accum 2 1) (melm to-multiply-mat 1 3))
                 (cl:* (melm mat-accum 2 2) (melm to-multiply-mat 2 3))
                 (cl:* (melm mat-accum 2 3) (melm to-multiply-mat 3 3))))
        (m (cl:+ (cl:* (melm mat-accum 3 0) (melm to-multiply-mat 0 0))
                 (cl:* (melm mat-accum 3 1) (melm to-multiply-mat 1 0))
                 (cl:* (melm mat-accum 3 2) (melm to-multiply-mat 2 0))
                 (cl:* (melm mat-accum 3 3) (melm to-multiply-mat 3 0))))
        (n (cl:+ (cl:* (melm mat-accum 3 0) (melm to-multiply-mat 0 1))
                 (cl:* (melm mat-accum 3 1) (melm to-multiply-mat 1 1))
                 (cl:* (melm mat-accum 3 2) (melm to-multiply-mat 2 1))
                 (cl:* (melm mat-accum 3 3) (melm to-multiply-mat 3 1))))
        (o (cl:+ (cl:* (melm mat-accum 3 0) (melm to-multiply-mat 0 2))
                 (cl:* (melm mat-accum 3 1) (melm to-multiply-mat 1 2))
                 (cl:* (melm mat-accum 3 2) (melm to-multiply-mat 2 2))
                 (cl:* (melm mat-accum 3 3) (melm to-multiply-mat 3 2))))
        (p (cl:+ (cl:* (melm mat-accum 3 0) (melm to-multiply-mat 0 3))
                 (cl:* (melm mat-accum 3 1) (melm to-multiply-mat 1 3))
                 (cl:* (melm mat-accum 3 2) (melm to-multiply-mat 2 3))
                 (cl:* (melm mat-accum 3 3) (melm to-multiply-mat 3 3)))))
    (setf (melm mat-accum 0 0) a)
    (setf (melm mat-accum 0 1) b)
    (setf (melm mat-accum 0 2) c)
    (setf (melm mat-accum 0 3) d)
    (setf (melm mat-accum 1 0) e)
    (setf (melm mat-accum 1 1) f)
    (setf (melm mat-accum 1 2) g)
    (setf (melm mat-accum 1 3) h)
    (setf (melm mat-accum 2 0) i)
    (setf (melm mat-accum 2 1) j)
    (setf (melm mat-accum 2 2) k)
    (setf (melm mat-accum 2 3) l)
    (setf (melm mat-accum 3 0) m)
    (setf (melm mat-accum 3 1) n)
    (setf (melm mat-accum 3 2) o)
    (setf (melm mat-accum 3 3) p)
    mat-accum))

(defun * (accum-mat &rest mat4s)
  "Add two matrices and returns the mutated matrix (accum-mat)"
  (reduce #'%* mat4s :initial-value accum-mat))

(define-compiler-macro * (&whole whole accum-mat &rest mat4s)
  (assert accum-mat)
  (case= (cl:length mat4s)
    (0 accum-mat)
    (1 `(%* ,accum-mat (first mat4s)))
    (otherwise whole)))

;;----------------------------------------------------------------
