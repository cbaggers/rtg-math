(in-package :%rtg-math.matrix4.common)

;;----------------------------------------------------------------

(defn melm ((mat-a mat4) (row (integer 0 3)) (col (integer 0 3))) single-float
  "Provides access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (declare (mat4 mat-a)
           (type (integer 0 4) row col))
  (aref mat-a (cl:+ row (cl:* col 4))))

(defn (setf melm) ((value single-float)
                   (mat-a mat4) (row (integer 0 3)) (col (integer 0 3)))
    single-float
  "Provides access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (setf (aref mat-a (cl:+ row (cl:* col 4))) value))

(define-compiler-macro melm (mat-a row col)
  "Provide access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (cond ((and (typep row '(integer 0 3))
              (typep col '(integer 0 3)))
         `(aref ,mat-a ,(cl:+ row (cl:* col 4))))
        ((typep col '(integer 0 3))
         `(aref ,mat-a (cl:+ ,row ,(cl:* col 4))))
        (t `(aref ,mat-a (cl:+ ,row (cl:* ,col 4))))))
