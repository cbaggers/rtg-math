(in-package :%rtg-math.matrix3.common)

;;----------------------------------------------------------------

(defn melm ((mat-a mat3) (row (integer 0 2)) (col (integer 0 2))) single-float
  "Provides access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (aref mat-a (cl:+ row (cl:* col 3))))

(defn (setf melm) ((value single-float) (mat-a mat3)
                   (row (integer 0 2)) (col (integer 0 2))) single-float
  "Provides access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (declare (mat3 mat-a)
           (type (integer 0 3) row col)
           (single-float value))
  (setf (aref mat-a (cl:+ row (cl:* col 3))) value))

(define-compiler-macro melm (mat-a row col)
  "Provide access to data in the matrix by row
   and column number. The actual data is stored in a 1d list in
   column major order, but this abstraction means we only have
   to think in row major order which is how most mathematical
   texts and online tutorials choose to show matrices"
  (cond ((and (numberp row) (numberp col))
         `(aref ,mat-a ,(cl:+ row (cl:* col 3))))
        ((numberp col)
         `(aref ,mat-a (cl:+ ,row ,(cl:* col 3))))
        (t `(aref ,mat-a (cl:+ ,row (cl:* ,col 3))))))

;;----------------------------------------------------------------
