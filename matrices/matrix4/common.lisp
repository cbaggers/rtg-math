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

(defn minor ((mat-a mat4) (row-0 (integer 0 3)) (row-1 (integer 0 3))
             (row-2 (integer 0 3)) (col-0 (integer 0 3)) (col-1 (integer 0 3))
             (col-2 (integer 0 3))) single-float
  (declare (optimize (speed 3) (safety 1) (debug 1)))
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
