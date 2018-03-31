(in-package :rtg-math.matrices)

(docs:define-docs
  (defun *
      "
Takes any number of matrices (of the same kind) and multiplies them together
returning a new matrix of the same kind.
")
  (defun +
      "
Takes any number of matrices (of the same kind) and performs component-wise
addition them together returning a new matrix of the same kind.
")
  (defun -
      "
Takes any number of matrices (of the same kind) and performs component-wise
negation of them returning a new matrix of the same kind.
")
  (defun 0p
      "
Returns T if all the components of the matrix `=` 0
")
  (defun 1+
      "
Returns a new matrix which is equal to the given matrix with every component
incremented by 1
")
  (defun 1-
      "
Returns a new matrix which is equal to the given matrix with every component
decremented by 1
")
  (defun affine-inverse
      "
Returns a new matrix which is the inverse of the one given.
")
  (defun determinant
      "
Returns the determinant of the given matrix
")
  (defun elm
      "
DEPRECATED: Please use melm
")
  (defun elt
      "
DEPRECATED: Please use melm
")
  (defun melm
      "
Returns the specific component from the matrix
")
  (defun get-column
      "
Returns the specifed column of the matrix as vector
")
  (defun get-columns
      "
Returns the columns of the matrix as a list of vectors
")
  (defun get-row
      "
Returns the specifed row of the matrix as vector
")
  (defun get-rows
      "
Returns the rows of the matrix as a list of vectors
")
  (defun inverse
      "
Returns the inverse of the given matrix
")
  (defun negate
      "
Returns a fresh matrix with all the components negated
")
  (defun to-string
      "
{TODO}
")
  (defun trace
      "
Returns the trace of the given matrix as a vector
")
  (defun transpose
      "
Returns the transpose of the given matrix
")
  (defun unitp
      "
Returns T if the matrix is the unit matrix.
"))
