(in-package :rtg-math.matrix2)

;;----------------------------------------------------------------

(docs:define-docs
  (defun *
      "
Takes any number of `mat2`s and multiplies them together returning a
new `mat2`.
")
  (defun copy-mat2
      "
Returns a fresh `mat2` which is m2:= to the orignal matrix
")
  (defun mrow*vec2
      "
{TODO}
")
  (DEFUN 0!
      "
Return a `mat2` where all components are zero")
  (DEFUN MAKE
      "
Make a `mat2`. Data must be provided in row major order")
  (DEFUN IDENTITY
      "
Return a `mat2` identity matrix")
  (DEFUN FROM-ROWS
      "
Make a `mat2` using the data in the 2 `vec2`s to populate the rows")
  (DEFUN GET-ROWS
      "
Return the rows of the matrix as 2 `vec2`s")
  (DEFUN GET-ROW
      "
Return the specified row of the matrix as a `vec2`")
  (DEFUN FROM-COLUMNS

      "
Make a `mat2` using the data in the 2 `vec2`s to populate the columns")
  (DEFUN GET-COLUMNS
      "
Return the columns of the matrix as 2 `vec2`s")
  (DEFUN GET-COLUMN
      "
Return the specified column of the matrix as a `vec2`")
  (DEFUN 0P
      "
Returns T if this is a zero matrix

As contents of the matrix are floats the values have an error bound as defined
in base-maths

Note: Old, float epsilons are tricky, I have a ticket open to review this")
  (DEFUN IDENTITYP
      "
Returns T if this is an identity matrix

As contents of the matrix are floats the values have an error bound as defined
in base-maths

Note: Old, float epsilons are tricky, I have a ticket open to review this")
  (DEFUN =
      "
Returns T if all elements of both matrices provided are equal")
  (DEFUN TRANSPOSE
      "
Returns the transpose of the provided matrix")
  (DEFUN ADJOINT
      "
Returns the adjoint of the provided matrix")
  (DEFUN TRACE
      "
Returns the trace of the matrix (That is the diagonal values)")
  (DEFUN ROTATION-FROM-EULER
      "
Creates a rotation `mat2` which represents a anticlockwise rotation.

The angle is specified in radians")
  (DEFUN SCALE
      "
Returns a matrix which will scale by the amounts specified")
  (DEFUN +
      "
Adds the 2 matrices component wise and returns the result as
a new `mat2`")
  (DEFUN -
      "
Subtracts the 2 matrices component wise and returns the result
as a new `mat2`")
  (DEFUN NEGATE
      "
Negates the components of the `mat2`")
  (DEFUN *V
      "
Multiplies the `vec2` by the `mat2` and returns the result
as a new `vec2`")
  (DEFUN %*
      "
Multiplies 2 matrices and returns the result as a new
`mat2`")
  (DEFUN *S
      "
Multiplies the components of the `mat2` by the scalar
provided"))
