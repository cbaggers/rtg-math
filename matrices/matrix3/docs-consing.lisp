(in-package :rtg-math.matrix3)

;;----------------------------------------------------------------

(docs:define-docs
  (defun *
      "
Takes any number of `mat3`s and multiplies them together returning a
new `mat3`.
")
  (defun copy-mat3
      "
Returns a fresh `mat3` which is m3:= to the orignal matrix
")
  (defun mrow*vec3
      "
{TODO}
")
  (defun from-direction
      "
Creates a `mat3` that would rotate the vector (v! 0 0 -1) to point in the given
direction.
")
  (defun look-at
      "
Creates a `mat3` that would rotate the vector (v! 0 0 -1) to point in the
direction from `from3` to `to3`
")
  (defun point-at
      "
Create the `mat3` rotation portion to a 'world to view' 'look-at' matrix
")
  (DEFUN MAKE
      "
Make a `mat3`. Data must be provided in row major order")
  (DEFUN IDENTITY
      "
Return a `mat3` identity matrix")
  (DEFUN FROM-ROWS
      "
Make a `mat3` using the data in the 3 `vec3`s to populate the rows")
  (DEFUN GET-ROWS
      "
Return the rows of the matrix as 3 `vec3`s")
  (DEFUN GET-ROW
      "
Return the specified row of the matrix as a `vec3`")
  (DEFUN FROM-COLUMNS

      "
Make a `mat3` using the data in the 3 `vec3`s to populate the columns")
  (DEFUN GET-COLUMNS
      "
Return the columns of the matrix as 3 `vec3`s")
  (DEFUN GET-COLUMN
      "
Return the specified column of the matrix as a `vec3`")
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
Creates a rotation `mat3` which represents a anticlockwise rotation.

The angle is specified in radians")
  (DEFUN SCALE
      "
Returns a matrix which will scale by the amounts specified")
  (DEFUN +
      "
Adds the 2 matrices component wise and returns the result as
a new `mat3`")
  (DEFUN -
      "
Subtracts the 2 matrices component wise and returns the result
as a new `mat3`")
  (DEFUN NEGATE
      "
Negates the components of the `mat3`")
  (DEFUN *V
      "
Multiplies the `vec3` by the `mat3` and returns the result
as a new `vec3`")
  (DEFUN *S
      "
Multiplies the components of the `mat3` by the scalar
provided")
  (DEFUN ROTATION-X
      "
Returns a `mat3` which would rotate a point around the x axis
by the specified amount")
  (DEFUN ROTATION-Y
      "
Returns a `mat3` which would rotate a point around the y axis
by the specified amount")
  (DEFUN ROTATION-Z
      "
Returns a `mat3` which would rotate a point around the z axis
by the specified amount")
  (DEFUN ROTATION-FROM-AXIS-ANGLE
      "
Returns a `mat3` which will rotate a point about the axis
specified by the angle provided")
  (DEFUN GET-FIXED-ANGLES
      "
Gets one set of possible z-y-x fixed angles that will generate
this `mat3`.

Assumes that this is a rotation matrix. It is critical that this
is true (and elements are between -1f0 and 1f0) as otherwise you will
at best get a runtime error, and most likely a silently incorrect result.")
  (DEFUN GET-AXIS-ANGLE
      "
Gets one possible axis-angle pair that will generate this
`mat3`.

Assumes that this is a rotation matrix. It is critical that this
is true (and elements are between -1f0 and 1f0) as otherwise you will
at best get a runtime error, and most likely a silently incorrect result.")
  (DEFUN AFFINE-INVERSE
      "
returns the inverse of the `mat3`")
  (DEFUN DETERMINANT
      "
Returns the determinant of the `mat3` (uses the cramer method)"))
