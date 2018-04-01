(in-package :rtg-math.matrix4)

;;----------------------------------------------------------------

(docs:define-docs
  (defun *
      "
Takes any number of `mat4`s and multiplies them together returning a
new `mat4`.
")
  (defun copy-mat4
      "
Returns a fresh `mat4` which is m4:= to the orignal matrix
")
  (defun mrow*vec4
      "
{TODO}
")
  (defun from-direction
      "
Creates a `mat4` that would rotate the vector (v! 0 0 -1) to face in the given
direction.
")
  (defun look-at
      "
Creates a `mat4` that would rotate the vector (v! 0 0 -1) to point in the
direction from `from3` to `to3`
")
  (defun point-at
      "
Create the `mat4` rotation portion to a 'world to view' 'look-at' matrix
")
  (defun *v
      "
Transforms the `vec4` by the `mat4` returning a new `vec4`
")
  (defun *v3
      "
Transforms the `vec3` by the `mat4` returning a new `vec3`
")

  (defun inverse
      "
Inverts the matrix.
")
  (defun minor
      "
Returns the minor of the matrix
")
  (defun to-mat3
      "
Returns a fresh `mat3` which contains the 'top left' 3x3 portion of the
given `mat4`
")

  (DEFUN 0!
      "
Return a `mat4` where every component is 0")
  (DEFUN MAKE
      "
Make a `mat4`. Data must be provided in row major order")
  (DEFUN IDENTITY
      "
Return a `mat4` identity matrix")
  (DEFUN FROM-ROWS
      "
Make a `mat4` using the data in the 4 `vec4`s to populate the rows")
  (DEFUN GET-ROWS
      "
Return the rows of the matrix as 4 `vec4`s")
  (DEFUN GET-ROW
      "
Return the specified row of the matrix as a `vec4`")
  (DEFUN FROM-COLUMNS

      "
Make a `mat4` using the data in the 4 `vec4`s to populate the columns")
  (DEFUN GET-COLUMNS
      "
Return the columns of the matrix as 4 `vec4`s")
  (DEFUN GET-COLUMN
      "
Return the specified column of the matrix as a `vec4`")
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
Creates a rotation `mat4` which represents a anticlockwise rotation.

The angle is specified in radians")
  (DEFUN SCALE
      "
Returns a matrix which will scale by the amounts specified")
  (DEFUN +
      "
Adds the 2 matrices component wise and returns the result as
a new `mat4`")
  (DEFUN -
      "
Subtracts the 2 matrices component wise and returns the result
as a new `mat4`")
  (DEFUN NEGATE
      "
Negates the components of the `mat4`")
  (DEFUN *V
      "
Multiplies the `vec4` by the `mat4` and returns the result
as a new `vec4`")
  (DEFUN *S
      "
Multiplies the components of the `mat4` by the scalar
provided")
  (DEFUN ROTATION-X
      "
Returns a `mat4` which would rotate a point around the x axis
by the specified amount")
  (DEFUN ROTATION-Y
      "
Returns a `mat4` which would rotate a point around the y axis
by the specified amount")
  (DEFUN ROTATION-Z
      "
Returns a `mat4` which would rotate a point around the z axis
by the specified amount")
  (DEFUN ROTATION-FROM-AXIS-ANGLE
      "
Returns a `mat4` which will rotate a point about the axis
specified by the angle provided")
  (DEFUN GET-FIXED-ANGLES
      "
Gets one set of possible z-y-x fixed angles that will generate
this `mat4`.

Assumes that this is a rotation matrix. It is critical that this
is true (and elements are between -1f0 and 1f0) as otherwise you will
at best get a runtime error, and most likely a silently incorrect result.")
  (DEFUN GET-AXIS-ANGLE
      "
Gets one possible axis-angle pair that will generate this
`mat4`.

Assumes that this is a rotation matrix. It is critical that this
is true (and elements are between -1f0 and 1f0) as otherwise you will
at best get a runtime error, and most likely a silently incorrect result.")
  (DEFUN AFFINE-INVERSE
      "
returns the inverse of the `mat4`")
  (DEFUN DETERMINANT
      "
Returns the determinant of the `mat4`")
  (DEFUN FROM-MAT3
      "
Takes a `mat3` and returns a `mat4` rotation matrix with the same values.

The 4th row & column are filled as an identity matrix would be.")
  (DEFUN FROM-ROWS-V3
      "
Make a `mat4` using the data in the 3 `vec3`s provided to populate the rows.

The 4th row & column are filled as an identity matrix would be.")
  (DEFUN FROM-COLUMNS-V3
      "
Make a `mat4` using the data in the 3 `vec3`s provided to populate the
columns.

The 4th row & column are filled as an identity matrix would be.")
  (DEFUN ROTATION-FROM-MAT3
      "
Takes a `mat3` rotation matrix and returns a `mat4` rotation matrix
with the same values. The 4th row & column are filled as an identity matrix
would be.")
  (DEFUN TRANSLATION
      "
Takes a `vec3` and returns a `mat4` which will translate by the specified
amount")
  (DEFUN *V3
      "
Multiplies the `vec3` by the `mat3` and returns the result as a new `vec3`"))
