(in-package :rtg-math.matrix4.non-consing)

;;----------------------------------------------------------------

(docs:define-docs
  (defun mrow*vec4
      "
{TODO}
")
  (defun *
      "
Takes any number of `mat4`s and multiplies them together
destructively writing the result into the first `mat4`.
")
  (defun set-rotation-from-euler
      "
Destructively modifies the `mat4` to be a rotation matrix
whos rotation angles come from the `vec3` provided.
")
  (defun set-components
      "
Destructively updates of the components of the given `mat4` to the new
`single-float`s provided.
")
  (DEFUN SET-FROM-ROWS
      "
Destructively updates of the components of the given `mat4` using
the 4 `vec4`s provided to populate the rows")
  (DEFUN SET-FROM-COLUMNS
      "
Destructively updates of the components of the given `mat4` using
the 4 `vec4`s provided to populate the columns")
  (DEFUN TRANSPOSE
      "
Mutates the given `mat4` to be it's own transpose")
  (DEFUN ADJOINT
      "
Mutates the given `mat4` to be it's own adjoint")
  (DEFUN SET-FROM-SCALE
      "
Mutates the given `mat4` to be a matrix which will scale by the amounts
specified")
  (DEFUN +
      "
Add the second `mat4` component wise to the first and return
the first")
  (DEFUN -
      "
Subtracts the second `mat4` component wise from the first and return
the first")
  (DEFUN NEGATE
      "
Negates the components of the `mat4`")
  (DEFUN *V
      "
Destructively multiplies the `vec4` by the `mat4` and returning the
mutated `vec4`")
  (DEFUN AFFINE-INVERSE
      "
Mutates the given `mat4` to be it's own inverse")
  (DEFUN SET-FROM-ROTATION-X
      "
Destructively updates of the components of the given `mat4` making
it a matrix which would rotate a point around the x axis by the
specified amount")
  (DEFUN SET-FROM-ROTATION-Y
      "
Destructively updates of the components of the given `mat4` making
it a matrix which would rotate a point around the y axis by the
specified amount")
  (DEFUN SET-FROM-ROTATION-Z
      "
Destructively updates of the components of the given `mat4` making
it a matrix which would rotate a point around the z axis by the
specified amount")
  (DEFUN SET-ROTATION-FROM-AXIS-ANGLE
      "
Destructively updates of the components of the given `mat4` making
it a matrix which will rotate a point about the axis specified by
the angle provided")

  (DEFUN *S
      "
Destructively performs component-wise multiplication of the `vec3` by
the scalar")
  (DEFUN *V
      "
Destructively multiplies the `vec3` by the top left 3x3 portion of the `mat4`
returning the mutated `vec3`")
  (DEFUN SET-FROM-MAT3
      "
Destructively updates of the components of the given `mat4` making
it's top left 3x3 portion the same as the `mat3` provided.

The 4th row & column is filled as an identity matrix would be.")
  (DEFUN SET-FROM-ROWS-V3
      "
Make a `mat4` using the data in the 3 `vec3` provided to populate the rows

The 4th row & column is filled as an identity matrix would be.")
  (DEFUN SET-FROM-COLUMNS-V3
      "
Make a `mat4` using the data in the 3 `vec3` provided to populate the columns

The 4th row & column is filled as an identity matrix would be.")
  (DEFUN SET-FROM-TRANSLATION
      "
Destructively updates of the components of the given `mat4` making
it a matrix which which will translate a point by the specified amount"))
