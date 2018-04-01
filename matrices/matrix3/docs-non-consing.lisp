(in-package :rtg-math.matrix3.non-consing)

;;----------------------------------------------------------------

(docs:define-docs
  (defun *
      "
Takes any number of `mat3`s and multiplies them together
destructively writing the result into the first `mat3`.
")
  (defun mrow*vec3
      "
{TODO}
")
  (defun set-rotation-from-euler
      "
Destructively modifies the `mat3` to be a rotation matrix
whos rotation angles come from the `vec3` provided.
")
  (defun set-components
      "
Destructively updates of the components of the given `mat3` to the new
`single-float`s provided.
")
  (DEFUN SET-FROM-ROWS
      "
Destructively updates of the components of the given `mat3` using
the 3 `vec3`s provided to populate the rows")
  (DEFUN SET-FROM-COLUMNS
      "
Destructively updates of the components of the given `mat3` using
the 3 `vec3`s provided to populate the columns")
  (DEFUN TRANSPOSE
      "
Mutates the given `mat3` to be it's own transpose")
  (DEFUN ADJOINT
      "
Mutates the given `mat3` to be it's own adjoint")
  (DEFUN SET-FROM-SCALE
      "
Mutates the given `mat3` to be a matrix which will scale by the amounts
specified")
  (DEFUN +
      "
Add the second `mat3` component wise to the first and return
the first")
  (DEFUN -
      "
Subtracts the second `mat3` component wise from the first and return
the first")
  (DEFUN NEGATE
      "
Negates the components of the `mat3`")
  (DEFUN *V
      "
Destructively multiplies the `vec3` by the `mat3` and returning the
mutated `vec3`")
  (DEFUN AFFINE-INVERSE
      "
Mutates the given `mat3` to be it's own inverse")
  (DEFUN SET-FROM-ROTATION-X
      "
Destructively updates of the components of the given `mat3` making
it a matrix which would rotate a point around the x axis by the
specified amount")
  (DEFUN SET-FROM-ROTATION-Y
      "
Destructively updates of the components of the given `mat3` making
it a matrix which would rotate a point around the y axis by the
specified amount")
  (DEFUN SET-FROM-ROTATION-Z
      "
Destructively updates of the components of the given `mat3` making
it a matrix which would rotate a point around the z axis by the
specified amount")
  (DEFUN SET-ROTATION-FROM-AXIS-ANGLE
      "
Destructively updates of the components of the given `mat3` making
it a matrix which will rotate a point about the axis specified by
the angle provided")
  (DEFUN *S "Multiplies the components of the `mat4` by the scalar
   provided"))
