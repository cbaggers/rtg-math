(in-package :rtg-math.matrix2.non-consing)

;;----------------------------------------------------------------

(docs:define-docs
  (defun *
      "
Takes any number of `mat2`s and multiplies them together
destructively writing the result into the first `mat2`.
")
  (defun mrow*vec2
      "
{TODO}
")
  (defun set-rotation-from-euler
      "
Destructively modifies the `mat2` to be a rotation matrix
whos rotation angle comes from the single-float provided.
")
  (defun set-components
      "
Destructively updates of the components of the given `mat2` to the new
`single-float`s provided.
"))

(DEFUN SET-FROM-ROWS
    "
Destructively updates of the components of the given `mat2` using
the 2 `vec2`s provided to populate the rows")
(DEFUN SET-FROM-COLUMNS
    "
Destructively updates of the components of the given `mat2` using
the 2 `vec2`s provided to populate the columns")
(DEFUN TRANSPOSE
    "
Mutates the given `mat2` to be it's own transpose")
(DEFUN ADJOINT
    "
Mutates the given `mat2` to be it's own adjoint")
(DEFUN SET-FROM-SCALE
    "
Mutates the given `mat2` to be a which will scale by the amounts specified")
(DEFUN +
    "
Add the second `mat2` component wise to the first and return
the first")
(DEFUN -
    "
Subtracts the second `mat2` component wise from the first and return
the first")
(DEFUN NEGATE
    "
Negates the components of the `mat2`")
(DEFUN *V
    "
Destructively multiplies the `vec3` by the `mat2` and returning the
mutated `vec3`")
