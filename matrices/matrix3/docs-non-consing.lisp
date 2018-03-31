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
"))
