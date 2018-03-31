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
"))
