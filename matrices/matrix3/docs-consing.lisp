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
"))
