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
"))
