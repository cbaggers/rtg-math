(in-package :rtg-math.vector3)

;;----------------------------------------------------------------

(docs:define-docs
  (defun +s
      "
Component-wise add of single-float to `vec3`
")
  (defun -s
      "
Component-wise subtraction of single-float from `vec3`
")
  (defun bezier
      "
{TODO}
")
  (defun copy-vec3
      "
Returns a fresh `vec3` which is v3:= to the orignal vector
")
  (defun face-foreward
      "
Returns `vector-a` if `(> (v3:dot vector-a vector-b) 0)` else returns
`(v3:negate vector-a)`
")
  (defun lerp
      "
Linearly interpolates between the two `vec3`s by the `single-float` amount
")
  (defun make
      "
Make a `vec3` from 3 `single-float`s
")
  (defun rotate
      "
Creates a new `vec3` which is equivilent to the original once rotated
in each axis by the radians in the `rotation` vec3 provided.
")
  (defun spline
      "
{TODO}
")
  (defmacro decf
      "
Decrements the `vec3` in 'place' by another `vec3`
")
  (defmacro incf
      "
Increments the `vec3` in 'place' by another `vec3`
"))
