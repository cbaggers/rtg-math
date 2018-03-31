(in-package :rtg-math.vector2)

;;----------------------------------------------------------------

(docs:define-docs
  (defun +s
      "
Component-wise add of single-float to `vec2`
")
  (defun -s
      "
Component-wise subtraction of single-float from `vec2`
")
  (defun angle-between
      "
Returns the angle between two `vec2`s.

Equivilent to (abs (v2:angle-from a b))
")
  (defun angle-from
      "
Returns the signed angle between the two `vec2`s.

Positive angles are counter-clockwise
")
  (defun bezier
      "
{TODO}
")
  (defun copy-vec2
      "
Returns a fresh `vec2` which is v2:= to the orignal vector
")
  (defun face-foreward
      "
Returns `vector-a` if `(> (v2:dot vector-a vector-b) 0)` else returns
`(v2:negate vector-a)`
")
  (defun from-angle
      "
Makes a `vec2` from an angle in radians.

Angle is in radians turning counterclockwise from `(v! 0 1)`
")
  (defun from-complex
      "
Makes a `vec2` from a `complex`. The `realpart` becomes the `x` component
and the `imagpart` becomes the `y` component.
")
  (defun lerp
      "
Linearly interpolates between the two `vec2`s by the `single-float` amount
")
  (defun perp-dot
      "
{TODO}
")
  (defun rotate
      "
Creates a new `vec2` which is equivilent to the original once rotated
counterclockwise by `angle` radians.
")
  (defun spline
      "
{TODO}
")
  (defmacro decf
      "
Decrements the `vec2` in 'place' by another `vec2`
")
  (defmacro incf
      "
Increments the `vec2` in 'place' by another `vec2`
"))
