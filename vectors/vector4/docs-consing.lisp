(in-package :rtg-math.vector4)

;;----------------------------------------------------------------

(docs:define-docs
  (defun +s
      "
Component-wise add of single-float to `vec4`
")
  (defun -s
      "
Component-wise subtraction of single-float from `vec4`
")
  (defun bezier
      "
{TODO}
")
  (defun copy-vec4
      "
Returns a fresh `vec4` which is v4:= to the orignal vector
")
  (defun face-foreward
      "
Returns `vector-a` if `(> (v4:dot vector-a vector-b) 0)` else returns
`(v4:negate vector-a)`
")
  (defun lerp
      "
Linearly interpolates between the two `vec4`s by the `single-float` amount
")
  (defun make
      "
Make a `vec4` from 4 `single-float`s
")
  (defun spline
      "
{TODO}
")
  (defmacro decf
      "
Decrements the `vec4` in 'place' by another `vec4`
")
  (defmacro incf
      "
Increments the `vec4` in 'place' by another `vec4`
"))
