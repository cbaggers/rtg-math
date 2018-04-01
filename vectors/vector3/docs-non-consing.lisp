(in-package :rtg-math.vector3.non-consing)

;;----------------------------------------------------------------

(docs:define-docs
  (defun +s
      "
Destructive Component-wise add of `single-float` into the given `vec3`
")
  (defun -s
      "
Destructive Component-wise subtraction of `single-float` from the given `vec3`
")
  (defun rotate
      "
Rotates the given `vec3` counterclockwise by the radians in the `rotation`
vec3 provided.
")
  (defun set-components
      "
Destructively updates of the components of the given `vec3` to the new
`single-float`s provided.
")
  (defun +
      "
Destructively performs component-wise addition of the `vec3`s, the
first `vec3` is mutated.")
  (DEFUN -
      "
Destructively performs component-wise substraction of the `vec3`s, the
first `vec3` is mutated.")
  (DEFUN *
      "
Destructively performs component-wise multiplication of the `vec3`s, the
first `vec3` is mutated.")
  (DEFUN *S
      "
Destructively performs component-wise multiplication of the `vec3` by
the scalar")
  (DEFUN /S
      "
Destructively performs component-wise division of the `vec3` by
the scalar")
  (DEFUN /
      "
Destructively performs component-wise division of the `vec3`s, the
first `vec3` is mutated")
  (DEFUN NEGATE
      "
Destructively negates the given vector")
  (DEFUN NORMALIZE
      "
Destructively normalizes the vector"))
