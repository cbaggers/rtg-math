(in-package :rtg-math.vector4.non-consing)

;;----------------------------------------------------------------

(docs:define-docs
  (defun +s
      "
Destructive Component-wise add of `single-float` into the given `vec2`
")
  (defun -s
      "
Destructive Component-wise subtraction of `single-float` from the given `vec2`
")
  (defun set-components
      "
Destructively updates of the components of the given `vec2` to the new
`single-float`s provided.
")
  (defun +
      "
Destructively performs component-wise addition of the `vec4`s, the
first `vec4` is mutated.")
  (DEFUN -
      "
Destructively performs component-wise substraction of the `vec4`s, the
first `vec4` is mutated.")
  (DEFUN *
      "
Destructively performs component-wise multiplication of the `vec4`s, the
first `vec4` is mutated.")
  (DEFUN *S
      "
Destructively performs component-wise multiplication of the `vec4` by
the scalar")
  (DEFUN /S
      "
Destructively performs component-wise division of the `vec4` by
the scalar")
  (DEFUN /
      "
Destructively performs component-wise division of the `vec4`s, the
first `vec4` is mutated")
  (DEFUN NEGATE
      "
Destructively negates the given vector")
  (DEFUN NORMALIZE
      "
Destructively normalizes the vector"))
