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
")
  (defun make
      "
This takes 2 single-floats and give back a `vec2`.")
  (defun 0p
      "
Checks if the length of the vector is zero. As this is a floating point number
it checks to see if the length is below a threshold set in the base-maths
package

Note: Old, float epsilons are tricky, I have a ticket open to review this")
  (defun UNITP
      "
Checks if the vector is of unit length. As this is a
floating point number it checks to see if the length is
within the range of 1 + or - and threshold set in base-maths

Note: Old, float epsilons are tricky, I have a ticket open to review this")
  (defun =
      "
Returns T if the two vectors are equal. Otherwise it returns nil.")
  (defun +
      "
Takes any number of `vec2` and performs component-wise addition on them
returning a new `vec2`")
  (defun -
      "
Takes any number of `vec2` and performs component-wise subtraction on them
returning a new `vec2`")
  (defun *
      "
Takes any number of `vec2` and performs component-wise multiplication on them
returning a new `vec2`")
  (defun *V
      "
Component-wise multiplication of two `vec2`")
  (defun *S
      "
Component-wise multiplication of the vector by the scalar")
  (defun /S
      "
Component-wise division of the vector by the scalar")
  (defun /
      "
Component-wise division of the two `vec2`")
  (defun NEGATE
      "
Return a vector that is the negated version of the vector passed in")
  (defun DOT
      "
Return the dot product of vector-a and vector-b.")
  (defun LENGTH-SQUARED
      "
Return the squared length of the `vec2`

`length` is the square root of this value. The `sqrt` function is
(relatively slow so if all thats needs doing is to compare lengths then
prefer the `length-squared` function")
  (defun LENGTH
      "
Returns the length of the `vec2`

If you only need to compare relative lengths then prefer the `length-squared`
function as `sqrt` is a (relatively) slow operation.")
  (defun DISTANCE-SQUARED
      "
Finds the squared distance between 2 points defined by `vec2`s
vector-a & vector-b

`distance` is the square root of this value. The `sqrt` function is
(relatively) slow so if all thats needs doing is to compare distances
then prefer the `distance-squared` function")
  (defun DISTANCE
      "
Return the distance between 2 points defined by vectors
   vector-a & vector-b. If comparing distances, use
   c-distance-squared as it desnt require a sqrt and thus is
   faster.
If simply comparing distances then prefer the `distance-squared` function as
the `sqrt` required here is (relatively) slow.")
  (defun ABS
      "
Return a `vec2` containing the `abs` of the original vec2's components.")
  (defun ABSOLUTE-DOT
      "
Return the absolute dot product of the vector-a and vector-b.")
  (defun NORMALIZE
      "
This normalizes the given `vec2`.")
  (defun CROSS
      "
Calculates the cross-product of 2 `vec2`s. This results in a
`single-float` which is 2 times the area of the triangle."))
