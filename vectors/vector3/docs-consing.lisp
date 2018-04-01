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
")
  (DEFUN 0P
      "
Checks if the length of the vector is zero. As this is a
floating point number it checks to see if the length is
below a threshold set in the base-maths package

Note: Old, float epsilons are tricky, I have a ticket open to review this")
  (DEFUN UNITP
      "
Checks if the vector is of unit length. As this is a
floating point number it checks to see if the length is
within the range of 1 + or - and threshold set in base-maths

Note: Old, float epsilons are tricky, I have a ticket open to review this")
  (DEFUN =
      "
Returns either t if the two vectors are equal. Otherwise it returns nil.")
  (defun +
      "
Takes any number of `vec3` and performs component-wise addition on them
returning a new `vec3`")
  (defun -
      "
Takes any number of `vec3` and performs component-wise subtraction on them
returning a new `vec3`")
  (defun *
      "
Takes any number of `vec3` and performs component-wise multiplication on them
returning a new `vec3`")
  (defun *V
      "
Component-wise multiplication of two `vec3`")
  (defun *S
      "
Component-wise multiplication of the vector by the scalar")
  (defun /S
      "
Component-wise division of the vector by the scalar")
  (defun /
      "
Component-wise division of the two `vec3`")
  (defun NEGATE
      "
Return a vector that is the negated version of the vector passed in")
  (defun DOT
      "
Return the dot product of vector-a and vector-b.")
  (defun LENGTH-SQUARED
      "
Return the squared length of the `vec3`

`length` is the square root of this value. The `sqrt` function is
(relatively slow so if all thats needs doing is to compare lengths then
prefer the `length-squared` function")
  (defun LENGTH
      "
Returns the length of the `vec3`

If you only need to compare relative lengths then prefer the `length-squared`
function as `sqrt` is a (relatively) slow operation.")
  (defun DISTANCE-SQUARED
      "
Finds the squared distance between 2 points defined by `vec3`s
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
Return a `vec3` containing the `abs` of the original vec3's components.")
  (defun ABSOLUTE-DOT
      "
Return the absolute dot product of the vector-a and vector-b.")
  (defun NORMALIZE
      "
This normalizes the given `vec3`.")
  (DEFUN CROSS
      "
Calculates the cross-product of 2 `vec3`, i.e. the vector
that lies perpendicular to them both. The resulting `vec3`
will not be normalized."))
