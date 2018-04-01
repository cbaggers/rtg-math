(in-package :rtg-math.quaternions)

(docs:define-docs
  (defun *
      "
Multiplies (and thus combines) the two `quaternion`s given, returning a
new `quaternion`
")
  (defun +
      "
Component-wise addition of any number of quaternions.
")
  (defun /=
      "
Component-wise comparison of two quaternions. Returns T if any components are
not equal (by =)
")
  (defun 0!
      "
Returns a new `quaternion` where every component is 0
")
  (defun 0p
      "
Returns T if all the components of the given `quaternion` are 0
")
  (defun =
      "
Returns T if the two `quaternion`s given are component-wise equal (by cl:=)
")
  (defun approx-slerp
      "
A faster and less accurate version of `slerp` for quaternions
")
  (defun conjugate
      "
Returns the conjugate of the given quaternion
")
  (defun copy
      "
Returns a fresh `quaternion` which is = to the one given
")
  (defun from-axies
      "
{TODO}
")
  (defun from-axis-angle
      "
Returns a new `quaternion` which represents a rotation around the given
axis by the given angle
")
  (defun from-direction
      "
Creates a `quaternion` that would rotate the vector (v! 0 0 -1) to face in the
given direction.
")
  (defun from-fixed-angles
      "
Creates a `quaternion` that represents a rotation around the given axies by the
given angles
")
  (defun from-fixed-angles-v3
      "
Creates a `quaternion` that represents a rotation around the given axies by the
angles given in the `vec3`
")
  (defun from-mat3
      "
Creates `quaternion` that represents the same rotation the `mat3` does.
")
  (defun identity
      "
Returns a fresh identity quaternion
")
  (defun identity-p
      "
Returns T if the given `quaternion` is the identity quaternion
")
  (defun inverse
      "
Returns a new `quaternion` that is the inverse of the one given
")
  (defun look-at
      "
Creates a `quaternion` that would rotate the vector (v! 0 0 -1) to point in
the direction from `from3` to `to3`
")
  (defun magnitude
      "
Returns the magnitude of the quaternion
")
  (defun norm
      "
Returns the 'norm' of the `quaternion`

Note: This is not the normalized verison of the quaternion, that is performed
      by the `normalize` function.
")
  (defun normalize
      "
Returns a fresh, normalized version of the given vector.
")
  (defun point-at
      "
Creates a `quaternion` that
")
  (defun qconjugate
      "
Deprecated: Please use `conjugate`
")
  (defun to-direction
      "
Returns a new `vec3` is equal to `(v! 0 0 -1)` having been rotated by the
given `quaternion`
")
  (defun to-direction-vec4
      "
Returns a new `vec4` is equal to `(v! 0 0 -1 0)` having been rotated by the
given `quaternion`
")
  (defun to-mat3
      "
Returns a new `mat3` which represents the same rotation as the given
`quaternion`
")
  (defun to-mat4
      "
Returns a new `mat4` which represents the same rotation as the given
`quaternion` (and zero translation)
")
  (defun unitp
      "
Returns T if this is a unit quaternion.
")
  (DEFUN Q!
      "
Make a `quaternion` from 4 `single-float`s")
  (DEFUN MAKE
      "
Make a `quaternion` from 4 `single-float`s")
  (DEFUN GET-AXIS-ANGLE
      "
Gets one possible axis-angle pair that will generate this `quaternion`

Assumes that this is a normalized quaternion. It is critical that this
is true as otherwise you will at best get a runtime error, and most likely a
silently incorrect result.")
  (defun -
      "
Takes any number of `quaternion` and performs component-wise subtraction on
them returning a new `quaternion`")
  (defun *S
      "
Component-wise multiplication of the `quaternion` by the scalar")
  (DEFUN ROTATE
      "
Rotates the given `vec3` using the `quaternion`.

Assumes the `quaternion` is normalized.")
  (DEFUN ROTATE-V4
      "
Rotates the given `vec4` by the `quaternion`.

Assumes the `quaternion` is normalized.")
  (DEFUN LERP
      "
Linearaly interpolate between two quaternions. Note that this
will always take the shortest path.")
  (DEFUN SLERP
      "
Spherically interpolate between two quaternions. Note that this
will always take the shortest path."))
