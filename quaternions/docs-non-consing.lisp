(in-package :rtg-math.quaternions.non-consing)

(docs:define-docs
  (defun *
      "
Destructively multiplies (and thus combines) the second  `quaternion` into the
first.
")
  (defun conjugate
      "
Destructively replaces the given `quaternion` with it's conjugate
")
  (defun from-axis-angle
      "
Turns the given `quaternion` into one which represents a rotation around the
given axis by the given angle
")
  (defun from-fixed-angles
      "
Turns the given `quaternion` into one which  represents a rotation around the
given axies by the given angles.
")
  (defun normalize
      "
Destructively replaces the given `quaternion` with the normalized version of
itself.
")
  (defun to-mat3
      "
Given a `quaternion` and a `mat3` to mutate this function will mutate the
`mat3` such that it represents the same rotation as the `quaternion` does.
")
  (defun to-mat4
      "
Given a `quaternion` and a `mat4` to mutate this function will mutate the
`mat4` such that it represents the same rotation as the `quaternion` does.
"))
