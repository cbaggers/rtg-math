(in-package :rtg-math.base-maths)

(docs:define-docs
  (defconstant +inv-pi+
      "
`(/ 1 pi)` as a `single-float`. This is the older naming, it is now prefered
to use `inv-pi-f`
")
  (defconstant +one-degree-in-radians+
      "
The number of degrees in 1 radian as a `single-float`
")
  (defconstant +pi+
      "
`pi` as a `single-float`. This is the older naming, it is now prefered
to use `pi-f`
")
  (defconstant 0.5pi
      "
`(/ pi 2)` as a `double-float`
")
  (defconstant 0.5pi-f
      "
`(/ pi 2)` as a `single-float`
")
  (defconstant 2pi
      "
`(* PI 2)` as a `double-float`
")
  (defconstant 2pi-f
      "
`(* PI 2)` as a `single-float`
")
  (defconstant inv-pi
      "
`(/ 1 pi) as a `double-float`
")
  (defconstant inv-pi-f
      "
`(/ 1 pi) as a `single-float`
")
  (defconstant pi-f
      "
`pi` as a `single-float`
")

  (defun clamp
      "
Force a `single-float` to be constrained to the given range.

Equivalent to `(min (max val-f min) max)`
")
  (defun degrees
      "
Converts a quantity, specified in radians into degrees.
")
  (defun degrees-f
      "
Converts a quantity, specified in radians as a `single-float` into degrees.
")
  (defun lerp
      "
Performs a linear interpolation between 'start' and 'end' by the
given `amount`

")
  (defun mix
      "
Performs a linear interpolation between 'start' and 'end' by the
given `amount`
")
  (defun radians
      "
Converts a quantity, specified in degrees into radians.
")
  (defun radians-f
      "
Converts a quantity, specified in degrees as a `single-float` into radians.
")
  (defun saturate
      "
clamps the given `single-float` between 0.0 and 1.0
")
  (defun smoothstep
      "
Performs smooth Hermite interpolation between 0 and 1 when 'a' < 'x'
< 'b'
")
  (defun spline
      "
{TODO}
"))
