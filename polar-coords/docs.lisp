(in-package :rtg-math.polar)

;;----------------------------------------------------------------

(docs:define-docs
  (defun cartesian->polar
      "Takes a cartesian `vec2` and converts it to a polar coord vector with the layout (θ r).

Notes:
r - length of υ
θ - angle between υ and y axis
θ>0 means counter-clockwise rotation
")
  (defun cartesian->polar
      "
Takes a polar coord vector with the layout (θ r) and and converts it to
a cartesian `vec2`

Notes:
r - length of υ
θ - angle between υ and y axis
θ>0 means counter-clockwise rotation
"))
