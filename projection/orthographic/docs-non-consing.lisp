(in-package :rtg-math.projection.non-consing)

;;------------------------------------------------------------

(docs:define-docs
  (defun orthographic
      "
Creates a `mat4` and mutates it to be an orthographic projection matrix
")
  (defun orthographic-v2
      "
Creates a `mat4` and mutates it to be an orthographic projection matrix (with
the frame size specified by a `vec2`)
"))
