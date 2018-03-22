(in-package #:rtg-math.quaternions.non-consing)

;;----------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (vari:v-deftype v-quaternion () :vec4)
  (varjo:add-alternate-type-name 'quaternion 'v-quaternion))

(varjo:v-def-glsl-template-fun
 w (a) "~a.x" (quaternion) :float :v-place-index 0 :pure t)

(varjo:v-def-glsl-template-fun
 x (a) "~a.y" (quaternion) :float :v-place-index 0 :pure t)

(varjo:v-def-glsl-template-fun
 y (a) "~a.z" (quaternion) :float :v-place-index 0 :pure t)

(varjo:v-def-glsl-template-fun
 z (a) "~a.w" (quaternion)  :float :v-place-index 0 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 dot (a b) "dot(~a, ~a)" (quaternion quaternion) :float :pure t)

;;----------------------------------------------------------------
