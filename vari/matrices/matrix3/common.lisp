(in-package :%rtg-math.matrix3.common)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun melm (m r c) "~a[~a, ~a]" (:mat3 :int :int)
         :float :v-place-index 0 :pure t)

(varjo:v-def-glsl-template-fun melm (m r c) "~a[~a, ~a]" (:dmat3 :int :int)
         :double :v-place-index 0 :pure t)

;;----------------------------------------------------------------
