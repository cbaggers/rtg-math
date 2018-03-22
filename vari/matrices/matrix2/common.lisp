(in-package :%rtg-math.matrix2.common)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 melm (m r c) "~a[~a, ~a]" (:mat2 :int :int)
 :float :v-place-index 0 :pure t)

(varjo:v-def-glsl-template-fun
 melm (m r c) "~a[~a, ~a]" (:dmat2 :int :int)
 :double :v-place-index 0 :pure t)

;;----------------------------------------------------------------
