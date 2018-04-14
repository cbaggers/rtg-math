(in-package :%rtg-math.matrix3.common)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun mref (m c r) "~a[~a, ~a]" (:mat3 :int :int)
         :float :v-place-index 0 :pure t)

(varjo:v-def-glsl-template-fun mref (m c r) "~a[~a, ~a]" (:dmat3 :int :int)
         :double :v-place-index 0 :pure t)

(varjo:v-defun melm ((m :mat3) (r :int) (c :int))
  (mref m c r))

(varjo:v-defun melm ((m :dmat3) (r :int) (c :int))
  (mref m c r))

;;----------------------------------------------------------------
