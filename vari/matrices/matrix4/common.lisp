(in-package :%rtg-math.matrix4.common)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun
 melm (m r c) "~a[~a, ~a]" (:mat4 :int :int)
 :float :v-place-index 0 :pure t)

(varjo:v-def-glsl-template-fun
 melm (m r c) "~a[~a, ~a]" (:dmat4 :int :int)
 :double :v-place-index 0 :pure t)

;;----------------------------------------------------------------


(varjo:v-defun minor ((mat-a :mat4)
                      (row-0 :int) (row-1 :int) (row-2 :int)
                      (col-0 :int) (col-1 :int) (col-2 :int))
  (cl:+ (cl:* (melm mat-a row-0 col-0)
              (cl:- (cl:* (melm mat-a row-1 col-1) (melm mat-a row-2 col-2))
                    (cl:* (melm mat-a row-2 col-1) (melm mat-a row-1 col-2))))

        (cl:* (melm mat-a row-0 col-2)
              (cl:- (cl:* (melm mat-a row-1 col-0) (melm mat-a row-2 col-1))
                    (cl:* (melm mat-a row-2 col-0) (melm mat-a row-1 col-1))))

        (cl:- (cl:* (melm mat-a row-0 col-1)
                    (cl:- (cl:* (melm mat-a row-1 col-0) (melm mat-a row-2 col-2))
                          (cl:* (melm mat-a row-2 col-0) (melm mat-a row-1 col-2)))))))

;;----------------------------------------------------------------
