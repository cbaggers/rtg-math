(in-package :rtg-math.vari)

(v-def-glsl-template-fun m3:melm (mat row col) "~a[~a][~a]"
                         (:mat3 :int :int) :mat3
                         :pure t
                         :template-arg-remap (0 2 1))

(v-def-glsl-template-fun m3:melm (mat row col) "~a[~a][~a]"
                         (:mat3 :uint :uint) :mat3
                         :pure t
                         :template-arg-remap (0 2 1))

(v-def-glsl-template-fun m3:0! () "mat3(0)" () :mat3
                         :pure t)


(v-def-glsl-template-fun m3:make (a b c d e f g h i)
                         "mat3(~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a)"
                         (:float :float :float
                                 :float :float :float
                                 :float :float :float)
                         :mat3
                         :pure t)

(v-def-glsl-template-fun m3:copy-mat3 (a) "mat3(~a)" (:mat3) :mat3
                         :pure t)

(defun-g from-rows ((row-1 :vec3) (row-2 :vec3) (row-3 :vec3))
  (mat3 (x row-1) (y row-1) (z row-1)
        (x row-2) (y row-2) (z row-2)
        (x row-3) (y row-3) (z row-3)))

(v-defun m3:get-row ((m )) "vec3(~a)" (:mat3 :int) :vec3
         :pure t)

(v-defun m3:get-row ((mat-a :mat3) (row-num :int))
  (vec3 (melm mat-a row-num 0)
        (melm mat-a row-num 1)
        (melm mat-a row-num 2)))

;; {TODO}
;; get-rows .. would be trivial if the result was values and not list
