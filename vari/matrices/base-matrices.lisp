(in-package :rtg-math.base-matrices)

(varjo:v-def-glsl-template-fun
 m! (a b c d)
 "mat2(~a,~a,~a,~a)"
 (:float :float :float :float)
 :mat2 :pure t)

(varjo:v-def-glsl-template-fun
 m! (a b c d e f g h i)
 "mat3(~a,~a,~a,~a,~a,~a,~a,~a,~a)"
 (:float :float :float :float :float :float :float :float :float)
 :mat3 :pure t)

(varjo:v-def-glsl-template-fun
 m! (a b c d e f g h i j k l m n o p)
 "mat4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
 (:float :float :float :float :float :float :float :float
         :float :float :float :float :float :float :float :float)
 :mat4 :pure t)
