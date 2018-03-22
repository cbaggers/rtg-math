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

(varjo:v-def-glsl-template-fun
 m! (a b) "mat2(~a,~a)" (:vec2 :vec2) :mat2 :pure t)

(varjo:v-def-glsl-template-fun
 m! (a b c) "mat3(~a,~a,~a)" (:vec3 :vec3 :vec3) :mat3 :pure t)

(varjo:v-def-glsl-template-fun
 m! (a b c d) "mat4(~a,~a,~a,~a)" (:vec4 :vec4 :vec4 :vec4) :mat4 :pure t)
