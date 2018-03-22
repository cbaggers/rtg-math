(in-package :rtg-math.base-vectors)

(varjo:v-def-glsl-template-fun v! (v) "~a" (:vec2) :vec2 :pure t)
(varjo:v-def-glsl-template-fun v! (v) "~a" (:vec3) :vec3 :pure t)
(varjo:v-def-glsl-template-fun v! (v) "~a" (:vec4) :vec4 :pure t)
(varjo:v-def-glsl-template-fun v! (x y) "vec3(~a,~a)" (:float :vec2) :vec3 :pure t)
(varjo:v-def-glsl-template-fun v! (x y) "vec3(~a,~a)" (:vec2 :float) :vec3 :pure t)

(varjo:v-def-glsl-template-fun v! (x y) "vec4(~a,~a)" (:vec2 :vec2) :vec4 :pure t)
(varjo:v-def-glsl-template-fun v! (x y) "vec4(~a,~a)" (:float :vec3) :vec4 :pure t)
(varjo:v-def-glsl-template-fun v! (x y) "vec4(~a,~a)" (:vec3 :float) :vec4 :pure t)

(varjo:v-def-glsl-template-fun v! (x y z) "vec4(~a,~a,~a)" (:vec2 :float :float) :vec4 :pure t)
(varjo:v-def-glsl-template-fun v! (x y z) "vec4(~a,~a,~a)" (:float :vec2 :float) :vec4 :pure t)
(varjo:v-def-glsl-template-fun v! (x y z) "vec4(~a,~a,~a)" (:float :float :vec2) :vec4 :pure t)

(varjo:v-def-glsl-template-fun v! (x y) "vec2(~a,~a)" (:float :float) :vec2 :pure t)
(varjo:v-def-glsl-template-fun v! (x y z) "vec3(~a,~a,~a)" (:float :float :float) :vec3 :pure t)
(varjo:v-def-glsl-template-fun v! (x y z w) "vec4(~a,~a,~a,~a)" (:float :float :float :float)
         :vec4 :pure t)


(varjo:v-def-glsl-template-fun v!bool (x y) "bvec3(~a,~a)" (:bool :bvec2) :bvec3 :pure t)
(varjo:v-def-glsl-template-fun v!bool (x y) "bvec3(~a,~a)" (:bvec2 :bool) :bvec3 :pure t)

(varjo:v-def-glsl-template-fun v!bool (x y) "bvec4(~a,~a)" (:bvec2 :bvec2) :bvec4 :pure t)
(varjo:v-def-glsl-template-fun v!bool (x y) "bvec4(~a,~a)" (:bool :bvec3) :bvec4 :pure t)
(varjo:v-def-glsl-template-fun v!bool (x y) "bvec4(~a,~a)" (:bvec3 :bool) :bvec4 :pure t)

(varjo:v-def-glsl-template-fun v!bool (x y z) "bvec4(~a,~a,~a)" (:bvec2 :bool :bool) :bvec4 :pure t)
(varjo:v-def-glsl-template-fun v!bool (x y z) "bvec4(~a,~a,~a)" (:bool :bvec2 :bool) :bvec4 :pure t)
(varjo:v-def-glsl-template-fun v!bool (x y z) "bvec4(~a,~a,~a)" (:bool :bool :bvec2) :bvec4 :pure t)

(varjo:v-def-glsl-template-fun v!bool (x y) "bvec2(~a,~a)" (:bool :bool) :bvec2 :pure t)
(varjo:v-def-glsl-template-fun v!bool (x y z) "bvec3(~a,~a,~a)" (:bool :bool :bool) :bvec3 :pure t)
(varjo:v-def-glsl-template-fun v!bool (x y z w) "bvec4(~a,~a,~a,~a)" (:bool :bool :bool :bool)
         :bvec4 :pure t)


(varjo:v-def-glsl-template-fun v!int (x) "~a" (:ivec2) :ivec2 :pure t)
(varjo:v-def-glsl-template-fun v!int (x) "~a" (:ivec3) :ivec3 :pure t)
(varjo:v-def-glsl-template-fun v!int (x) "~a" (:ivec4) :ivec4 :pure t)

(varjo:v-def-glsl-template-fun v!int (x) "ivec2(~a)" (:vec2) :ivec2 :pure t)
(varjo:v-def-glsl-template-fun v!int (x) "ivec3(~a)" (:vec3) :ivec3 :pure t)
(varjo:v-def-glsl-template-fun v!int (x) "ivec4(~a)" (:vec4) :ivec4 :pure t)

(varjo:v-def-glsl-template-fun v!int (x y) "ivec3(~a,~a)" (:int :ivec2) :ivec3 :pure t)
(varjo:v-def-glsl-template-fun v!int (x y) "ivec3(~a,~a)" (:ivec2 :int) :ivec3 :pure t)

(varjo:v-def-glsl-template-fun v!int (x y) "ivec4(~a,~a)" (:ivec2 :ivec2) :ivec4 :pure t)
(varjo:v-def-glsl-template-fun v!int (x y) "ivec4(~a,~a)" (:int :ivec3) :ivec4 :pure t)
(varjo:v-def-glsl-template-fun v!int (x y) "ivec4(~a,~a)" (:ivec3 :int) :ivec4 :pure t)

(varjo:v-def-glsl-template-fun v!int (x y z) "ivec4(~a,~a,~a)" (:ivec2 :int :int) :ivec4 :pure t)
(varjo:v-def-glsl-template-fun v!int (x y z) "ivec4(~a,~a,~a)" (:int :ivec2 :int) :ivec4 :pure t)
(varjo:v-def-glsl-template-fun v!int (x y z) "ivec4(~a,~a,~a)" (:int :int :ivec2) :ivec4 :pure t)

(varjo:v-def-glsl-template-fun v!int (x y) "ivec2(~a,~a)" (:int :int) :ivec2 :pure t)
(varjo:v-def-glsl-template-fun v!int (x y z) "ivec3(~a,~a,~a)" (:int :int :int) :ivec3 :pure t)
(varjo:v-def-glsl-template-fun v!int (x y z w) "ivec4(~a,~a,~a,~a)" (:int :int :int :int)
         :ivec4 :pure t)


(varjo:v-def-glsl-template-fun v!uint (x y) "uvec3(~a,~a)" (:uint :uvec2) :uvec3 :pure t)
(varjo:v-def-glsl-template-fun v!uint (x y) "uvec3(~a,~a)" (:uvec2 :uint) :uvec3 :pure t)

(varjo:v-def-glsl-template-fun v!uint (x y) "uvec4(~a,~a)" (:uvec2 :uvec2) :uvec4 :pure t)
(varjo:v-def-glsl-template-fun v!uint (x y) "uvec4(~a,~a)" (:uint :uvec3) :uvec4 :pure t)
(varjo:v-def-glsl-template-fun v!uint (x y) "uvec4(~a,~a)" (:uvec3 :uint) :uvec4 :pure t)

(varjo:v-def-glsl-template-fun v!uint (x y z) "uvec4(~a,~a,~a)" (:uvec2 :uint :uint) :uvec4 :pure t)
(varjo:v-def-glsl-template-fun v!uint (x y z) "uvec4(~a,~a,~a)" (:uint :uvec2 :uint) :uvec4 :pure t)
(varjo:v-def-glsl-template-fun v!uint (x y z) "uvec4(~a,~a,~a)" (:uint :uint :uvec2) :uvec4 :pure t)

(varjo:v-def-glsl-template-fun v!uint (x y) "uvec2(~a,~a)" (:uint :uint) :uvec2 :pure t)
(varjo:v-def-glsl-template-fun v!uint (x y z) "uvec3(~a,~a,~a)" (:uint :uint :uint) :uvec3 :pure t)
(varjo:v-def-glsl-template-fun v!uint (x y z w) "uvec4(~a,~a,~a,~a)" (:uint :uint :uint :uint)
         :uvec4 :pure t)


(varjo:v-def-glsl-template-fun v!double (x y) "dvec3(~a,~a)" (:double :dvec2) :dvec3 :pure t)
(varjo:v-def-glsl-template-fun v!double (x y) "dvec3(~a,~a)" (:dvec2 :double) :dvec3 :pure t)

(varjo:v-def-glsl-template-fun v!double (x y) "dvec4(~a,~a)" (:dvec2 :dvec2) :dvec4 :pure t)
(varjo:v-def-glsl-template-fun v!double (x y) "dvec4(~a,~a)" (:double :dvec3) :dvec4 :pure t)
(varjo:v-def-glsl-template-fun v!double (x y) "dvec4(~a,~a)" (:dvec3 :double) :dvec4 :pure t)

(varjo:v-def-glsl-template-fun v!double (x y z) "dvec4(~a,~a,~a)" (:dvec2 :double :double) :dvec4 :pure t)
(varjo:v-def-glsl-template-fun v!double (x y z) "dvec4(~a,~a,~a)" (:double :dvec2 :double) :dvec4 :pure t)
(varjo:v-def-glsl-template-fun v!double (x y z) "dvec4(~a,~a,~a)" (:double :double :dvec2) :dvec4 :pure t)

(varjo:v-def-glsl-template-fun v!double (x y) "dvec2(~a,~a)" (:double :double) :dvec2 :pure t)
(varjo:v-def-glsl-template-fun v!double (x y z) "dvec3(~a,~a,~a)" (:double :double :double) :dvec3 :pure t)
(varjo:v-def-glsl-template-fun v!double (x y z w) "dvec4(~a,~a,~a,~a)" (:double :double :double :double)
         :dvec4 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun v2! (x) "vec2(~a)" (:float) :vec2 :pure t)
(varjo:v-def-glsl-template-fun v2! (x y) "vec2(~a, ~a)" (:float :float) :vec2 :pure t)
(varjo:v-def-glsl-template-fun v2!double (x) "dvec2(~a)" (:double) :dvec2 :pure t)
(varjo:v-def-glsl-template-fun v2!double (x y) "dvec2(~a, ~a)" (:double :double) :dvec2 :pure t)
(varjo:v-def-glsl-template-fun v2!int (x) "ivec2(~a)" (:int) :ivec2 :pure t)
(varjo:v-def-glsl-template-fun v2!int (x y) "ivec2(~a, ~a)" (:int :int) :ivec2 :pure t)
(varjo:v-def-glsl-template-fun v2!uint (x) "uvec2(~a)" (:uint) :uvec2 :pure t)
(varjo:v-def-glsl-template-fun v2!uint (x y) "uvec2(~a, ~a)" (:uint :uint) :uvec2 :pure t)

(varjo:v-def-glsl-template-fun v3! (x) "vec3(~a)" (:float) :vec3 :pure t)
(varjo:v-def-glsl-template-fun v3! (x y) "vec3(~a, ~a, 0.0f)" (:float :float) :vec3 :pure t)
(varjo:v-def-glsl-template-fun v3! (x y z) "vec3(~a, ~a, ~a)" (:float :float :float) :vec3 :pure t)
(varjo:v-def-glsl-template-fun v3!double (x) "dvec3(~a)" (:double) :dvec3 :pure t)
(varjo:v-def-glsl-template-fun v3!double (x y) "dvec3(~a, ~a, 0.0lf)" (:double :double) :dvec3 :pure t)
(varjo:v-def-glsl-template-fun v3!double (x y z) "dvec3(~a, ~a, ~a)" (:double :double :double) :dvec3 :pure t)
(varjo:v-def-glsl-template-fun v3!int (x) "ivec3(~a)" (:int) :ivec3 :pure t)
(varjo:v-def-glsl-template-fun v3!int (x y) "ivec3(~a, ~a, 0)" (:int :int) :ivec3 :pure t)
(varjo:v-def-glsl-template-fun v3!int (x y z) "ivec3(~a, ~a, ~a)" (:int :int :int) :ivec3 :pure t)
(varjo:v-def-glsl-template-fun v3!uint (x) "uvec3(~a)" (:uint) :uvec3 :pure t)
(varjo:v-def-glsl-template-fun v3!uint (x y) "uvec3(~a, ~a, 0)" (:uint :uint) :uvec3 :pure t)
(varjo:v-def-glsl-template-fun v3!uint (x y z) "uvec3(~a, ~a, ~a)" (:uint :uint :uint) :uvec3 :pure t)

(varjo:v-def-glsl-template-fun v4! (x) "vec4(~a)" (:float) :vec4 :pure t)
(varjo:v-def-glsl-template-fun v4! (x y) "vec4(~a, ~a, 0.0f, 0.0f)" (:float :float) :vec4 :pure t)
(varjo:v-def-glsl-template-fun v4! (x y z) "vec4(~a, ~a, ~a, 0.0f)" (:float :float :float) :vec4 :pure t)
(varjo:v-def-glsl-template-fun v4! (x y z w) "vec4(~a, ~a, ~a, ~a)" (:float :float :float :float) :vec4 :pure t)
(varjo:v-def-glsl-template-fun v4!double (x) "dvec4(~a)" (:double) :dvec4 :pure t)
(varjo:v-def-glsl-template-fun v4!double (x y) "dvec4(~a, ~a, 0.0lf, 0.0lf)" (:double :double) :dvec4 :pure t)
(varjo:v-def-glsl-template-fun v4!double (x y z) "dvec4(~a, ~a, ~a, 0.0lf)" (:double :double :double) :dvec4 :pure t)
(varjo:v-def-glsl-template-fun v4!double (x y z w) "dvec4(~a, ~a, ~a, ~a)" (:double :double :double :double) :dvec4 :pure t)
(varjo:v-def-glsl-template-fun v4!int (x) "ivec4(~a)" (:int) :ivec4 :pure t)
(varjo:v-def-glsl-template-fun v4!int (x y) "ivec4(~a, ~a, 0, 0)" (:int :int) :ivec4 :pure t)
(varjo:v-def-glsl-template-fun v4!int (x y z) "ivec4(~a, ~a, ~a, 0)" (:int :int :int) :ivec4 :pure t)
(varjo:v-def-glsl-template-fun v4!int (x y z w) "ivec4(~a, ~a, ~a, ~a)" (:int :int :int :int) :ivec4 :pure t)
(varjo:v-def-glsl-template-fun v4!uint (x) "uvec4(~a)" (:uint) :uvec4 :pure t)
(varjo:v-def-glsl-template-fun v4!uint (x y) "uvec4(~a, ~a, 0, 0)" (:uint :uint) :uvec4 :pure t)
(varjo:v-def-glsl-template-fun v4!uint (x y z) "uvec4(~a, ~a, ~a, 0)" (:uint :uint :uint) :uvec4 :pure t)
(varjo:v-def-glsl-template-fun v4!uint (x y z w) "uvec4(~a, ~a, ~a, ~a)" (:uint :uint :uint :uint) :uvec4 :pure t)

;;----------------------------------------------------------------

(varjo:v-def-glsl-template-fun x (a) "~a.x" (:bvec2) :bool :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:bvec3) :bool :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:bvec4) :bool :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:dvec2) :double :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:dvec3) :double :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:dvec4) :double :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:ivec2) :int :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:ivec3) :int :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:ivec4) :int :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:uvec2) :uint :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:uvec3) :uint :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:uvec4) :uint :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:vec2) :float :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:vec3) :float :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun x (a) "~a.x" (:vec4) :float :v-place-index 0 :pure t)

(varjo:v-def-glsl-template-fun y (a) "~a.y" (:bvec2) :bool :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:bvec3) :bool :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:bvec4) :bool :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:dvec2) :double :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:dvec3) :double :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:dvec4) :double :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:ivec2) :int :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:ivec3) :int :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:ivec4) :int :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:uvec2) :uint :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:uvec3) :uint :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:uvec4) :uint :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:vec2) :float :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:vec3) :float :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun y (a) "~a.y" (:vec4) :float :v-place-index 0 :pure t)

(varjo:v-def-glsl-template-fun z (a) "~a.z" (:vec3)  :float :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun z (a) "~a.z" (:bvec3) :bool :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun z (a) "~a.z" (:ivec3) :int :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun z (a) "~a.z" (:uvec3) :uint :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun z (a) "~a.z" (:dvec3) :double :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun z (a) "~a.z" (:vec4)  :float :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun z (a) "~a.z" (:bvec4) :bool :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun z (a) "~a.z" (:ivec4) :int :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun z (a) "~a.z" (:uvec4) :uint :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun z (a) "~a.z" (:dvec4) :double :v-place-index 0 :pure t)

(varjo:v-def-glsl-template-fun w (a) "~a.w" (:vec4) :float :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun w (a) "~a.w" (:bvec4) :bool :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun w (a) "~a.w" (:ivec4) :int :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun w (a) "~a.w" (:uvec4) :uint :v-place-index 0 :pure t)
(varjo:v-def-glsl-template-fun w (a) "~a.w" (:dvec4) :double :v-place-index 0 :pure t)

;;----------------------------------------------------------------
