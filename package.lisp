;;;; package.lisp

(defpackage #:cl-game-math
  (:use #:cl))

(defpackage #:cl-game-math.types
  (:use #:cl)
  (:export :mat3 :mat4
	   :vec2 :vec3 :vec4
	   :ivec2 :ivec3 :ivec4
	   :uvec2 :uvec3 :uvec4
	   :ivec2 :ivec3 :ivec4
	   :uvec2 :uvec3 :uvec4
	   :int8-vec2 :int8-vec3 :int8-vec4
	   :uint8-vec2 :uint8-vec3 :uint8-vec4))

(defpackage :cl-game-math.base-maths
  (:use :cl)
  (:export :clamp
           :clampf
           :+float-threshold+
           :+one-degree-in-radians+
           :+pi+
           :float-zero
           :float>=0
           :float<=0
           :float>0
           :float<0
           :float-greater-than-zero
           :c-sqrt
           :c-inv-sqrt
           :degrees
           :radians))

(defpackage :cl-game-math.maths
  (:use :cl)
  (:export :lerp :mix :stepv :clamp :smoothstep :pulse
           :spline))

(defpackage :cl-game-math.base-vectors
  (:use :cl)
  (:export :v! :v-x :v-y :v-z :v-w
           :v!byte :v!ubyte :v!int))

(defpackage :cl-game-math.base-matrices
  (:use :cl)
  (:export :m!))

(defpackage :cl-game-math.vector2
  (:use :cl)
  (:nicknames :v2)
  (:shadow :eql :+ :- :* :/ :length :zerop)
  (:export :make-vector2 :+ :- :* :eql
           :*vec :/ :/vec :negate :length-squared
           :length :distance-squared :distance :dot
           :absolute-dot :normalize :perp-dot
           :*unit-x* :*unit-y* :*unit-scale*
           :zerop :unitp :cross :face-foreward :lerp
           :bezier :spline :from-complex)
  (:import-from :cl-game-math.base-maths :float-zero
                :c-sqrt
                :c-inv-sqrt)
  (:import-from :cl-game-math.base-vectors :v-x :v-y))

(defpackage :cl-game-math.vector3
  (:use :cl)
  (:nicknames :v3)
  (:shadow :incf :eql :+ :- :* :/ :length :zerop)
  (:export :make-vector3 :eql :+ :- :* :/
           :*vec :/vec :negate :length-squared
           :length :distance-squared :distance :dot
           :absolute-dot :normalize :cross
           :*unit-x* :*unit-y* :*unit-z* :*unit-scale*
           :zerop :unitp :cross :face-foreward :lerp
           :bezier :spline :incf)
  (:import-from :cl-game-math.base-maths :float-zero
                :c-sqrt
                :c-inv-sqrt)
  (:import-from :cl-game-math.base-vectors :v-x :v-y :v-z))

(defpackage :cl-game-math.vector4
  (:use :cl)
  (:nicknames :v4)
  (:shadow :eql :+ :- :* :/ :length :zerop)
  (:export :make-vector4 :+ :- :* :/ :v3* :eql
           :*vec :/vec :negate :length-squared
           :length :distance-squared :distance :dot
           :absolute-dot :normalize :cross
           :*unit-x* :*unit-y* :*unit-z* :*unit-w* :*unit-scale*
           :zerop :unitp :face-foreward :lerp
           :bezier :spline)
  (:import-from :cl-game-math.base-maths :float-zero
                :c-sqrt
                :c-inv-sqrt)
  (:import-from :cl-game-math.base-vectors :v-x :v-y :v-z :v-w))

(defpackage :cl-game-math.vectors
  (:use :cl)
  (:nicknames :v)
  (:export :v :make-vector :zerop :unitp := :+ :/= :1+ :1- :- :*
           :/ :length :length-squared :distance :distance-squared
           :dot :absolute-dot :perp-dot :normalize :cross :eql
           :swizzle :s~ :merge-into-vector :negate :face-foreward :lerp
           :mix :bezier :x :y :z :w :dvec :dvec*)
  (:shadow :zerop :+ :eql := :/= :1+ :1- :- :* :/ :length)
  (:import-from :cl-game-math.vector2
                :make-vector2)
  (:import-from :cl-game-math.vector3
                :make-vector3)
  (:import-from :cl-game-math.vector4
                :make-vector4))

(defpackage :cl-game-math.matrix3
  (:use :cl)
  (:nicknames :m3)
  (:shadow :eql :identity)
  (:export :melm :identity :zero-matrix3
           :make :! :make-from-rows :get-rows
           :get-row :make-from-columns :get-columns
           :get-column :determinate-cramer :inverse
           :mzerop :identityp :transpose :adjoint
           :mtrace :rotation-from-euler
           :rotation-from-axis-angle :scale
           :rotation-x :rotation-y :rotation-z
           :get-fixed-angles :get-axis-angle :m+ :m- :negate
           :m* :m*vec :mcol*vec3 :mrow*vec3 :m*scalar :eql)
  (:import-from :cl-game-math.base-maths :float-zero
                :c-sqrt)
  (:import-from :cl-game-math.vector3
                :make-vector3)
  (:import-from :cl-game-math.base-vectors :v-x :v-y :v-z :v-w))

(defpackage :cl-game-math.matrix4
  (:use :cl)
  (:nicknames :m4)
  (:shadow :eql :identity)
  (:export :melm :identity :zero-matrix4
           :2dclipspace-to-imagespace-matrix4 :make :!
           :mzerop :identityp :minor :adjoint
           :determinant :affine-inverse :transpose
           :translation :rotation-from-matrix3
           :rotation-from-euler :rotation-from-axis-angle
           :scale :rotation-x :rotation-y
           :rotation-z :get-fixed-angles :mtrace
           :get-axis-angle :m+ :m- :negate :m*scalar
           :mcol*vec4 :mrow*vec4 :m* :transform
           :to-matrix3 :get-row :get-rows :get-column
           :get-columns :eql)
  (:import-from :cl-game-math.base-maths :float-zero
                :c-sqrt)
  (:import-from :cl-game-math.vector3
                :make-vector3)
  (:import-from :cl-game-math.vector4
                :make-vector4)
  (:import-from :cl-game-math.base-vectors :v-x :v-y :v-z :v-w))


(defpackage :cl-game-math.matrices
  (:use :cl)
  (:nicknames :m)
  (:export :zerop :unitp :+ :eql := :/= :1+ :1- :- :*
           :identityp :elt :elm :get-rows :get-row
           :get-columns :get-column :determinant
           :inverse :transpose :trace :negate
           :to-string)
  (:shadow :zerop :unitp :+ :eql := :/= :1+ :1- :- :*
           :elt :trace))

(defpackage :cl-game-math.quaternions
  (:use :cl :cl-game-math.base-maths)
  (:nicknames :q)
  (:shadow :lerp)
  (:export :w :x :y :z :q! :zero-quit :zero-quatp
           :unit-quatp :identity-quat :identity-quatp
           :make-quat :make-quat-from-vec3
           :make-quat-from-rotation-matrix3
           :make-quat-from-axis-angle
           :make-quat-from-look-at
           :make-quat-from-axies
           :make-quat-from-fixed-angles
           :magnitude :norm :quat-eql :quat-!eql
           :copy :get-axis-angle :normalize :qconjugate
           :inverse :q+1 :q+ :q-1 :q- :q* :q*quat
           :dot :rotate :lerp :slerp :approx-slerp
           :to-matrix3 :to-matrix4))

(defpackage :cl-game-math.projection
  (:use :cl :cl-game-math.base-maths)
  (:shadow :lerp)
  (:export :perspective :orthographic))
