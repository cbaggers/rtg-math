;;;; package.lisp

(defpackage #:%rtg-math
  (:use #:cl)
  (:export :case= :defun-typed :defun-typed-inline))

(defpackage #:rtg-math.types
  (:use #:cl :%rtg-math)
  (:export :quaternion
	   :mat3 :mat4
	   :vec2 :vec3 :vec4
	   :ivec2 :ivec3 :ivec4
	   :uvec2 :uvec3 :uvec4
	   :ivec2 :ivec3 :ivec4
	   :uvec2 :uvec3 :uvec4
	   :int8-vec2 :int8-vec3 :int8-vec4
	   :uint8-vec2 :uint8-vec3 :uint8-vec4))

(defpackage :rtg-math.base-maths
  (:use :cl :%rtg-math :rtg-math.types)
  (:export :clamp
           :clampf
           :+one-degree-in-radians+
           :+pi+
           :inv-sqrt
           :degrees
           :radians))

(defpackage :rtg-math.maths
  (:use :cl :%rtg-math :rtg-math.types)
  (:export :lerp :mix :stepv :clamp :smoothstep :pulse
           :spline))

(defpackage :rtg-math.base-vectors
  (:use :cl :%rtg-math :rtg-math.types)
  (:export :v! :x :y :z :w
           :v!byte :v!ubyte :v!int))

(defpackage :rtg-math.base-matrices
  (:use :cl :%rtg-math :rtg-math.types)
  (:export :m!))

(defpackage :rtg-math.vector2
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v2)
  (:import-from :rtg-math.base-vectors :x :y :z :w)
  (:shadow := :+ :- :* :/ :length :0p)
  (:export :make :+ :- :* :=
           :*vec :/ :/vec :negate :length-squared
           :length :distance-squared :distance :dot
           :absolute-dot :normalize :perp-dot
           :0p :unitp :cross :face-foreward :lerp
           :bezier :spline :from-complex)
  (:import-from :rtg-math.base-maths
                :inv-sqrt))

(defpackage :rtg-math.vector3
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v3)
  (:import-from :rtg-math.base-vectors :x :y :z :w)
  (:shadow :incf := :+ :- :* :/ :length :0p)
  (:export :make := :+ :- :* :/
           :*vec :/vec :negate :length-squared
           :length :distance-squared :distance :dot
           :absolute-dot :normalize :cross
           :0p :unitp :cross :face-foreward :lerp
           :bezier :spline :incf)
  (:import-from :rtg-math.base-maths
                :inv-sqrt))

(defpackage :rtg-math.vector4
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v4)
  (:import-from :rtg-math.base-vectors :x :y :z :w)
  (:shadow := :+ :- :* :/ :length :0p)
  (:export :make :+ :- :* :/ :v3* :=
           :*vec :/vec :negate :length-squared
           :length :distance-squared :distance :dot
           :absolute-dot :normalize :cross
           :0p :unitp :face-foreward :lerp
           :bezier :spline)
  (:import-from :rtg-math.base-maths
                :inv-sqrt))

(defpackage :rtg-math.vectors
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v)
  (:import-from :rtg-math.base-maths :case=)
  (:import-from :rtg-math.base-vectors :x :y :z :w)
  (:export :v :make-vector :0p :unitp := :+ :/= :1+ :1- :- :*
           :/ :length :length-squared :distance :distance-squared
           :dot :absolute-dot :perp-dot :normalize :cross
           :swizzle :s~ :merge-into-vector :negate :face-foreward :lerp
           :mix :bezier :x :y :z :w)
  (:shadow :0p :+ := :/= :1+ :1- :- :* :/ :length)
  (:import-from :rtg-math.vector2
                :make)
  (:import-from :rtg-math.vector3
                :make)
  (:import-from :rtg-math.vector4
                :make))

(defpackage :rtg-math.matrix3
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :m3)
  (:shadow := :identity)
  (:import-from :rtg-math.base-vectors :x :y :z :w)
  (:export :melm :identity :zero-matrix3
           :make :! :from-rows :get-rows
           :get-row :from-columns :get-columns
           :get-column :determinate :affine-inverse
           :0p :identityp :transpose :adjoint
           :mtrace :rotation-from-euler
           :rotation-from-axis-angle :scale
           :rotation-x :rotation-y :rotation-z
           :get-fixed-angles :get-axis-angle :m+ :m- :negate
           :m* :m*vec :mcol*vec3 :mrow*vec3 :m*scalar :=)
  (:import-from :rtg-math.base-maths)
  (:import-from :rtg-math.vector3
                :make))

(defpackage :rtg-math.matrix4
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :m4)
  (:import-from :rtg-math.base-vectors :x :y :z :w)
  (:shadow := :identity)
  (:export :melm :identity :zero-matrix4
	   :from-rows :from-columns
           :2dclipspace-to-imagespace-matrix4 :make :!
           :0p :identityp :minor :adjoint
           :determinant :affine-inverse :transpose
           :translation :rotation-from-matrix3
           :rotation-from-euler :rotation-from-axis-angle
           :scale :rotation-x :rotation-y
           :rotation-z :get-fixed-angles :mtrace
           :get-axis-angle :m+ :m- :negate :m*scalar
           :mcol*vec4 :mrow*vec4 :m* :transform
           :to-mat3 :get-row :get-rows :get-column
           :get-columns :=)
  (:import-from :rtg-math.base-maths)
  (:import-from :rtg-math.vector3
                :make)
  (:import-from :rtg-math.vector4
                :make))


(defpackage :rtg-math.matrices
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :m)
  (:import-from :rtg-math.base-vectors :x :y :z :w)
  (:import-from :rtg-math.base-maths :case=)
  (:export :0p :unitp :+ := :/= :1+ :1- :- :*
           :identityp :elt :elm :get-rows :get-row
           :get-columns :get-column :determinant
           :inverse :transpose :trace :negate
           :to-string)
  (:shadow :0p :unitp :+ := :/= :1+ :1- :- :*
           :elt :trace))

(defpackage :rtg-math.quaternions
  (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths)
  (:nicknames :q)
  (:shadow :lerp :/= := :+ :- :*)
  (:export :w :x :y :z :q! :0! :0p
           :unitp :identity :identity-p
           :make-quat
           :from-matrix3
           :from-axis-angle
           :from-look-at
           :from-axies
           :from-fixed-angles
           :magnitude :norm := :/=
           :copy :get-axis-angle :normalize :qconjugate
           :inverse :+ :- :* :*v :*s
           :dot :rotate :lerp :slerp :approx-slerp
           :to-mat3 :to-mat4))

(defpackage :rtg-math.projection
  (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths)
  (:shadow :lerp)
  (:export :perspective :orthographic))

(defpackage #:rtg-math
  (:use #:cl)
  (:import-from :rtg-math.quaternions :q!))
