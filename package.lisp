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
	   :+inv-pi+
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
           :v!byte :v!ubyte :v!int :v!uint :v!bool :v!double
	   :v!int8 :v!uint8
	   :v2! :v2!double :v2!int :v2!uint :v2!ubyte
	   :v2!byte :v2!uint8 :v2!int8 :v2!short :v2!ushort
	   :v3! :v3!double :v3!int :v3!uint :v3!ubyte
	   :v3!byte :v3!uint8 :v3!int8 :v3!short :v3!ushort
	   :v4! :v4!double :v4!int :v4!uint :v4!ubyte
	   :v4!byte :v4!uint8 :v4!int8 :v4!short :v4!ushort))

(defpackage :rtg-math.base-matrices
  (:use :cl :%rtg-math :rtg-math.types)
  (:export :m!))

(defpackage :rtg-math.vector2
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v2)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:import-from :rtg-math.base-maths :inv-sqrt)
  (:shadow := :+ :- :* :/ :length :abs)
  (:export :make
	   :0p :unitp
	   := :+ :- :* :/ :*s :/s
	   :negate
	   :length-squared :length :distance-squared :distance
       :abs
	   :dot :absolute-dot :perp-dot
	   :normalize
	   :cross
	   :face-foreward
	   :lerp :bezier :spline
	   :from-complex))

(defpackage :rtg-math.vector3
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v3)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:import-from :rtg-math.base-maths :inv-sqrt)
  (:shadow := :+ :- :* :/ :length :abs)
  (:export :make
	   :0p :unitp
	   := :+ :- :* :/ :*s :/s
       :negate
       :abs
	   :length-squared :length :distance-squared :distance
	   :dot :absolute-dot
	   :normalize
           :cross
	   :face-foreward
	   :lerp :bezier :spline))

(defpackage :rtg-math.vector4
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v4)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:import-from :rtg-math.base-maths :inv-sqrt)
  (:shadow := :+ :- :* :/ :length :abs)
  (:export :make
	   :0p :unitp
	   := :+ :- :* :/ :*s :/s :*v3
	   :negate
       :abs
	   :length-squared :length :distance-squared :distance
	   :dot :absolute-dot
	   :normalize
	   :cross
	   :face-foreward
	   :lerp :bezier :spline))

(defpackage :rtg-math.vectors
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:shadow :+ := :/= :1+ :1- :- :* :/ :length)
  (:export :0p :unitp
	   := :+ :/= :1+ :1- :- :* :/
	   :negate
	   :length :length-squared :distance :distance-squared
           :dot :absolute-dot :perp-dot
	   :normalize
	   :cross
           :swizzle :s~
	   :merge-into-vector
	   :face-foreward
	   :lerp :bezier :mix
	   :x :y :z :w))

(defpackage :rtg-math.matrix3
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :m3)
  (:shadow :identity :trace :+ := :/= :- :* :/)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:export :0p :identityp
	   :make :0! :identity
	   := :+ :- :* :*s :*v :mrow*vec3 :negate
	   :melm
	   :from-rows :from-columns :get-rows :get-columns :get-row :get-column
	   :transpose :adjoint :determinate :trace
	   :rotation-from-euler :rotation-from-axis-angle
	   :rotation-x :rotation-y :rotation-z
	   :scale
	   :affine-inverse
           :get-fixed-angles :get-axis-angle))

(defpackage :rtg-math.matrix4
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :m4)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:shadow :identity :trace :+ := :/= :- :* :/)
  (:export :0p :identityp
	   :make :0! :identity :from-mat3
	   := :+ :- :* :*s :*v :*v3 :negate :mrow*vec4
	   :melm :to-mat3
	   :from-rows :from-columns :get-rows :get-columns :get-row :get-column
	   :from-rows-v3 :from-columns-v3
	   :transpose :adjoint :determinant :minor :trace
           :2dclipspace-to-imagespace-matrix4
	   :translation
	   :rotation-from-mat3 :rotation-from-euler :rotation-from-axis-angle
	   :rotation-x :rotation-y :rotation-z
	   :scale
	   :inverse
	   :affine-inverse
	   :get-fixed-angles :get-axis-angle))


(defpackage :rtg-math.matrices
  (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :m)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:shadow :0p :unitp :+ := :/= :1+ :1- :- :* :elt :trace)
  (:export :0p :unitp :+ := :/= :1+ :1- :- :*
           :identityp :elt :elm :get-rows :get-row
           :get-columns :get-column :determinant
           :inverse :affine-inverse :transpose :trace :negate
           :to-string))

(defpackage :rtg-math.quaternions
  (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths)
  (:nicknames :q)
  (:shadow :lerp :/= := :+ :- :* :identity)
  (:export :w :x :y :z :q! :0! :0p
           :unitp :identity :identity-p
           :from-mat3
           :from-axis-angle
           :from-look-at :to-look-at :to-look-at-vec4
           :from-axies
           :from-fixed-angles
           :magnitude :norm := :/=
           :copy :get-axis-angle :normalize :qconjugate
           :inverse :+ :- :* :*v :*s
           :dot :rotate :lerp :slerp :approx-slerp
           :to-mat3 :to-mat4))

(defpackage :rtg-math.polar
  (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-vectors)
  (:export :polar->cartesian :cartesian->polar))

(defpackage :rtg-math.spherical
  (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-vectors)
  (:export :spherical->cartesian :cartesian->spherical))

(defpackage :rtg-math.projection
  (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths
	:rtg-math.base-vectors)
  (:shadow :lerp)
  (:export :perspective :orthographic :blinn-newell-env-map :spherical-env-map))

(defpackage #:rtg-math
  (:use #:cl)
  (:import-from :rtg-math.base-maths :radians :degrees)
  (:import-from :rtg-math.base-vectors :v! :v!byte :v!ubyte :v!int8 :v!uint8
		:v!int :x :y :z :w :v2! :v3! :v4!)
  (:import-from :rtg-math.base-matrices :m!)
  (:import-from :rtg-math.quaternions :q!)
  (:import-from :rtg-math.vectors :s~)
  (:export :radians :degrees
	   :v! :v!byte :v!ubyte :v!int8 :v!uint8 :v!int :v2! :v3! :v4!
	   :x :y :z :w
	   :m!
	   :q!
	   :s~))
