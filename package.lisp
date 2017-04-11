;;;; package.lisp

(uiop:define-package #:%rtg-math
    (:use #:cl #:alexandria)
  (:export :case= :defn :defn-inline))

(uiop:define-package #:rtg-math.types
    (:use #:cl :%rtg-math)
  (:export :quaternion
           :mat3 :mat4
           :vec2 :vec3 :vec4
           :ivec2 :ivec3 :ivec4
           :uvec2 :uvec3 :uvec4
           :ivec2 :ivec3 :ivec4
           :uvec2 :uvec3 :uvec4
           :int8-vec2 :int8-vec3 :int8-vec4
           :uint8-vec2 :uint8-vec3 :uint8-vec4
           :line3 :ray3 :line-segment3
           :axis-aligned-box))

(uiop:define-package :rtg-math.base-maths
    (:use :cl :%rtg-math :rtg-math.types)
  (:export :clamp
           :clampf
           :+one-degree-in-radians+
           :+pi+
           :+inv-pi+
           :inv-sqrt
           :degrees
           :radians
           :degrees-f
           :radians-f))

(uiop:define-package :rtg-math.maths
    (:use :cl :%rtg-math :rtg-math.types)
  (:export :lerp :mix :stepv :clamp :smoothstep :pulse
           :spline))

(uiop:define-package :rtg-math.base-vectors
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

(uiop:define-package :rtg-math.base-matrices
    (:use :cl :%rtg-math :rtg-math.types)
  (:export :m!))

(uiop:define-package :rtg-math.vector2.non-consing
    (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v2-n)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:import-from :rtg-math.base-maths :inv-sqrt)
  (:shadow := :+ :- :* :/ :length :abs)
  (:export :set-components :+s :-s :+ :- :* :*s :/s :/ :negate :normalize))

(uiop:define-package :rtg-math.vector3.non-consing
    (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v3-n)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:import-from :rtg-math.base-maths :inv-sqrt)
  (:shadow := :+ :- :* :/ :length :abs)
  (:export :set-components :+s :-s :+ :- :* :*s :/s :/ :negate :normalize))

(uiop:define-package :rtg-math.vector4.non-consing
    (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v4-n)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:import-from :rtg-math.base-maths :inv-sqrt)
  (:shadow := :+ :- :* :/ :length :abs)
  (:export :set-components :+s :-s :+ :- :* :*s :/s :/ :negate :normalize))

(uiop:define-package :rtg-math.vector2
    (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v2)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:import-from :rtg-math.base-maths :inv-sqrt)
  (:shadow := :+ :- :* :/ :length :abs)
  (:export :make :copy-vec2
           :0p :unitp
           := :+ :- :* :/ :*s :/s :+s :-s
           :negate
           :length-squared :length :distance-squared :distance
           :abs
           :dot :absolute-dot :perp-dot
           :normalize
           :cross
           :face-foreward
           :lerp :bezier :spline
           :from-complex))

(uiop:define-package :rtg-math.vector3
    (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v3)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:import-from :rtg-math.base-maths :inv-sqrt)
  (:shadow := :+ :- :* :/ :length :abs)
  (:export :make :copy-vec3
           :0p :unitp
           := :+ :- :* :/ :*s :/s :+s :-s
           :negate
           :abs
           :length-squared :length :distance-squared :distance
           :dot :absolute-dot
           :normalize
           :cross
           :face-foreward
           :lerp :bezier :spline))

(uiop:define-package :rtg-math.vector4
    (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v4)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:import-from :rtg-math.base-maths :inv-sqrt)
  (:shadow := :+ :- :* :/ :length :abs)
  (:export :make :copy-vec4
           :0p :unitp
           := :+ :- :* :/ :*s :/s :+s :-s
           :negate
           :abs
           :length-squared :length :distance-squared :distance
           :dot :absolute-dot
           :normalize
           :cross
           :face-foreward
           :lerp :bezier :spline))

(uiop:define-package :rtg-math.vectors
    (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :v)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:shadow :+ := :/= :1+ :1- :- :* :/ :length)
  (:export :0p :unitp
           := :+ :/= :1+ :1- :- :* :/ :+s :-s
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

(uiop:define-package :%rtg-math.matrix3.common
    (:use :cl :%rtg-math :rtg-math.types)
  (:shadow :identity :trace :+ := :/= :- :* :/)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:export :melm))

(uiop:define-package :rtg-math.matrix3.non-consing
    (:use :cl :%rtg-math :rtg-math.types :%rtg-math.matrix3.common)
  (:nicknames :m3-n)
  (:shadow :identity :trace :+ := :/= :- :* :/)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:export :set-components :* :set-from-rows :set-from-columns :affine-inverse
           :transpose :adjoint :set-rotation-from-euler :set-from-scale
           :set-from-rotation-x :set-from-rotation-y :set-from-rotation-z
           :set-rotation-from-axis-angle :+ :- :negate :*v
           :mrow*vec3 :*s))

(uiop:define-package :rtg-math.matrix3
    (:use :cl :%rtg-math :rtg-math.types :%rtg-math.matrix3.common)
  (:nicknames :m3)
  (:shadow :identity :trace :+ := :/= :- :* :/)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:export :0p :identityp
           :make :0! :identity :copy-mat3
           := :+ :- :* :*s :*v :mrow*vec3 :negate
           :melm
           :from-rows :from-columns :get-rows :get-columns :get-row :get-column
           :transpose :adjoint :determinant :trace
           :rotation-from-euler :rotation-from-axis-angle
           :rotation-x :rotation-y :rotation-z
           :scale
           :affine-inverse
           :get-fixed-angles :get-axis-angle))

(uiop:define-package :%rtg-math.matrix4.common
    (:use :cl :%rtg-math :rtg-math.types)
  (:shadow :identity :trace :+ := :/= :- :* :/)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:export :melm :minor))

(uiop:define-package :rtg-math.matrix4.non-consing
    (:use :cl :%rtg-math :rtg-math.types :%rtg-math.matrix4.common)
  (:nicknames :m4-n)
  (:shadow :identity :trace :+ := :/= :- :* :/)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:export :set-components :* :set-from-mat3 :set-from-rows :set-from-rows-v3
           :set-from-columns :set-from-columns-v3 :adjoint :affine-inverse
           :transpose :set-from-translation :set-rotation-from-euler
           :set-from-scale :set-from-rotation-x :set-from-rotation-y
           :set-from-rotation-z :set-rotation-from-axis-angle :+ :- :negate
           :*s :*v :*v3 :mrow*vec4))

(uiop:define-package :rtg-math.matrix4
    (:use :cl :%rtg-math :rtg-math.types :%rtg-math.matrix4.common)
  (:nicknames :m4)
  (:shadow :identity :trace :+ := :/= :- :* :/)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:export :0p :identityp
           :make :0! :identity :from-mat3 :copy-mat4
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


(uiop:define-package :rtg-math.matrices
    (:use :cl :%rtg-math :rtg-math.types)
  (:nicknames :m)
  (:import-from :rtg-math.base-vectors :x :y :z :w :v!)
  (:shadow :0p :unitp :+ := :/= :1+ :1- :- :* :elt :trace)
  (:export :0p :unitp :+ := :/= :1+ :1- :- :*
           :identityp :elt :elm :get-rows :get-row
           :get-columns :get-column :determinant
           :inverse :affine-inverse :transpose :trace :negate
           :to-string))

(uiop:define-package :%rtg-math.quaternion.common
    (:use :cl :%rtg-math :rtg-math.types)
  (:shadow :lerp :/= := :+ :- :* :identity :conjugate)
  (:export :w :x :y :z :dot))

(uiop:define-package :rtg-math.quaternions.non-consing
    (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths
          :%rtg-math.quaternion.common)
  (:nicknames :q-n)
  (:shadow :lerp :/= := :+ :- :* :identity :conjugate)
  (:export :from-mat3 :from-axis-angle :from-fixed-angles
           :normalize :conjugate :+  :- :* :to-mat3 :to-mat4 :rotate))

(uiop:define-package :rtg-math.quaternions
    (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths
          :%rtg-math.quaternion.common)
  (:nicknames :q)
  (:shadow :lerp :/= := :+ :- :* :identity :conjugate)
  (:export :w :x :y :z :q! :0! :0p
           :unitp :identity :identity-p
           :from-mat3
           :from-axis-angle
           :from-look-at :to-look-at :to-look-at-vec4
           :from-axies
           :from-fixed-angles
           :magnitude :norm := :/=
           :copy :get-axis-angle :normalize :qconjugate :conjugate
           :inverse :+ :- :* :*v :*s
           :dot :rotate :lerp :slerp :approx-slerp
           :to-mat3 :to-mat4))

(uiop:define-package :rtg-math.polar
    (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-vectors)
  (:export :polar->cartesian :cartesian->polar))

(uiop:define-package :rtg-math.spherical
    (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-vectors)
  (:export :spherical->cartesian :cartesian->spherical))

(uiop:define-package :rtg-math.projection
    (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths
          :rtg-math.base-vectors)
  (:shadow :lerp)
  (:export :perspective-v2-radian-fov
           :perspective-radian-fov
           :perspective-v2
           :perspective
           :orthographic-v2
           :orthographic
           :blinn-newell-env-map
           :spherical-env-map))

(uiop:define-package :rtg-math.region.line3
    (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths
          :rtg-math.base-vectors)
  (:import-from :rtg-math.base-maths :sfzero-p)
  (:import-from :rtg-math.types
                :make-line3
                :line3-direction
                :line3-origin
                :line3-p
                :copy-line3)
  (:shadow := :/=)
  (:nicknames :line3)
  (:export :line3 :line3-p :origin :direction :make :transform-m3 :transform-q
           :copy-line3 :closest-point :closest-line-points
           :distance-to-point :distance-squared-to-point
           :distance-to-line3 :distance-squared-to-line3))

(uiop:define-package :rtg-math.region.ray3
    (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths
          :rtg-math.base-vectors)
  (:import-from :rtg-math.base-maths :sfzero-p)
  (:import-from :rtg-math.types
                :make-ray3
                :ray3-direction
                :ray3-origin
                :ray3-p
                :copy-ray3)
  (:shadow := :/=)
  (:nicknames :ray3)
  (:export :ray3 :ray3-p :origin :direction :make := :/=
           :copy-ray3 :transform-m3 :transform-q
           :distance-squared-to-ray3 :distance-to-ray3
           :distance-squared-to-line3 :distance-to-line3
           :distance-squared-to-point :distance-to-point
           :closest-ray-points :closest-point))

(uiop:define-package :rtg-math.region.line-segment3
    (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths
          :rtg-math.base-vectors)
  (:import-from :rtg-math.base-maths :sfzero-p)
  (:import-from :rtg-math.types
                :line-segment3-p
                :make-line-segment3
                :line-segment3-end-point0
                :line-segment3-offset
                :copy-line-segment3)
  (:shadow := :/= :length :abs)
  (:nicknames :line-seg3)
  (:export :end-point0 :end-point1 :direction :line-segment3-p
           :copy-line-segment3 :make :make-from-point-offset := :/=
           :transform-m3 :transform-q :length-squared :length
           :distance-squared-to-line-seg3 :distance-to-line-seg3
           :distance-squared-to-ray3 :distance-to-ray3
           :distance-squared-to-line3 :distance-to-line3
           :distance-squared-to-point :distance-to-point
           :closest-line-segment-points :closest-ray-points
           :closest-line-points :closest-point))

(uiop:define-package :%rtg-math.regions.axis-aligned-box.common
    (:use :cl :%rtg-math :rtg-math.types)
  (:import-from :rtg-math.types
                :axis-aligned-box
                :make-axis-aligned-box
                :axis-aligned-box-p
                :axis-aligned-box-maxima
                :axis-aligned-box-minima
                :copy-axis-aligned-box)
  (:shadow :lerp :/= :=)
  (:export :with-aab))

(uiop:define-package :rtg-math.region.axis-aligned-box.non-consing
    (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths
          :rtg-math.base-vectors)
  (:import-from :rtg-math.base-maths :sfzero-p)
  (:import-from :%rtg-math.regions.axis-aligned-box.common
                :with-aab)
  (:import-from :rtg-math.types
                :axis-aligned-box
                :make-axis-aligned-box
                :axis-aligned-box-p
                :axis-aligned-box-maxima
                :axis-aligned-box-minima
                :copy-axis-aligned-box)
  (:shadow := :/=)
  (:nicknames :aab-n)
  (:export :merge-point))

(uiop:define-package :rtg-math.region.axis-aligned-box
    (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths
          :rtg-math.base-vectors)
  (:import-from :rtg-math.base-maths :sfzero-p)
  (:import-from :%rtg-math.regions.axis-aligned-box.common
                :with-aab)
  (:import-from :rtg-math.types
                :axis-aligned-box
                :make-axis-aligned-box
                :axis-aligned-box-p
                :axis-aligned-box-maxima
                :axis-aligned-box-minima
                :copy-axis-aligned-box)
  (:shadow := :/=)
  (:nicknames :aab)
  (:export :axis-aligned-box :axis-aligned-box-p :axis-aligned-box
           :copy-axis-aligned-box :maxima :minima :make
           :from-aabs := :/= :from-points :add-point
           :intersects-p :intersects-with-line3-p :intersects-with-ray3-p
           :intersects-with-line-segment-p))

(uiop:define-package :rtg-math.region
    (:use :cl :%rtg-math :rtg-math.types :rtg-math.base-maths
          :rtg-math.base-vectors)
  (:nicknames :regions)
  (:export :intersects-p
           :distance-squared
           :distance
           :closest-points
           :closest-point))

(uiop:define-package #:rtg-math
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
