;;

(uiop:define-package #:rtg-math.vari
    (:use #:cl :varjo.api #:named-readtables #:varjo-conditions)
  (:import-from :rtg-math :v! :m! :s~ :radians :degrees)
  (:import-from :rtg-math.base-vectors :x :y :z :w
                :v!int :v!uint :v!bool :v!double
                :v!int :v!uint :v!bool :v!double
                :v2! :v2!double :v2!int :v2!uint
                :v3! :v3!double :v3!int :v3!uint
                :v4! :v4!double :v4!int :v4!uint)
  (:import-from :rtg-math.vectors
                :swizzle
                :dot
                :normalize
                :cross))
