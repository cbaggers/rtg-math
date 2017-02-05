;;;; rtg-math.asd

(asdf:defsystem #:rtg-math
  :description "A selection of the math routines most commonly needed for realtime graphics in lisp"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "deftypes")
               (:file "base-maths")
               (:file "maths")
               (:file "vectors/base-vectors")
               (:file "vectors/vector2")
               (:file "vectors/destructive/vector2")
               (:file "vectors/destructive/vector3")
               (:file "vectors/destructive/vector4")
               (:file "vectors/vector3")
               (:file "vectors/vector4")
               (:file "vectors/vectors")
               (:file "matrices/destructive/matrix3")
               (:file "matrices/destructive/matrix4")
               (:file "matrices/matrix3")
               (:file "matrices/matrix4")
               (:file "matrices/base-matrices")
               (:file "matrices/matrices")
               (:file "quaternions")
               (:file "projection/camera")
               (:file "polar-coords/polar")
               (:file "spherical-coords/spherical")))
