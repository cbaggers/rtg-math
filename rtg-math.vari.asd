;;;; rtg-math.asd

(asdf:defsystem #:rtg-math.vari
  :description "Provides gpu equivalents of rtg-math's functionality"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:glsl-symbols :varjo :rtg-math)
  :components ((:file "vari/package")

               (:file "vari/base-maths")

               (:file "vari/vectors/base-vectors")
               (:file "vari/vectors/vectors")
               (:file "vari/vectors/vector2/consing")
               (:file "vari/vectors/vector3/consing")
               (:file "vari/vectors/vector4/consing")

               (:file "vari/matrices/base-matrices")
               (:file "vari/matrices/matrices")
               (:file "vari/matrices/matrix2/common")
               (:file "vari/matrices/matrix2/consing")
               (:file "vari/matrices/matrix3/common")
               (:file "vari/matrices/matrix3/consing")
               (:file "vari/matrices/matrix4/common")
               (:file "vari/matrices/matrix4/consing")

               (:file "vari/quaternions/common")
               (:file "vari/quaternions/consing")

               (:file "vari/polar-coords/polar")
               (:file "vari/spherical-coords/spherical")

               (:file "vari/projection/perspective/consing")
               (:file "vari/projection/orthographic/consing")
               (:file "vari/projection/environment")))
