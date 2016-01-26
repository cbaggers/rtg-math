;;;; cl-game-math.asd

(asdf:defsystem #:cl-game-math
  :description "A selection of the math routines most commonly needed for making games in lisp"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "deftypes")
	       (:file "base-maths")
               (:file "maths")
               (:file "vectors/base-vectors")
               (:file "vectors/vector2")
               (:file "vectors/vector3")
               (:file "vectors/vector4")
               (:file "vectors/vectors")
               (:file "matrices/base-matrices")
               (:file "matrices/matrix3")
               (:file "matrices/matrix4")
               (:file "matrices/matrices")
               (:file "quaternions")
               (:file "projection")))
