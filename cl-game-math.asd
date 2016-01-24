;;;; cl-game-math.asd

(asdf:defsystem #:cl-game-math
  :description "Describe cl-game-math here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
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
