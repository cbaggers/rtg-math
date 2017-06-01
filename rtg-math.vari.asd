;;;; rtg-math.asd

(asdf:defsystem #:rtg-math.vari
  :description "Provides gpu equivalents of rtg-math's functionality"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:glsl-symbols :varjo :rtg-math)
  :components ((:file "vari/package")
               (:file "vari/functions")))
