;;;; rtg-math.tests.asd

(asdf:defsystem #:rtg-math.tests
  :description "Testing rtg-math against other CL matrix libs"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:fiveam :rtg-math)
  :components ((:file "tests/package")
               (:file "tests/suite")))
