;;;; rtg-math.asd

(asdf:defsystem #:rtg-math.tests
  :description "Tests for rtg-math"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :encoding :utf-8
  :serial t
  :depends-on (#:rtg-math #:fiveam #:alexandria)
  :components ((:file "tests/package")
               (:file "tests/suites")
               (:file "tests/matrices/matrix3/data")
               (:file "tests/matrices/matrix3/consing")))
