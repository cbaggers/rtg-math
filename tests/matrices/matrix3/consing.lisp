(in-package :rtg-math.tests)

(def-test mat3-0 (:suite m3)
  (loop
     :for tm3 :in *some-mats-1-to-1*
     :for list :in *some-lists-1-to-1*
     :for m3 := (apply #'m3:make list)
     :do
     (is-true
      (loop :for tval :across tm3
         :for mval :across m3
         :always (= tval mval)))))
