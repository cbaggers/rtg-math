(in-package :rtg-math.polar)

;; given some vec υ from the origin
;;
;; r - length of υ
;; θ - angle between υ and y axis
;;
;; θ>0 means counter-clockwise rotation
;;

(declaim (ftype (function (vec2) vec2)
                polar->cartesian))
(defun polar->cartesian (angle-length-v2)
  (declare (vec2 angle-length-v2))
  (let ((θ (x angle-length-v2))
        (r (y angle-length-v2)))
    (v2:make (* r (cos θ))
             (* r (sin θ)))))

(declaim (ftype (function (vec2) vec2)
                cartesian->polar))
(defun cartesian->polar (vec2)
  (declare (vec2 vec2))
  (let ((x (x vec2))
        (y (y vec2)))
    (v2:make (atan y x)
             (sqrt (+ (* x x) (* y y))))))
