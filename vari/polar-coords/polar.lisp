(in-package :rtg-math.polar)

;; given some vec υ from the origin
;;
;; r - length of υ
;; θ - angle between υ and y axis
;;
;; θ>0 means counter-clockwise rotation
;;

(vari:v-defun polar->cartesian ((angle-length-v2 :vec2))
  (v2:make (* r (x angle-length-v2))
           (* r (y angle-length-v2))))

(vari:v-defun cartesian->polar ((vec2 :vec2))
  (v2:make (atan (y vec2) (x vec2))
           (v2:length vec2)))
