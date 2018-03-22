(in-package :rtg-math.spherical)

;; ρ - distance from origin (lengh of υ)
;; θ - angle between υ and x axis (rotating around z)
;; φ - angle between υ and z axis

(vari:v-defun spherical->cartesian ((spherical-coord-v3 :vec3))
  (let ((θ (x spherical-coord-v3))
        (φ (y spherical-coord-v3))
        (ρ (z spherical-coord-v3)))
    (v3:make (* ρ (sin φ) (cos θ))
             (* ρ (sin φ) (sin θ))
             (* ρ (cos φ)))))

(vari:v-defun cartesian->spherical ((vec3 :vec3))
  (let ((x (x vec3))
        (y (y vec3))
        (z (z vec3)))
    (let* ((x²+y² (+ (* x x) (* y y)))
           (ρ (sqrt (+ x²+y² (* z z))))
           (φ (atan (sqrt x²+y²) z))
           (θ (atan y x)))
      (v3:make θ φ ρ))))


(vari:v-defun unit-spherical->unit-cartesian ((unit-spherical-coord :vec2))
  (let ((θ (x unit-spherical-coord))
        (φ (y unit-spherical-coord)))
    (v3:make (* (sin φ) (cos θ))
             (* (sin φ) (sin θ))
             (cos φ))))

(vari:v-defun unit-spherical->unit-cartesian ((unit-spherical-coord :vec3))
  (let ((θ (x unit-spherical-coord))
        (φ (y unit-spherical-coord)))
    (v3:make (* (sin φ) (cos θ))
             (* (sin φ) (sin θ))
             (cos φ))))
