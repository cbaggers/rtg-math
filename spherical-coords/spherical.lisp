(in-package :rtg-math.spherical)

;; ρ - distance from origin (lengh of υ)
;; θ - angle between υ and x axis (rotating around z)
;; φ - angle between υ and z axis

(declaim (ftype (function (vec3) vec3)
                spherical->cartesian))
(defun spherical->cartesian (spherical-coord-v3)
  "Takes a spherical coord vector with the layout (θ φ ρ)
and converts it to a cartesian vector3.

Notes:
 θ - angle between υ and x axis (rotating around z)
 φ - angle between υ and z axis
 ρ - distance from origin (lengh of υ)"
  (declare (vec3 spherical-coord-v3))
  (let ((θ (x spherical-coord-v3))
        (φ (y spherical-coord-v3))
        (ρ (z spherical-coord-v3)))
    (v3:make (* ρ (sin φ) (cos θ))
             (* ρ (sin φ) (sin θ))
             (* ρ (cos φ)))))

(declaim (ftype (function (vec3) vec3)
                cartesian->spherical))
(defun cartesian->spherical (vec3)
  "Takes a cartesian vector3 and converts it to a
spherical coord vector with the layout (θ φ ρ).

Notes:
 θ - angle between υ and x axis (rotating around z)
 φ - angle between υ and z axis
 ρ - distance from origin (lengh of υ)"
  (declare (vec3 vec3))
  (let ((x (x vec3))
        (y (y vec3))
        (z (z vec3)))
    (let* ((x²+y² (+ (* x x) (* y y)))
           (ρ (sqrt (+ x²+y² (* z z))))
           (φ (atan (sqrt x²+y²) z))
           (θ (atan y x)))
      (v3:make θ φ ρ))))


(defun unit-spherical->unit-cartesian (unit-spherical-coord)
  (let ((θ (x unit-spherical-coord))
        (φ (y unit-spherical-coord)))
    (v3:make (* (sin φ) (cos θ))
             (* (sin φ) (sin θ))
             (cos φ))))
