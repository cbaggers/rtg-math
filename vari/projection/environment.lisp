(in-package :rtg-math.projection)

;; these projections are used in environment mapping so
;; I stuck them in their own file

(vari:v-defun blinn-newell-env-map ((reflected-view-vec :vec3))
  (v! (acos (- (z reflected-view-vec)))
      (atan (y reflected-view-vec)
            (z reflected-view-vec))))


(vari:v-defun spherical-env-map ((normal-vec :vec3)
                                 (view-vec :vec3)
                                 (forward-vec :vec3)
                                 (up-vec :vec3)
                                 (horizon-right-vec :vec3))
  ;; This basis gives a matrix. The normal-vec and view-vec are transformed
  ;; by this matrix and then the reflected view vec is calculated
  (let* ((sphere-space-m4 (m4:from-rows-v3 horizon-right-vec
                                           up-vec
                                           forward-vec))
         (nv (m4:*v sphere-space-m4 normal-vec))
         (vv (m4:*v sphere-space-m4 view-vec))
         (reflected-vv (v4:- vv (v4:*s nv (* 2 (v4:dot nv vv)))))
         (rx (x reflected-vv))
         (ry (y reflected-vv))
         (rz (z reflected-vv))
         (1+rz (+ 1 rz))
         (2m (* 2 (sqrt (+ (* rx rx) (* ry ry) (* 1+rz 1+rz)))))
         (u (+ (/ rx 2m) 0.5f0))
         (v (+ (/ ry 2m) 0.5f0)))
    (v! u v)))
