(in-package :rtg-math.projection)

;; these projections are used in environment mapping so
;; I stuck them in their own file

(defun blinn-newell-env-map (reflected-view-vec)
  "Blinn & Newell's Environment Mapping Method

pros:
- Uses the familar longitude/latitude system.
- easy to understand

cons:
- information not uniform
- texture seam
- sigularities at poles
"
  (v! (acos (- (z reflected-view-vec)))
      (atan (y reflected-view-vec)
            (z reflected-view-vec))))


(defun spherical-env-map (normal-vec view-vec forward-vec up-vec horizon-right-vec)
  "Initially by William and then refined by Miller & Hoffman
First env map technique supported in hardware.

Along with the normal-vec you supply the sphere basis where:

 f - is the vector (in some space) the camera was looking when the
     map captured
 u - the up vector
 h - vector horizontally to the right

pros:
- only 1 sigularity
- no seams

cons:
- only valid for single direction
"
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
