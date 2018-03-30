(in-package :rtg-math.projection.non-consing)

;;------------------------------------------------------------

;; Checked against GLM's perspectiveRH
(defn perspective-radian-fov ((mat-to-mutate mat4)
                              (width single-float) (height single-float)
                              (near single-float) (far single-float)
                              (fov single-float))
    mat4

  ;; note: is right handed
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((aspect (/ width height))
         (tan-half-fov (tan (/ fov 2f0)))
         (result (m4:0!)))
    (m4-n:set-components
     (/ 1f0 (* aspect tan-half-fov))
     0f0
     0f0
     0f0
     0f0
     (/ 1f0 tan-half-fov)
     0f0
     0f0
     0f0
     0f0
     (- (/ (+ far near) (- far near)))
     -1f0
     0f0
     0f0
     (- (/ (* 2f0 far near) (- far near)))
     0f0
     mat-to-mutate)
    result))


(defn perspective ((mat-to-mutate mat4)
                   (width single-float) (height single-float)
                   (near single-float) (far single-float)
                   (fov-degrees single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (perspective-radian-fov mat-to-mutate
                          width height near far (radians fov-degrees)))

(defn perspective-v2 ((mat-to-mutate mat4)
                      (frame-size-v2 vec2)
                      (near single-float) (far single-float)
                      (fov-degrees single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (perspective-radian-fov mat-to-mutate
                          (x frame-size-v2) (y frame-size-v2) near far
                          (radians fov-degrees)))

(defn perspective-v2-radian-fov ((mat-to-mutate mat4)
                                 (frame-size-v2 vec2)
                                 (near single-float) (far single-float)
                                 (fov-degrees single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (perspective-radian-fov mat-to-mutate
                          (x frame-size-v2) (y frame-size-v2) near far
                          fov-degrees))

;;------------------------------------------------------------
